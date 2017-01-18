module Json.Serialization

open System
open System.Reflection
open Microsoft.FSharp.Reflection 

[<AutoOpen>]
module private Helpers1 = 

    let changeType (x:obj) (tx:Type) : obj =  Convert.ChangeType(x, tx) 
    
    let isStringMap (tx:Type) = 
        let x = tx.GetGenericArguments()
        tx.IsGenericType && tx.GetGenericTypeDefinition() = typedefof<Map<string,_>> &&                
        x.Length=2 &&
        x.[0]=typeof<string>

    let isMap (type':Type) = 
        type'.IsGenericType && type'.GetGenericTypeDefinition() = typedefof<Map<_,_>>
    let isOption (type':Type) = 
        type'.IsGenericType && type'.GetGenericTypeDefinition() = typedefof<option<_>>

    let isSeq (tx:Type) = 
        if not tx.IsGenericType then false else
        let genType = tx.GetGenericTypeDefinition()
        genType = typedefof<list<_>> || 
        genType = typedefof<Set<_>> 

    let isList (type':Type) = 
        type'.IsGenericType && type'.GetGenericTypeDefinition() = typedefof<list<_>>

    let isSet (type':Type) = 
        type'.IsGenericType && type'.GetGenericTypeDefinition() = typedefof<Set<_>>
        
    let makeGenericType (baseType : Type) (types : Type list) =  
        if (not baseType.IsGenericTypeDefinition) then
            invalidArg "baseType" "The base type specified was not a generic type definition." 
        baseType.MakeGenericType ( types |> List.toArray )

    let makeListOf itemType (items : obj list) = 
        let listType = 
            makeGenericType 
            <| typedefof<Microsoft.FSharp.Collections.List<_>> 
            <| [ itemType; ] 
        let add =  
            let cons =  listType.GetMethod ("Cons")            
            fun item list ->
                cons.Invoke (null, [| item; list; |])                 
        let list = 
            let empty = listType.GetProperty ("Empty") 
            empty.GetValue (null, [||]) 
        list
        |> List.foldBack add items

    let stringify = Json.formatWith JsonFormattingOptions.Pretty 

    let toJsonDefault : obj -> Json option = function 
        | :? string as x -> x |> decimal |> Number |> Some
        | :? decimal as x -> x |> Number |> Some
        | :? float as x -> x |> decimal |> Number |> Some
        | :? single as x -> x |> decimal |> Number |> Some
        | :? int8 as x -> x |> decimal |> Number |> Some
        | :? int16 as x -> x |> decimal |> Number |> Some
        | :? int as x -> x |> decimal |> Number |> Some
        | :? int64 as x -> x |> decimal |> Number |> Some
        | :? byte as x -> x |> decimal |> Number |> Some
        | :? uint16 as x -> x |> decimal |> Number |> Some
        | :? uint32 as x -> x |> decimal |> Number |> Some
        | :? uint64 as x -> x |> decimal |> Number |> Some
        | :? bool as x -> x |> Bool |> Some
        | :? DateTime as x -> Json.String (x.ToUniversalTime().ToString("o")) |> Some
        | :? DateTimeOffset as x -> Json.String (x.ToString("o")) |> Some
        | :? TimeSpan as x -> Json.String (x.ToString( @"hh\:mm\:ss" )) |> Some
        | _ -> None

    let (~&&) (type':Type) = type'.Name

type SerializationResult = Result<Json,string>    

module private Serialization =  
    let (==>) x y = x, y |> decimal |> Number
    let plain' (x : obj) = 
        let (~%%) = Ok >> Some    
        match x with
        | :? string as x -> %% Json.String x 
        | :? decimal as x -> %% Number x
        | :? float as x -> %% Number (decimal x)
        | :? single as x -> %% Number (decimal x)
        | :? int8 as x -> %% Number (decimal x)
        | :? int16 as x -> %% Number (decimal x)
        | :? int as x -> %% Number (decimal x)
        | :? int64 as x -> %% Number (decimal x)
        | :? byte as x -> %% Number (decimal x)
        | :? uint16 as x -> %% Number (decimal x)
        | :? uint32 as x -> %% Number (decimal x)
        | :? uint64 as x -> %% Number (decimal x)
        | :? bool as x -> %% Bool x       
        
        | :? DateTime as x ->  
            
            [   "Year" ==> x.Year
                "Month" ==> x.Month
                "Day" ==> x.Day
                "Hour" ==> x.Hour
                "Minute" ==> x.Minute
                "Second" ==> x.Second
                "Millisecond" ==> x.Millisecond ] 
            |> Json.obj |> Ok |> Some 
        | :? TimeSpan as x ->             
            [   "Days" ==> x.Days
                "Hours" ==> x.Hours
                "Minutes" ==> x.Minutes
                "Seconds" ==> x.Seconds
                "Milliseconds" ==> x.Milliseconds ] 
            |> Json.obj |> Ok |> Some
        | _ -> None

    let rec serializeUntyped (x : obj) : SerializationResult  = 
        if x=null then Ok (Null ()) else
        let type' = x.GetType()
        match plain' x with
        | Some json -> json
        | _ -> 
            match trySerializeCustomToJson x with
            | Some r -> r
            | _ -> 
                match trySerializeByKnownType x with 
                | Some r -> r
                | _ -> failwithf "%A : %A, not supported type" x (x.GetType())
    
    and trySerializeCustomToJson (x : obj) : SerializationResult option = 
        let type' = x.GetType()

        

        let toJsonUntyped = 
            type'.GetMethod
                (   "ToJsonUntyped", 
                    BindingFlags.Public ||| BindingFlags.Static,
                    null,                
                    CallingConventions.Any,
                    [| type' |],
                    null )
        
        if toJsonUntyped <> null then            
            toJsonUntyped.Invoke(null, [| box x |] ) :?> Json |> Ok |> Some 
        else None
    and returnSeq rs = 
        let oks, fails = List.partition Result.isOk rs
        match fails with
        | [] -> 
            oks 
            |> List.map Result.Unwrap.ok
            |> Ok 
        | _ -> 
            fails
            |> List.map Result.Unwrap.err 
            |> Seq.toStr ";\n" id
            |> sprintf "error serializing sequence to json: %s"
            |> Err
    and returnSeqWith f rs =
        rs
        |> returnSeq 
        |> Result.map f

    and seq' (x:obj) =        
        x :?> System.Collections.IEnumerable 
        |> Seq.cast 
        |> Seq.toList 
        |> List.map  serializeUntyped
        |> returnSeqWith Json.Array

    and stringMap' (x:obj) =
        x :?> System.Collections.IEnumerable |> Seq.cast |> Seq.toList |> List.map ( fun keyValueObject -> 
            let keyValueObjectType = keyValueObject.GetType()
            let keyProp = keyValueObjectType.GetProperty("Key")
            let kvpKey = string ( keyProp.GetValue(keyValueObject, null) )
            let kvpValue = keyValueObjectType.GetProperty("Value").GetValue(keyValueObject, null)
            match serializeUntyped kvpValue with
            | Err error -> Err error
            | Ok json -> 
                Ok (kvpKey, json) )
        |> returnSeqWith ( Map.ofList >> Json.Object )

    and map' (x:obj) = 
        let type' = x.GetType()
        let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( type'.GetGenericArguments() )
        x :?> System.Collections.IEnumerable |> Seq.cast |> Seq.toList |> List.map ( fun y -> 
            let ty = y.GetType()
            let kvpKey = ty.GetProperty("Key").GetValue(y, null)
            let kvpValue = ty.GetProperty("Value").GetValue(y, null)                    
            Activator.CreateInstance(keyValuePairType, [|kvpKey; kvpValue|]) 
            |> serializeUntyped )
        |> returnSeq 
        |> Result.map Json.Array        

    and array' (x:obj) = 
        [ for y in (x :?> Array) -> serializeUntyped y ] 
        |> returnSeq 
        |> Result.map Json.Array
        
    and tuple' (x:obj) = 
        FSharpValue.GetTupleFields x 
        |> Array.toList
        |> List.map serializeUntyped
        |> returnSeq 
        |> Result.map Json.Array
        
    and option' (x:obj) =  
        let type' = x.GetType()
        match type'.GetProperty("Value").GetValue(x, null) with
        | null -> Null () |> Ok 
        | value -> serializeUntyped value 
          
    and union' (x:obj) =  
        let type' = x.GetType()
        let case, vals =  FSharpValue.GetUnionFields(x, type') 
        if vals |> Array.isEmpty then
            Json.String case.Name  
            |> Ok
        else
            vals
            |> Array.toList
            |> List.map serializeUntyped
            |> returnSeq 
            |> function
                | Err error -> Err <| sprintf "error serializing discriminated union to json: %A : %A, %s" x (&& type') error  
                | Ok fields ->
                    [ case.Name, Json.Array fields ]
                    |> Map.ofList 
                    |> Json.Object
                    |> Ok

    and record' (x:obj) =
        let type' = x.GetType()
        let oks, fails = 
            FSharpType.GetRecordFields(type')
            |> Array.toList
            |> List.choose( fun y -> 
                if y.PropertyType = typeof<option<_>> then
                    match FSharpValue.GetRecordField(x,y) with
                    | null -> None
                    | value ->
                        let value = y.PropertyType.GetProperty("Value").GetValue(value, null)                            
                        Some( y.Name, serializeUntyped value)
                else
                    (y.Name, FSharpValue.GetRecordField(x,y) |> serializeUntyped )
                    |> Some  ) 
            |> List.partition( snd >> Result.isOk)
        if fails.IsEmpty then 
            oks 
            |> List.map ( fun (k,v) -> k, Result.Unwrap.ok v)
            |> Map.ofList
            |> Json.Object
            |> Ok
        else
            fails 
            |> List.map ( fun (k,v) -> sprintf "%s: %s" k (Result.Unwrap.err v) )
            |> Seq.toStr ";\n" id
            |> sprintf "error serializing record to json: %A : %A, %s" x (&& type')
            |> Err   

    and trySerializeByKnownType (x : obj) : SerializationResult option = 
        let type' = x.GetType()
        if isSeq type' then    
            Some <| seq' x
        elif isStringMap type' then
            Some <| stringMap' x
        elif isMap type' then
            Some <| map' x
        elif type'.IsArray  then                
             Some <| array' x
        elif  FSharpType.IsTuple type' then            
            Some <| tuple' x
        elif isOption type' then
            Some  <| option' x
        elif FSharpType.IsUnion type' then 
            Some  <| union' x
        elif FSharpType.IsRecord type' then             
            Some <| record' x        
        else None

module private Deserialization = 
    open System.Globalization

    let str' = function 
        | Json.String x -> x 
        | Number x -> sprintf "%g" x 
        | Null () -> ""
        | Array _ as x ->  stringify x  
        | Object _ as x -> stringify x  
        | Bool x -> sprintf "%b" x 

    let int' integerNumberType = function
        | Number x -> 
            changeType (Math.Round x) integerNumberType
            |> Ok
        | Bool x -> 
            changeType (if x then 1 else 0) integerNumberType
            |> Ok
        | String x -> 
            let b,x = Int64.TryParse x
            if b then changeType x integerNumberType |> Ok else 
            Err <| sprintf "error deserialize JSON, %A is not json integer number value %A" x (&& integerNumberType)
        | json -> 
            stringify json
            |> sprintf "error deserialize JSON, not a json integer number value %A : %s" (&& integerNumberType)
            |> Err

    let float' floatNumberType = function
        | Number x -> changeType x floatNumberType |> Ok
        | Bool x -> changeType (if x then 1 else 0) floatNumberType |> Ok
        | String x -> 
            let b,x = Decimal.TryParse x
            if b then changeType x floatNumberType |> Ok else 
            Err <| sprintf "%A is not json float number value %A" x (&& floatNumberType)
        | json -> 
            stringify json
            |> sprintf "not a json integer number value %A : %s" (&& floatNumberType)
            |> Err

    let bool' = function
        | Number x -> Ok ( x <> 0m )
        | Bool x -> Ok x
        | String x -> 
            match x.ToLower() with 
            | "true" -> Ok true
            | "false" -> Ok false
            | value -> sprintf "error deserialize JSON, not a boolean value %A" value |> Err             
        | json -> 
            stringify json
            |> sprintf "error deserialize JSON, not a boolean value %A : %s" json
            |> Err

    let integerTypes = 
        [ typeof<int8> ; typeof<int16>; typeof<int>; typeof<int64>;        
          typeof<byte>; typeof<uint16>; typeof<uint32> ; typeof<uint64> ]
    let floatTypes = 
        [ typeof<float> ; typeof<single>; typeof<decimal> ]

    let (|NumProp|_|) name json = 
        match json with
        | Prop name (Number value) -> Some value
        | _ -> None
    let datetime' = function
        |   NumProp "Year" year &  
            NumProp "Month" month &  
            NumProp "Day" day &  
            NumProp "Hour" hour &
            NumProp "Minute" minute & 
            NumProp "Second" second & 
            NumProp "Millisecond" millisecond ->                    
                DateTime( int year, int month, int day, int hour, int minute, int second, int millisecond) |> box |> Ok 
        | String s ->
            match DateTime.TryParse(s) with
            | true, x -> x |> box |> Ok 
            | _ -> sprintf "not date time %A" s |> Err 
        | json -> 
            stringify json
            |> sprintf "not date time value %s"
            |> Err

    let timespan' = function
        | NumProp "Days" days &  
          NumProp "Hours" hours &
          NumProp "Minutes" minutes & 
          NumProp "Seconds" seconds & 
          NumProp "Milliseconds" milliseconds ->
            TimeSpan(int days, int hours, int minutes, int seconds, int milliseconds) |> box |> Ok 
        | json -> 
            stringify json
            |> sprintf "not time span value %s"
            |> Err
        
    let plain' type' json  = 
        if type'= typeof<string> then 
            str' json |> box |> Ok |> Some 

        elif type'= typeof<DateTime> then
            datetime' json |> Some 
        elif type'= typeof<TimeSpan> then
            timespan' json |> Some 

        elif List.tryFind ( (=) type') integerTypes |> Option.isSome  then 
            int' type' json |> Some
        elif List.tryFind ( (=) type') floatTypes |> Option.isSome   then 
            float' type' json |> Some
        elif type'=typeof<bool> then 
            bool' json |> Result.map( box ) |> Some

        elif type'=typeof<unit> then 
            match json with
            | Null () -> box () |> Ok 
            | _ -> 
                stringify json
                |> sprintf "error deserialize JSON, not null %A : %s" (&& type')
                |> Err
            |> Some

        else None
            
    let returnList' valuetype (xs: _ list) = 
        xs         
        |> List.map ( snd >> Result.Unwrap.ok ) 
        |> makeListOf valuetype 
        |> Ok

    let returnOfList' type' valuetype (xs: _ list) = 
        Activator.CreateInstance ( type', returnList' valuetype xs)
        |> Ok

    let list' (type':Type) deserialize json  = 
        let valueType = type'.GetGenericArguments().[0]
        match json with 
        | Array jsons  -> 
            let oks,fails = 
                List.map ( deserialize valueType ) jsons 
                |> List.zip jsons
                |> List.partition ( snd >> Result.isOk )
            if fails.IsEmpty then  returnList' valueType oks else
                fails 
                |> List.map ( fun (json, xleft) -> 
                    sprintf "%s: %s" (stringify json) (Result.Unwrap.err xleft) )
                |> Seq.toStr ";\n\t" id
                |> sprintf "error deserializing json to list %A, %s" (&& type')
                |> Err   
        | _ -> Err <| sprintf "error deserializing json to list %A, %s" (&& type') (stringify json)

    let stringMap' (type':Type) deserialize json = 
        let valuetype = type'.GetGenericArguments().[1]
        let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( type'.GetGenericArguments() )
        match json with 
        | Object jsonProps  -> 
            let oks,fails = 
                jsonProps |> Map.toList |> List.map ( fun ( key,jsonValue) -> 
                    ( key,jsonValue), deserialize valuetype jsonValue )
                |> List.partition ( snd >> Result.isOk )
            if fails.IsEmpty then   
                let kvs = 
                    oks 
                    |> List.map ( fun ((key,_),xright) ->                     
                        Activator.CreateInstance( keyValuePairType, [| box key; Result.Unwrap.ok xright|] ) )  
                    |> makeListOf keyValuePairType                    
                Activator.CreateInstance ( type', kvs )
                |> Ok
            else
                fails 
                |> List.map ( fun ((k,json), xleft) -> 
                    sprintf "key %s, %s, %s" k (Result.Unwrap.err xleft) (stringify json) )
                |> Seq.toStr ";\n\t" id
                |> sprintf "error deserializing json to string map %A, %s" (&& type')
                |> Err   
         | _ -> Err <| sprintf "error deserializing json to string map %A, %s" (&& type') (stringify json)

    let map' (type':Type) deserialize json = 
        let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( type'.GetGenericArguments() )
        match json with 
        | Array jsons  -> 
            let oks,fails = 
                jsons 
                |> List.map ( fun xjson -> 
                    (keyValuePairType, xjson), deserialize keyValuePairType xjson ) 
               |> List.partition ( snd >> Result.isOk )

            if fails.IsEmpty then 
                let kvs =
                    oks         
                    |> List.map ( snd >> Result.Unwrap.ok ) 
                    |> makeListOf keyValuePairType 
                Activator.CreateInstance ( type', kvs )
                |> Ok                
            else
                fails 
                |> List.map ( fun ((k,json), xleft) -> 
                    sprintf "key type is %A, %s, %s" k (Result.Unwrap.err xleft) (stringify json)  )
                |> Seq.toStr ";\n\t" id
                |> sprintf "error deserializing json to map %A, %s" (&& type')
                |> Err 
        | _ -> 
            Err <| sprintf "error deserializing json to map %A, %s" (&& type') (stringify json)

    let set' (type':Type) deserialize json = 
        let valueType = type'.GetGenericArguments().[0]
        match json with 
        | Array jsons  -> 
            let oks,fails = 
                jsons 
                |> List.map ( fun xjson -> xjson, deserialize valueType xjson ) 
                |> List.partition ( snd >> Result.isOk )

            if fails.IsEmpty then 
                let kvs =
                    oks         
                    |> List.map ( snd >> Result.Unwrap.ok )                     
                Activator.CreateInstance ( type', makeListOf valueType  kvs )
                |> Ok
            else
                fails 
                |> List.map ( fun (json, xleft) -> 
                    sprintf "%s, %s" (Result.Unwrap.err xleft) (stringify json)  )
                |> Seq.toStr ";\n\t" id
                |> sprintf "error deserializing json to set %A, %s" (&& type')
                |> Err 
        | _ -> 
            Err <| sprintf "error deserializing json to set %A, %s" (&& type') (stringify json)

    let array' (type':Type) deserialize json = 
        let valueType = type'.GetElementType()       
        match json with 
        | Array jsons  -> 
            let result = Array.CreateInstance( valueType, jsons.Length )
            let oks,fails = 
                jsons 
                |> List.mapi ( fun n xjson -> (xjson,n), deserialize valueType xjson ) 
                |> List.partition ( snd >> Result.isOk )
            if fails.IsEmpty then
                oks                
                |> List.iter( fun ((_,n),xright) ->  
                    result.SetValue( Result.Unwrap.ok xright, n ) )
                box result |> Ok
            else
                fails 
                |> List.map ( fun ((json,n), xleft) -> 
                    sprintf "[%d] - %s, %s" n (Result.Unwrap.err xleft) (stringify json)  )
                |> Seq.toStr ";\n" id
                |> sprintf "error deserializing json to array %A, %s" (&& type')
                |> Err 
        | _ -> 
            Err <| sprintf "error deserializing json to array %A, %s" (&& type') (stringify json)

    let tuple' (type':Type) deserialize json = 
        let tes = FSharpType.GetTupleElements(type')
        match json with 
        | Array src when src.Length >= tes.Length ->
            let oks,fails = 
                src 
                |> Seq.take tes.Length 
                |> Seq.toArray |> Array.zip tes 
                |> Array.map( fun (te,jsonx) -> (te,jsonx), deserialize te jsonx )
                |> Array.partition (snd >> Result.isOk)

            if Array.isEmpty fails then 
                FSharpValue.MakeTuple( oks |> Array.map ( snd >> Result.Unwrap.ok ), type' )
                |> Ok
            else 
                fails |> Array.map ( fun ((t,json), xleft) -> 
                    sprintf "%A : %s, %s" t (Result.Unwrap.err xleft) (stringify json)  )
                |> Seq.toStr ";\n\t" id
                |> sprintf "error deserializing json to tuple %A, %s" (&& type')
                |> Err 
        | _ -> 
            Err <| sprintf "error deserializing json to tuple %A, %s" (&& type') (stringify json)

    let option' (type':Type) deserialize json = 
        let cases =  FSharpType.GetUnionCases type' |> Array.toList 
        match json with
        | Null () -> FSharpValue.MakeUnion(cases.[0], [||]) |> Ok
        | _ -> 
            deserialize (type'.GetGenericArguments().[0]) json
            |> Result.map( fun x -> 
                FSharpValue.MakeUnion(cases.[1], [|x|]) )

    let (|GetCase|_|) cases x = 
        match cases |> List.tryFind( fun (case : UnionCaseInfo) -> case.Name=x) with
        | Some case -> Some(case, case.GetFields() |> Array.toList )
        | _ -> None

    let (|MapToList|) = Map.toList

    let union' (type':Type) deserialize json = 
        let cases =  FSharpType.GetUnionCases type' |> Array.toList
        match json with
        | String ( GetCase cases (case,fields) ) when fields.IsEmpty  ->
            FSharpValue.MakeUnion(case, [||]) |> Ok
        | Object ( MapToList [ GetCase cases (case,fields), Array jsons] ) when fields.Length = jsons.Length ->
            let oks,fails = 
                jsons |> List.zip fields
                |> List.map( fun ((field,xjson) as k) -> 
                    k, deserialize field.PropertyType xjson )
                |> List.partition (snd >> Result.isOk)
            if fails.IsEmpty then
                let xs = oks |> List.map( snd >> Result.Unwrap.ok ) |> List.toArray
                FSharpValue.MakeUnion(case, xs) 
                |> Ok
            else                
                fails |> List.map ( fun ((p,json), xerr) -> 
                    sprintf 
                        "union case %A : %A - %s, %s" 
                        p.Name p.PropertyType 
                        (Result.Unwrap.err xerr) 
                        (stringify json)  )
                |> Seq.toStr ";\n\t" id
                |> sprintf "error deserializing json to union %A, %s" (&& type')
                |> Err 
        | _ -> 
            Err <| sprintf "error deserializing json to union %A, %s" (&& type') (stringify json)

    let recordFieldNone' (type':Type) =
        let gtype' = type'.GetGenericTypeDefinition()
        if gtype' = typedefof<option<_>> then
            let case =  (FSharpType.GetUnionCases type').[0]
            let value = FSharpValue.MakeUnion(case, [||])
            Some value 
        elif gtype' = typedefof<Map<_,_>> then                
            let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( type'.GetGenericArguments() )
            let value = Activator.CreateInstance( type',  makeListOf keyValuePairType [] )
            Some value
        elif gtype' = typedefof<list<_>> then                
            let valueType = type'.GetGenericArguments().[0]
            let value = makeListOf valueType []
            Some value
        else None 

    let recordField' jprops deserialize (prop : PropertyInfo) = 
        let propType = prop.PropertyType
        match Map.tryFind prop.Name jprops with
        | None when propType.IsGenericType ->
            match recordFieldNone' propType with
            | Some x -> Ok x
            | _ -> sprintf "missing value of property %A" prop.Name |> Err
        | None -> sprintf "missing value of property %A" prop.Name |> Err
        | Some xjson -> 
            deserialize propType xjson 
            |> Result.mapErr( fun error -> sprintf "%s - %s, %s" prop.Name error (stringify xjson)  )

    let record' (type':Type) deserialize json = 
        match json with
        | Object jprops ->
            let oks,fails = 
                FSharpType.GetRecordFields(type') |> Array.map (recordField' jprops deserialize)
                |> Array.partition Result.isOk
            if Array.isEmpty fails then 
                FSharpValue.MakeRecord( type', oks |> Array.map Result.Unwrap.ok )
                |> Ok
            else 
                fails 
                |> Array.map Result.Unwrap.err
                |> Seq.toStr ";\n\t" id
                |> sprintf "error deserializing json to record %A, %s" (&& type')
                |> Err 
        | _ -> 
            Err <| sprintf "error deserializing json to record %A, %s" (&& type') (stringify json)

    let deserializerFor' (t:Type) = 
        if isList t then Some list' 
        elif isStringMap t then Some stringMap' 
        elif isMap t then Some map' 
        elif isSet t then Some set' 
        elif t.IsArray then Some array'
        elif FSharpType.IsTuple t then Some tuple'
        elif isOption t then Some option'
        elif FSharpType.IsUnion t then Some union'
        elif FSharpType.IsRecord t then Some record'
        else None        

    let rec deserializeUntyped (type':Type) (json : Json) : Result<obj,string>  =
        
        match tryDeserializeCustomFromJson type' json with
        | Some x -> x
        | _ ->
            match plain' type' json with
            | Some x -> x
            | _ ->             
                match deserializerFor' type' with
                | None -> failwithf "can't find json deserializer for %A" (&& type')
                | Some deserialize' ->
                    deserialize' type' deserializeUntyped json

    and tryDeserializeCustomFromJson(type':Type) (json:Json) =         
        let fromJson = 
            type'.GetMethod
                (   "FromJsonUntyped", 
                    BindingFlags.Public ||| BindingFlags.Static,
                    null,                
                    CallingConventions.Any,
                    [| typeof<Json> |],
                    null )

        let rtype' = typeof<Result<obj,string>>
        
        if fromJson <> null then 
            if fromJson.ReturnType <> rtype' then
                failwithf "return type of %A.FromJsonUntyped must be %A, but is %A" 
                    type'.Name rtype'.Name fromJson.ReturnType.Name
            try
                fromJson.Invoke(null, [|json|] ) :?> Result<obj,string>                                
            with e ->
                Err <| sprintf "error calling %A.FromJsonUntyped on %A : %A" type'.Name (stringify json) e.Message
            |> Some
        else None

let serializeUntyped = Serialization.serializeUntyped

let serialize<'a> (x:'a) = 
    match serializeUntyped x with
    | Err e -> failwithf "error serialize JSON %A : %A - %s" x (&& x.GetType()) e
    | Ok x -> x

let deserializeUntyped = Deserialization.deserializeUntyped

let deserialize<'T> json = 
    deserializeUntyped typeof<'T> json 
    |> Result.map( fun x -> x :?> 'T)

let parse<'T> = 
    Json.parse 
    >> Result.mapErr ( sprintf "can't parse %A - %s" (&& typeof<'T>)  ) 
    >> Result.bind( 
        deserialize<'T> 
        >> Result.mapErr ( sprintf "can't deserialize %A - %s" (&& typeof<'T>)  ) )

let stringify<'a> = serialize<'a> >> stringify
