[<AutoOpen>]
module FsharpReflectionUtils

open System
open Microsoft.FSharp.Reflection 

type Type with
    static member isIEnumerable (tx:Type) =
        typeof<System.Collections.IEnumerable>.IsAssignableFrom tx 
        || typeof<System.Collections.Generic.IEnumerable<_>>.IsAssignableFrom tx

module FSharpValue =
    let inline unionCaseName<'a> (x:'a) = 
        try
            match FSharpValue.GetUnionFields(x, typeof<'a>) with
            | case, _ -> 
                case.Name
        with e ->
            failwithf "Utils unif %A" e

    let inline tryGetUnionCaseAttribute<'T,'a> (x:'a) = 
        let case,_ = FSharpValue.GetUnionFields(x, x.GetType() )
        case.GetCustomAttributes() |> 
        Seq.tryFind( fun e -> e.GetType()=typeof< 'T > ) |> 
        Option.map( fun atr -> atr :?> 'T )

module FSharpType =
    let inline unionCasesList<'T> =
        FSharpType.GetUnionCases typeof<'T> 
        |> Array.toList 
        |> List.map( fun case -> FSharpValue.MakeUnion(case,[||]) :?> 'T )

type Activator with
    static member makeGenericType (baseType : Type) (types : Type list) =  
        if (not baseType.IsGenericTypeDefinition) then
            invalidArg "baseType" "The base type specified was not a generic type definition." 
        baseType.MakeGenericType ( types |> List.toArray )

module List = 

    let inline makeListOfObjects itemType (items : obj list) = 
        let listType = 
            Activator.makeGenericType 
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

[<AutoOpen>]
module private Helpers1 = 
    let rec generateFsharpObject (t:Type) : obj  =  
        let fail() = failwithf "generateFsharpObject %A " t
        if t.IsValueType then Activator.CreateInstance(t) 
        elif t = typeof<string> then box ""
        elif Type.isIEnumerable t then 
            if t.IsArray then
                let valueType = t.GetElementType()
                let result = Array.CreateInstance( valueType, 0 )
                box result
            elif t.IsGenericType  then
                let deft = t.GetGenericTypeDefinition()
                if deft = typedefof<list<_>> then      
                    let valueType = t.GetGenericArguments().[0]          
                    List.makeListOfObjects valueType []
                elif deft = typedefof<Set<_>> then      
                    let valueType = t.GetGenericArguments().[0]
                    Activator.CreateInstance( t,   List.makeListOfObjects valueType [] )
                elif deft = typedefof<Map<_,_>> then                
                    let keyValuePairType = typedefof<System.Tuple<_,_>>.MakeGenericType( t.GetGenericArguments() )                
                    Activator.CreateInstance( t, List.makeListOfObjects keyValuePairType [] )
                else 
                    let valueType = t.GetGenericArguments().[0]                
                    Activator.CreateInstance( t, List.makeListOfObjects valueType [] )
            else fail()
        elif  FSharpType.IsTuple t then 
            FSharpValue.MakeTuple( FSharpType.GetTupleElements(t) |> Array.map generateFsharpObject, t)
        elif FSharpType.IsUnion t then
            let cases = FSharpType.GetUnionCases t        
            let tag = 0
            let case = cases |> Array.find( fun x -> x.Tag=tag)
            let values = case.GetFields() |> Array.map (fun px -> generateFsharpObject px.PropertyType ) 
            FSharpValue.MakeUnion(case, values)
        elif FSharpType.IsRecord t then
            let values = 
                FSharpType.GetRecordFields(t)
                |> Array.map ( fun px -> generateFsharpObject px.PropertyType)
            FSharpValue.MakeRecord( t, values)
        else fail()

type Activator with
    static member makeFsharpObject<'a>() = 
        generateFsharpObject (typeof<'a>) :?> 'a

