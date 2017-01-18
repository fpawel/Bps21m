namespace Json  

open System
open System.Text
open System.Globalization
open FParsec

type Json = 
    | String of string
    | Number of decimal    
    | Bool of bool
    | Null of unit
    | Array of Json list
    | Object of Map<string, Json>
    
    static member obj = Map.ofList >> Object
    static member empty = Map.empty |> Object
                      
[<AutoOpen>]
module Helpers =
    let prop key = function    
        | Object x -> x |> Map.tryFind key
        | _ -> None

    let (|Prop|_|) key = prop key

    let (|Props|_|) keys json = 
        match json with        
        | Object x -> 
            let r = 
                keys 
                |> List.map( fun key -> Map.tryFind key x )
                |> List.choose id
                
            if r.Length = keys.Length then Some r else None            
        | _ -> None

    let (|StringInt|_|) = function    
        | String x -> 
            let b,x = Int32.TryParse x
            if b then Some x else None
        | _ -> None


    let (|StringInt64|_|) = function    
        | String x -> 
            let b,x = Int64.TryParse x
            if b then Some x else None
        | _ -> None

    let (|Int32|_|) = function    
        | Number x when Decimal.Round(x)=x -> x |> Decimal.ToInt32 |> Some
        | _ -> None

(* Functional

   Functional signatures for working with Json types, implying a monadic
   approach to working with Json where appropriate.

   Additionally includes common functions for combining and creating
   functions of type Json<'a> which may be used via operator based
   combinators or a computation expression (both provided later). *)

[<AutoOpen>]
module Functional =

    type Json<'a> =
        Json -> JsonResult<'a> * Json

    and JsonResult<'a> =
        | Value of 'a
        | Error of string

    (* Functions

       Common functions for combining Json<'a> functions in to new
       forms, and for creating new Json<'a> functions given suitable
       initial data. *)

    [<RequireQualifiedAccess>]
    module Json =

        let inline init (a: 'a) : Json<'a> = 
            fun json ->
                Value a, json

        let inline error (e: string) : Json<'a> =
            fun json ->
                Error e, json

        let inline internal ofResult result =
            fun json ->
                result, json

        let inline bind (m: Json<'a>) (f: 'a -> Json<'b>) : Json<'b> =
            fun json ->
                match m json with
                | Value a, json -> (f a) json
                | Error e, json -> Error e, json

        let inline apply (f: Json<'a -> 'b>) (m: Json<'a>) : Json<'b> =
            bind f (fun f' ->
                bind m (fun m' ->
                    init (f' m')))

        let inline map (f: 'a -> 'b) (m: Json<'a>) : Json<'b> =
            bind m (fun m' ->
                init (f m'))

        let inline map2 (f: 'a -> 'b -> 'c) (m1: Json<'a>) (m2: Json<'b>) : Json<'c> =
            apply (apply (init f) m1) m2

(* Escaping

   Functions for escaped string parsing and formatting, as a
   minimal encoding function (escaping only disallowed codepoints,
   but normalizing any input). *)
module private Escaping =

    let private digit i =
            (i >= 0x30 && i <= 0x39)

    let private hexdig i =
            (digit i)
         || (i >= 0x41 && i <= 0x46)
         || (i >= 0x61 && i <= 0x66)

    let private unescaped i =
            i >= 0x20 && i <= 0x21
         || i >= 0x23 && i <= 0x5b
         || i >= 0x5d && i <= 0x10ffff

    let private unescapedP =
        satisfy (int >> unescaped)

    let private hexdig4P =
        manyMinMaxSatisfy 4 4 (int >> hexdig)
        |>> fun s ->
            char (Int32.Parse (s, NumberStyles.HexNumber))

    let private escapedP =
            skipChar '\\'
        >>. choice [
                pchar '"'
                pchar '\\'
                pchar '/'
                skipChar 'b' >>% '\u0008'
                skipChar 'f' >>% '\u000c'
                skipChar 'n' >>% '\u000a'
                skipChar 'r' >>% '\u000d'
                skipChar 't' >>% '\u0009'
                skipChar 'u' >>. hexdig4P ]

    let private charP =
        choice [
            unescapedP
            escapedP ]

    let parse =
        many charP

    let escape (s: string) =
        let rec escape r =
            function | [] -> r
                     | h :: t when (unescaped (int h)) ->
                        escape (r @ [ h ]) t
                     | h :: t ->
                        let n =
                            match h with
                            | '"'  -> [ '\\'; '"' ]
                            | '\\' -> [ '\\'; '\\' ]
                            | '\b' -> [ '\\'; 'b' ]
                            | '\f' -> [ '\\'; 'f' ]
                            | '\n' -> [ '\\'; 'n' ]
                            | '\r' -> [ '\\'; 'r' ]
                            | '\t' -> [ '\\'; 't' ]
                            | x ->    [ '\\'; 'u' ] @ [ for c in (((int x).ToString ("X4")).ToCharArray ()) -> c ]

                        escape (r @ n) t

        new string (List.toArray (escape [] [ for c in (s.ToCharArray ()) -> c ]))

module private Parsing =

    (* Helpers

       Utlility functions for working with intermediate states of
       parsers, minimizing boilerplate and unpleasant code. *)

    let emp =
        function | Some x -> x
                 | _ -> ""

    (* Grammar

       Common grammatical elements forming parts of other parsers as
       as defined in RFC 1759. The elements are implemented slightly
       differently due to the design of parser combinators used, chiefly
       concerning whitespace, which is always implemented as trailing.

       Taken from RFC 7159, Section 2 Grammar
       See [http://tools.ietf.org/html/rfc7159#section-2] *)

    let wsp i =
            i = 0x20
         || i = 0x09
         || i = 0x0a
         || i = 0x0d

    let wspP =
        skipManySatisfy (int >> wsp)

    let charWspP c =
        skipChar c .>> wspP

    let beginArrayP =
        charWspP '['

    let beginObjectP =
        charWspP '{'

    let endArrayP =
        charWspP ']'

    let endObjectP =
        charWspP '}'

    let nameSeparatorP =
        charWspP ':'

    let valueSeparatorP =
        charWspP ','

    (* JSON

       As the JSON grammar is recursive in various forms, we create a
       reference parser which will be assigned later, allowing for recursive
       definition of parsing rules. *)

    let jsonP, jsonR =
        createParserForwardedToRef ()

    (* Values

       Taken from RFC 7159, Section 3 Values
       See [http://tools.ietf.org/html/rfc7159#section-3] *)

    let boolP =
            stringReturn "true" true
        <|> stringReturn "false" false
        .>> wspP

    let nullP =
        stringReturn "null" () .>> wspP

    (* Numbers

       The numbers parser is implemented by parsing the JSON number value
       in to a known representation valid as string under Double.Parse
       natively (invoked as the float conversion function on the eventual
       string).

       Taken from RFC 7159, Section 6 Numbers
       See [http://tools.ietf.org/html/rfc7159#section-6] *)

    let digit1to9 i =
            i >= 0x31 && i <= 0x39

    let digit i =
            digit1to9 i
         || i = 0x30

    let e i =
            i = 0x45 
         || i = 0x65

    let minusP =
        charReturn '-' "-"

    let intP =
        charReturn '0' "0" <|> (satisfy (int >> digit1to9) .>>. manySatisfy (int >> digit)
        |>> fun (h, t) -> string h + t)

    let fracP =
        skipChar '.' >>.  many1Satisfy (int >> digit)
        |>> fun i -> "." + i

    let expP =
            skipSatisfy (int >> e)
        >>. opt (charReturn '-' "-" <|> charReturn '+' "+")
        .>>. many1Satisfy (int >> digit)
        |>> function | Some s, d -> "e" + s + d
                     | _, d -> "e" + d

    let numberP =
        pipe4 (opt minusP) intP (opt fracP) (opt expP) (fun m i f e ->
            decimal (emp m + i + emp f + emp e)) .>> wspP

    (* Strings

       Taken from RFC 7159, Section 7 Strings
       See [http://tools.ietf.org/html/rfc7159#section-7] *)

    let quotationMarkP =
        skipChar '"'

    let stringP =
        between quotationMarkP quotationMarkP Escaping.parse .>> wspP
        |>> fun cs -> new string (List.toArray cs)

    (* Objects

       Taken from RFC 7159, Section 4 Objects
       See [http://tools.ietf.org/html/rfc7159#section-4] *)

    let memberP =
        stringP .>> nameSeparatorP .>>. jsonP

    let objectP =
        between beginObjectP endObjectP (sepBy memberP valueSeparatorP)
        |>> Map.ofList

    (* Arrays

       Taken from RFC 7159, Section 5 Arrays
       See [http://tools.ietf.org/html/rfc7159#section-5] *)

    let arrayP =
        between beginArrayP endArrayP (sepBy jsonP valueSeparatorP)

    (* JSON *)

    do jsonR :=
            wspP
        >>. choice [
                arrayP  |>> Array
                boolP   |>> Bool
                nullP   |>> Null
                numberP |>> Number
                objectP |>> Object
                stringP |>> String ]

(* Formatting *)
[<AutoOpen>]
module Formatting =

    (* Helpers *)

    type private Formatter<'a> =
        'a -> StringBuilder -> StringBuilder

    type private Separator =
        StringBuilder -> StringBuilder

    let private append (s: string) (b: StringBuilder) =
        b.Append s

    let private appendf (s: string) (v1: obj) (b: StringBuilder) =
        b.AppendFormat (s, v1)

    let private join<'a> (f: Formatter<'a>) (s: Separator) =
        let rec join values (b: StringBuilder) =
            match values with
            | [] -> b
            | [v] -> f v b
            | v :: vs -> (f v >> s >> join vs) b

        join

    (* Options

       Options for formatting, defined as functions for spacing and newline
       formatting appenders. Predefined formats are given as static members
       as a shorthand. *)

    type JsonFormattingOptions =
      { Spacing : StringBuilder -> StringBuilder
        NewLine : int -> StringBuilder -> StringBuilder }

      static member Compact =
        { Spacing = id
          NewLine = fun _ x -> x  }

      static member SingleLine =
        { Spacing = append " "
          NewLine = fun _ -> append " "  }

      static member Pretty =
        { Spacing = append " "
          NewLine = fun level -> append "\n" >> append (String.replicate level "  ")  }

    (* Formatters *)

    let notNull = function Json.Null () -> false | _ -> true
    

    let rec formatJson level options =
        function | Array x -> formatArray level options x
                 | Bool x -> formatBool x
                 | Number x -> formatNumber x
                 | Null _ -> formatNull ()
                 | Object x -> formatObject level options x
                 | String x -> formatString x

    and private formatArray level options =
        function | x ->
                       append "["
                    >> options.NewLine (level + 1)
                    >> join (formatJson (level + 1) options) (append "," >> options.NewLine (level + 1)) x
                    >> options.NewLine level
                    >> append "]"

    and private formatBool =
        function | true -> append "true"
                 | _ -> append "false"

    and private formatNumber =
        function | x -> append (string x)

    and private formatNull =
        function | () -> append "null" 

    and private formatObject level options =
        function | x -> 
                       append "{" 
                    >> options.NewLine (level + 1)
                    >> join (fun (k, v) -> appendf "\"{0}\":" (Escaping.escape k) >> options.Spacing >> formatJson (level + 1) options v)
                            (append "," >> options.NewLine (level + 1))
                            (Map.toList x)
                    >> options.NewLine level
                    >> append "}"

    and private formatString =
        function | x -> appendf "\"{0}\"" (Escaping.escape x)

    [<RequireQualifiedAccess>]
    module Json =

        let format json =
            StringBuilder ()
            |> formatJson 0 JsonFormattingOptions.Compact json
            |> string

        let formatWith options json =
            StringBuilder ()
            |> formatJson 0 options json
            |> string

        let stringify = formatWith JsonFormattingOptions.Pretty 


type Json with

    static member parse s =
        match run Parsing.jsonP s with
        | Success (json, _, _) -> Result.Ok json
        | Failure (e, _, _) -> Result.Err e    

    static member format json =
        StringBuilder ()
        |> Formatting.formatJson 0 JsonFormattingOptions.Compact json
        |> string

    static member formatWith options json =
        StringBuilder ()
        |> formatJson 0 options json
        |> string