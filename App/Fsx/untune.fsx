seq{
    for n = 0 to 100500 do
        yield if n<10 then Some n else None
}
|> Seq.takeWhile Option.isSome
|> Seq.choose id
|> Seq.toList
|> printfn "%A"