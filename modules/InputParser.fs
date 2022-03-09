module InputParser

let inline private charToInt c = int c - int '0'

let private parseLine line = line |> Seq.map charToInt |> Seq.toList

let parse (input: list<string>) : int [,] = input |> List.map parseLine |> array2D
