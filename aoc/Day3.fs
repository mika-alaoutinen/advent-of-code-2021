module Day3

open System

let takeHeads (xs: list<string>) =
    let head (s: string) = s |> Seq.head |> Char.ToString
    xs |> List.map head |> String.concat ""

let takeTails (xs: list<string>) = xs |> List.map (fun x -> x.[1..])

let rec transpose (xs: list<string>) : list<string> =
    if List.exists String.IsNullOrEmpty xs then
        []
    else
        takeHeads xs :: transpose (takeTails xs)

let flip (a, b) = (b, a)

// Return tuples (count, char) ordered from most frequent to least frequent
let frequencies input =
    input
    |> Seq.toList
    |> List.countBy id
    |> List.map flip
    |> List.sortByDescending (fun (count, _) -> count)

let mostCommon input =
    match input with
    | "" -> ""
    | str -> str |> frequencies |> List.head |> snd |> string

let reverseBit char =
    match char with
    | '0' -> '1'
    | '1' -> '0'
    | c -> c

let calculateGammaRate binaries =
    binaries
    |> List.map mostCommon
    |> String.concat ""

let calculateEpsilonRate binaries =
    binaries
    |> calculateGammaRate
    |> String.map reverseBit

let convertBinary binary = Convert.ToInt64(binary, 2)

let calculatePowerConsumption gamma epsilon =
    (convertBinary gamma) * (convertBinary epsilon)

let solve () =
    let input =
        [ "011110011100"
          "010001010101"
          "111111110000"
          "011101100011"
          "000111100100" ]

    let gamma = calculateGammaRate input
    let epsilon = calculateEpsilonRate input
    let power = calculatePowerConsumption gamma epsilon

    printfn "Gamma rate %s" gamma
    printfn "Epsilon rate %s" epsilon
    printfn "Sub power consumption %i" power
