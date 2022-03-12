module Day11

open System
open InputParser

[<StructuredFormatDisplay("(x: {X}, y: {Y})")>]
type Coordinate = { X: int; Y: int }

type Octopus =
    { Energy: int
      Flashed: bool
      FlashCount: int }

type Step =
    { Num: int
      Flashes: int
      Octopi: Octopus [,] }

let ROW_COUNT = 10
let COLUMN_COUNT = 10

module Array2D =
    let foldi (folder: int -> int -> 'S -> 'T -> 'S) (state: 'S) (array: 'T [,]) =
        let mutable state = state

        for x in 0 .. Array2D.length1 array - 1 do
            for y in 0 .. Array2D.length2 array - 1 do
                state <- folder x y state (array.[x, y])

        state

    let toNestedList array =
        [ let height = Array2D.length1 array

          for row in 0 .. height - 1 do
              yield array.[row, *] |> List.ofArray ]

module Print =
    let private intsToString (ints: list<int>) : string =
        ints |> List.map string |> String.concat ""

    let printGrid grid stepCount =
        let array =
            grid
            |> Array2D.map (fun octopus -> octopus.Energy)
            |> Array2D.toNestedList
            |> List.map intsToString
            |> String.concat "\n"

        printfn "Step %i" stepCount
        printfn "%s" array
        printfn ""

module Init =
    let initOctopi =
        Array2D.map (fun e ->
            { Energy = e
              Flashed = false
              FlashCount = 0 })

module Octopus =
    let increaseEnergy octopus =
        { Energy = octopus.Energy + 1
          Flashed = octopus.Flashed
          FlashCount = octopus.FlashCount }

    let flash octopus =
        { Energy = octopus.Energy
          Flashed = true
          FlashCount = octopus.FlashCount + 1 }

    let reset { FlashCount = count } =
        { Energy = 0
          Flashed = false
          FlashCount = count }

    let shouldBeFlashed octopus =
        octopus.Energy > 9 && not octopus.Flashed

module Coordinates =
    let private adjacent i max =
        [ Math.Max(0, (i - 1)) .. Math.Min(max, (i + 1)) ]

    let private findNeighbours coordinate =
        let xs = adjacent coordinate.X ROW_COUNT
        let ys = adjacent coordinate.Y COLUMN_COUNT

        // Remove target coordinate and only return list of neighbours
        Array2D.init (List.length ys) (List.length xs) (fun y x -> { X = x; Y = y })
        |> Seq.cast<Coordinate>
        |> Seq.filter ((<>) coordinate)
        |> Seq.toList

    let equals { X = x1; Y = y1 } { X = x2; Y = y2 } = x1 = x2 && y1 = y2

    let isNeighbour coord flashed =
        findNeighbours flashed |> List.contains coord

module Step =
    let increaseEnergy = Array2D.map Octopus.increaseEnergy

    let flashOctopus flashCoord grid =
        Array2D.mapi
            (fun y' x' octopus ->
                let coord = { X = x'; Y = y' }

                if Coordinates.equals coord flashCoord then
                    Octopus.flash octopus
                elif Coordinates.isNeighbour coord flashCoord then
                    Octopus.increaseEnergy octopus
                else
                    octopus)
            grid

    let flash grid =
        Array2D.foldi
            (fun y x array octopus ->
                let coord = { X = x; Y = y }

                if Octopus.shouldBeFlashed octopus then
                    flashOctopus coord grid
                else
                    array)
            grid
            grid

    let reset =
        Array2D.map (fun octopus ->
            if octopus.Flashed then
                Octopus.reset octopus
            else
                octopus)

module Simulate =
    let calculateFlashes grid =
        grid
        |> Seq.cast<Octopus>
        |> Seq.map (fun octopus -> octopus.FlashCount)
        |> Seq.sum

    let runStep grid step =
        let result =
            grid
            |> Step.increaseEnergy
            |> Step.flash
            |> Step.reset

        Print.printGrid result step
        result

    let run100Steps grid =
        seq { 1..100 }
        |> Seq.fold (fun result i -> runStep result i) grid

// This is too difficult
let solve () =
    let numbers =
        [ "5483143223"
          "2745854711"
          "5264556173"
          "6141336146"
          "6357385478"
          "4167524645"
          "2176841721"
          "6882881134"
          "4846848554"
          "5283751526" ]

    let flashes =
        numbers
        |> parse
        |> Init.initOctopi
        |> Simulate.run100Steps
        |> Simulate.calculateFlashes

    printfn "number of flashes: %i" flashes
