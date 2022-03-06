module Day9

type Cell = { X: int; Y: int; Value: int }

module Array2DFn =
    let createCells = Array2D.mapi (fun y x value -> { X = x; Y = y; Value = value })

    let filter predicate source =
        source
        |> Seq.cast<'T>
        |> Seq.filter predicate
        |> Seq.toList

module ArrayBounds =
    let private existsLeftColumn x matrix = Array2D.length2 matrix > 1 && x > 0
    let private existsRightColumn x matrix = x < Array2D.length2 matrix - 1
    let private existsUpRow y matrix = Array2D.length1 matrix > 1 && y > 0
    let private existsDownRow y matrix = y < Array2D.length1 matrix - 1

    let left x y matrix =
        if existsLeftColumn x matrix then
            Some matrix[y, x - 1]
        else
            None

    let right x y matrix =
        if existsRightColumn x matrix then
            Some matrix[y, x + 1]
        else
            None

    let up x y matrix =
        if existsUpRow y matrix then
            Some matrix[y - 1, x]
        else
            None

    let down x y matrix =
        if existsDownRow y matrix then
            Some matrix[y + 1, x]
        else
            None

module Parser =
    let inline private charToInt c = int c - int '0'
    let private parseLine line = line |> Seq.map charToInt |> Seq.toList

    let parseInput input = input |> List.map parseLine |> array2D

module Heatmap =
    let private checkDirection directionFn ({ X = x; Y = y; Value = value }) heatmap =
        heatmap
        |> directionFn x y
        |> Option.map (fun v -> v > value)
        |> Option.defaultValue true

    let private isLowPoint cell heatmap =
        checkDirection ArrayBounds.up cell heatmap
        && checkDirection ArrayBounds.right cell heatmap
        && checkDirection ArrayBounds.down cell heatmap
        && checkDirection ArrayBounds.left cell heatmap

    let findLowPoints heatmap =
        heatmap
        |> Array2DFn.createCells
        |> Array2DFn.filter (fun cell -> isLowPoint cell heatmap)

module Risk =
    let calculate lowPoints =
        lowPoints
        |> List.map (fun { Value = v } -> v + 1)
        |> List.sum


let solve () =
    let input =
        [ "2199943210"
          "3987894921"
          "9856789892"
          "8767896789"
          "9899965678" ]

    let lowPoints =
        input
        |> Parser.parseInput
        |> Heatmap.findLowPoints

    let totalRisk = Risk.calculate lowPoints

    printfn "Low points:"
    List.iter (printfn "%A") lowPoints
    printfn "Sum of risk levels: %i" totalRisk
