module Day5

type Coordinate = (int * int)
type CoordinatePair = (Coordinate * Coordinate)
type Line = list<Coordinate>

module Parser =
    // Don't bother validating input type and trust that it is (int, int)
    let private parseCoordinate (str: string) : Coordinate =
        match str.Split "," with
        | [| x; y |] -> (int x, int y)
        | _ -> failwith "Could not parse coordinate"

    let private parseInputLine (str: string) : CoordinatePair =
        let coordinates =
            str.Split "->"
            |> Array.map (fun s -> s.Trim())
            |> Array.map parseCoordinate

        (coordinates[0], coordinates[1])

    let parseInput = List.map parseInputLine

module Lines =
    let private sortCoordinatePairAsc (a, b) =
        let sorted = [ a; b ] |> List.sortBy (fun (c1, c2) -> c1 + c2)
        (sorted[0], sorted[1])

    let private createHorizontalLine xs y = List.map (fun x -> (x, y)) xs

    let private createVerticalLine x ys = List.map (fun y -> (x, y)) ys

    let private createLine coordinatePair =
        let (a, b) = sortCoordinatePairAsc coordinatePair
        let x1, y1 = a
        let x2, y2 = b

        if x1 = x2 then
            createVerticalLine x1 [ y1..y2 ]
        elif y1 = y2 then
            createHorizontalLine [ x1..x2 ] y1
        else
            []

    // Filter out diagonal lines and only consider horizontal or vertical lines
    let parseCoordinatePairs (coordinates: list<CoordinatePair>) : list<Line> =
        coordinates
        |> List.map createLine
        |> List.filter (fun line -> not (List.isEmpty line))

module Count =
    let private flip (a, b) = (b, a)

    let private countCoordinates coordinates =
        coordinates |> Seq.countBy id |> Seq.map flip

    let countOverlappingCoordinates coordinates =
        coordinates
        |> List.toSeq
        |> countCoordinates
        |> Seq.sortByDescending (fun (count, _) -> count)

module Logic =
    // Coordinates where at least 2 lines overlap
    let private countDangeroursCoordinates coordinatePairs =
        coordinatePairs
        |> Lines.parseCoordinatePairs
        |> List.concat
        |> Count.countOverlappingCoordinates
        |> Seq.filter (fun (count, _) -> count > 1)

    let findDangeroursCoordinates coordinatePairs =
        coordinatePairs
        |> countDangeroursCoordinates
        |> Seq.map (fun (_, coord) -> coord)
        |> Seq.toList

let solve () =
    let input =
        [ "0,9 -> 5,9"
          "8,0 -> 0,8"
          "9,4 -> 3,4"
          "2,2 -> 2,1"
          "7,0 -> 7,4"
          "6,4 -> 2,0"
          "0,9 -> 2,9"
          "3,4 -> 1,4"
          "0,0 -> 8,8"
          "5,5 -> 8,2" ]

    let coordinatePairs = Parser.parseInput input
    let dangerousCoordinates = Logic.findDangeroursCoordinates coordinatePairs

    printfn "Number of points where at least 2 lines overlap: %i" dangerousCoordinates.Length
