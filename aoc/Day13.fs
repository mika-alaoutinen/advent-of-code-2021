module Day13

open System

type Axis =
    | X
    | Y

type FoldLine = { Axis: Axis; Line: int }

module Input =
    let private stringToInt s =
        try
            s |> int |> Some
        with
        | :? FormatException -> None

    let private parsePoint (point: string) =
        match point.Split(",") with
        | [| a; b |] -> Option.map2 (fun x y -> (x, y)) (stringToInt a) (stringToInt b)
        | _ -> None

    let parsePoints points =
        points |> List.map parsePoint |> List.choose id

module Board =
    let private dimension maxComparator points =
        points
        |> List.maxBy maxComparator
        |> maxComparator
        |> ((+) 1)

    let private findDimensions points =
        let width = dimension fst points
        let height = dimension snd points
        (width, height)

    let private cellFill point points =
        if List.contains point points then
            "#"
        else
            "."

    let create2dArray points =
        let (width, height) = findDimensions points

        [| 0..height |]
        |> Array.map (fun y ->
            [| 0..width |]
            |> Array.map (fun x -> cellFill (x, y) points))

    let private boardAsString board =
        board
        |> Array.map (String.concat " ")
        |> String.concat "\n"

    let toString points =
        points |> create2dArray |> boardAsString

module Flip =
    let flipX width (x, y) = (2 * width - x, y)
    let flipY height (x, y) = (x, 2 * height - y)

module Points =
    let private findYPoints foldLine comparator =
        List.filter (fun (_, y) -> comparator y foldLine)

    let findPointsAboveFold foldLine = findYPoints foldLine (<)
    let findPointsBelowFold foldLine = findYPoints foldLine (>)

    let private findXPoints foldLine comparator =
        List.filter (fun (x, _) -> comparator x foldLine)

    let findPointsLeftOfFold foldLine = findXPoints foldLine (<)
    let findPointsRightOfFold foldLine = findXPoints foldLine (>)

module Paper =
    let foldLeft foldLine points =
        let pointsLeftOfFold = Points.findPointsLeftOfFold foldLine points

        let flippedPoints =
            points
            |> Points.findPointsRightOfFold foldLine
            |> List.map (Flip.flipX foldLine)

        List.distinct (pointsLeftOfFold @ flippedPoints)

    let foldUp foldLine points =
        let pointsAboveFold = Points.findPointsAboveFold foldLine points

        let flippedPoints =
            points
            |> Points.findPointsBelowFold foldLine
            |> List.map (Flip.flipY foldLine)

        List.distinct (pointsAboveFold @ flippedPoints)

module Folds =
    let private printBoard points =
        printfn "Points visible after fold: %i" (List.length points)
        printfn "%s" (Board.toString points)
        printf "\n"

    let private foldPaper points { Axis = axis; Line = line } =
        let result =
            match axis with
            | X -> Paper.foldLeft line points
            | Y -> Paper.foldUp line points

        printBoard result
        result

    let doFolds foldLines initialPoints =
        printfn "Initial board"
        printfn "%s" (Board.toString initialPoints)
        printfn ""
        List.fold foldPaper initialPoints foldLines

let solve () =
    let input =
        [ "6,10"
          "0,14"
          "9,10"
          "0,3"
          "10,4"
          "4,11"
          "6,0"
          "6,12"
          "4,1"
          "0,13"
          "10,12"
          "3,4"
          "3,0"
          "8,4"
          "1,10"
          "2,14"
          "8,10"
          "9,0" ]

    let foldLines =
        [ { Axis = Y; Line = 7 }
          { Axis = X; Line = 5 } ]

    input
    |> Input.parsePoints
    |> Folds.doFolds foldLines
