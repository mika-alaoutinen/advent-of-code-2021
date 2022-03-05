module Day2

type Direction =
    | Up
    | Down
    | Forward

type Command = { Direction: Direction; Distance: int }
type Position = { Horizontal: int; Depth: int }

let move currentPos { Direction = dir; Distance = dist } =
    let changeDepth depthChange position =
        let depth = position.Depth + depthChange

        // Don't go above water
        if depth < 0 then
            { Horizontal = position.Horizontal
              Depth = 0 }
        else
            { Horizontal = position.Horizontal
              Depth = depth }

    let goForward distance position =
        { Horizontal = position.Horizontal + distance
          Depth = position.Depth }

    match dir with
    | Up -> changeDepth (dist * -1) currentPos
    | Down -> changeDepth dist currentPos
    | Forward -> goForward dist currentPos

let calculatePosition (commands: list<Command>) (initialPos: Position) : Position = List.fold move initialPos commands

let commands =
    [ { Direction = Forward; Distance = 5 }
      { Direction = Down; Distance = 5 }
      { Direction = Forward; Distance = 8 }
      { Direction = Up; Distance = 3 }
      { Direction = Down; Distance = 8 }
      { Direction = Forward; Distance = 2 } ]

let origo = { Horizontal = 0; Depth = 0 }

let solve () =
    let { Horizontal = h; Depth = d } = calculatePosition commands origo
    printfn "Final Position: horizontal %i, depth %i" h d
    printfn "Horizontal position * depth = %i" (h * d)
