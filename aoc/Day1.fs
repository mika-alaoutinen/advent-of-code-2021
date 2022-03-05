module Day1

let safeHead xs =
    match xs with
    | [] -> None
    | nonEmpty -> Some(List.head nonEmpty)

let addIfIncreasing xs x =
    let head = defaultArg (safeHead xs) 0
    if x > head then x :: xs else xs

let filterIncreasing xs = List.fold addIfIncreasing [] xs

let increasedCount (xs: list<int>) : int = (filterIncreasing xs).Length + 1

let input =
    [ 199
      200
      208
      210
      200
      207
      240
      269
      260
      263 ]

let solve () = increasedCount input
