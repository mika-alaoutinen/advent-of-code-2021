module Day6

module Lanternfish =
    // Return list of either 1 or 2 fish
    let private updateFishState fish =
        match fish with
        | 0 -> [ 6; 8 ]
        | i -> [ i - 1 ]

    let private passDay fish =
        fish |> List.map updateFishState |> List.concat

    let rec simulate fish days =
        match days with
        | 0 -> fish
        | _ -> simulate (passDay fish) (days - 1)

let solve () =
    let fish = Lanternfish.simulate [ 3; 4; 3; 1; 2 ] 80
    printfn "# of fish after 80 days: %i" fish.Length
