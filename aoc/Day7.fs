module Day7

open System

module FuelConsumption =
    let private calculateFuel (crabs: list<int>) (targetPosition: int) : (int * int) =
        let cost =
            crabs
            |> List.map (fun crab -> Math.Abs(crab - targetPosition))
            |> List.sum

        (cost, targetPosition)

    let calculateFuelCosts crabs =
        [ List.min crabs .. List.max crabs ]
        |> List.map (fun position -> calculateFuel crabs position)
        |> List.sortBy (fun (cost, _) -> cost)

module Optimizer =
    let findOptimalPosition crabs =
        crabs
        |> FuelConsumption.calculateFuelCosts
        |> List.head

let solve () =
    let input = [ 16; 1; 2; 0; 4; 2; 7; 1; 2; 14 ]
    let (cost, position) = Optimizer.findOptimalPosition input

    printfn "Optimal position is: %i" position
    printfn "Total fuel cost is: %i units" cost
