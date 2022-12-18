open day16_2_2.Input
open day16_2_2.Solver
open day16_2_2.SeverGraph

let state = readValveMap "/tmp/aoc/input.16"

state |> printfn "%A"

// optimize state 26 

solution state 26