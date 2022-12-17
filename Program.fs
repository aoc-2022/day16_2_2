open day16_2_2.Input
open day16_2_2.Solver

let state = readValveMap "/tmp/aoc/input.16.t"

state |> printfn "%A"

solution state 26