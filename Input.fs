module day16_2_2.Input

open System.IO
open day16_2_2.Valve
open day16_2_2.State

let readValveMap (fileName:string) : State = 
    let input = File.ReadAllLines fileName |> Array.toList

    let parse (s: string) =
        let s = s.Split [| ' '; ';'; ','; '=' |]
        let name = s[1]
        let value = s[5] |> int
        let next = s |> Array.skip 11 |> Array.toList |> List.filter (fun s -> s.Length = 2)
        let next = next |> List.map (fun s -> s, 1) |> Map.ofList
        Valve(name, value, next)

    let valves = input |> List.map parse
    let paths = valves |> List.map (fun v -> v.Name,v.LeadsTo.Keys |>Set.ofSeq)
                       |> Map.ofSeq 
    
    let closed =
        valves
        |> Seq.filter (fun v -> v.FlowRate > 0)
        |> Seq.map (fun v -> v.Name,v.FlowRate)
        |> Map.ofSeq
    
    State("AA","AA",paths,closed,Set.empty)


