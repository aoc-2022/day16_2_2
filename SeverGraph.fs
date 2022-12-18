module day16_2_2.SeverGraph

open day16_2_2.State

let findDistances (state:State) (time:int) : Map<string,int> =
    let costs = state.Paths.Keys |> Seq.map (fun cave -> cave,100000) |> Map.ofSeq 
    let costs = costs.Add("AA",0)
    let rec updateCost (costs:Map<string,int>) ((cave,cost):string*int) : string*int =
        let nears = state.Paths[cave] |> Set.map (fun id -> costs[id] + 1) |> Set.minElement
        if (nears < cost) then cave,nears else cave,cost
    let updates = costs |> Map.toSeq |> Seq.map (updateCost costs) |> Map.ofSeq
    let rec updateAll (iter:int) (costs:Map<string,int>) : Map<string,int> =
        if iter < 1 then costs
        else 
            let costs = costs |> Map.toSeq |> Seq.map (updateCost costs) |> Map.ofSeq
            updateAll (iter - 1) costs
    updateAll 30 costs 
         
        
let optimize (state:State) (time:int) =
    let distances = findDistances state time
    let distant = distances |> Map.toSeq
                              |> Seq.filter (fun (k,v) -> v > 18)
                              |> Seq.map fst |> Seq.toList
    distant |> List.map (printfn "%A")
    // remove these 

