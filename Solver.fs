module day16_2_2.Solver

open System.Diagnostics.Contracts
open day16_2_2.BaseTypes
open day16_2_2.State
open day16_2_2.Utils
open day16_2_2.Cache
open day16_2_2.Track

let findMoves (state: State) =
    let together = state.You = state.Ele
    let apart = not together
    let youCanOpen = state.Closed.ContainsKey state.You
    let eleCanOpen = apart && state.Closed.ContainsKey state.Ele
    let youMoves = state.Paths[state.You] |> Set.toList
    let youMoves = if youCanOpen then "o" :: youMoves else youMoves
    let eleMoves = state.Paths[state.Ele] |> Set.toList
    let eleMoves = if eleCanOpen then "o" :: eleMoves else eleMoves
    let moves = carthesianList youMoves eleMoves

    let moves =
        if together then
            moves |> List.filter (fun (a, b) -> a <= b)
        else
            moves

    moves
    
let sortMovesByValue (moves:(string*string) list) (state:State) : (string*string) list =
    let valueOf id =
        if id = "o" then 1000
        elif state.Closed.ContainsKey id then state.Closed[id]
        else 0
    moves |> List.map (fun (y,e) -> (valueOf y, valueOf e,y,e))
          |> List.sort
          |> List.map (fun (_,_,y,e) -> (y,e))
         
        

let beenBetter (state: State) (cache: Cache) (track: Track) =
    let key = state.You, state.Ele, track.Time

    match cache.TryFind key with
    | None -> false
    | Some (opened, score) ->
        let openedMoreBefore = state.Opened.IsSubsetOf opened
        let betterScoreBefore = track.Score < score
        let beenBetterBefore = openedMoreBefore && betterScoreBefore
        // printfn $"cache found: {opened},{score} - better before={openedMoreBefore} {betterScoreBefore} -> {beenBetterBefore}"
        beenBetterBefore

let seenBetter (state: State) (track: Track) (move: string * string) =
    let (your, eles) = move

    let youLoopedWithoutOpening = track.youVisited.TryFind your = Some(track.youOpened)
    let eleLoopedWithoutOpening = track.eleVisited.TryFind eles = Some(track.eleOpened)

    // if (eleLoopedWithoutOpening) then printfn $"Ele looped rem={state.Closed.Count}"

    // let eleLoopedWithoutOpening = eleLoopedWithoutOpening && state.Closed.Count > 1 // safety, just in case

    youLoopedWithoutOpening || eleLoopedWithoutOpening

let updateCache (state: State) (cache: Cache) (track: Track) =
    let value = state.Opened, track.Score
    let key = state.You, state.Ele, track.Time
    let cache = cache.Add key value
    let cache = cache.RegisterScore track.Score
    cache

let addToTrack (track: Track) (state: State) = track

let bestScore(closed:Map<string,int>) (time:int) =
    (closed.Values |> Seq.sum) * time // initial - opens all at once now
    // if closed.IsEmpty then 0
    // let values = closed.Values |> Seq.sort | Seq.rev |> Seq.toList
    // let highest = closed.Values |> Seq.max
    // match values

let bestScore2 (closed:Map<string,int>) (time:int) =
    let time = max (time - 1) 0
    let v = closed.Values |> Seq.sort |> Seq.rev |> Seq.toList
    let v = if v.Length > time then v |> List.take time else v 
    // if closed.Values.Count > time then printfn $"LOTS {closed.Values |>Seq.sum} {v |> List.sum}"
    (v |> List.sum) * time  

let bestScore3 (closed:Map<string,int>) (time:int) : int =
    // let time = max (time - 1) 0
    let v = closed.Values |> Seq.toList |> Seq.sort |> Seq.rev |> Seq.toList 
    // printfn $"v{v}"
    let rec iter (v: int list) (time:int) : int =
        if time < 1 then 0
        else
            match v with
            | [] -> 0
            | [_] -> 0
            | a::b::rest -> ((a+b)*time) + (iter rest (time-2))
    iter v time 

let cantBeatTheScore (state:State) (cache: Cache) (track:Track) =
    let canEarn = bestScore2 state.Closed track.Time
    let potential = track.Score + canEarn
    cache.BestScore > potential 

let rec solve (depth: int) (state: State) (cache: Cache) (track: Track) : Cache =
    let cache = cache.RegisterScore 1617 // can't do worse than alone / 2171
    let prefix = " " |> String.replicate depth
    // printfn $"{prefix}solve: {state} {cache} {track}"
    // printfn $"{prefix}solve {state.You} {state.Ele} {state.Opened} {track.youVisited} {track.youOpened}"
    let tooLowScore = cantBeatTheScore state cache track
    // let bs1 = bestScore  state.Closed track.Time 
    // let bs2 = bestScore2 state.Closed track.Time 
    // let bs3 = bestScore3 state.Closed track.Time
    // printfn $"BEST REM: {bs1} {bs2} {bs3}"
    if (depth = 4) then
        printfn $"{prefix} {cache.BestScore} {track.Score} {track.Time} {bestScore state.Closed track.Time} {tooLowScore}"
    // bestScore2 state.Closed track.Time
    if cantBeatTheScore state cache track then 
        cache
    elif track.Time < 1 || state.Closed.IsEmpty then
        cache.RegisterScore track.Score
    elif beenBetter state cache track then
        cache.RegisterScore track.Score
    else
        let cache = updateCache state cache track
        let cache = updateCache state cache track
        let track = addToTrack track state
        let moves = findMoves state
        let moves = sortMovesByValue moves state 

        let rec iter (moves: (string * string) list) (cache: Cache) =
            match moves with
            | [] -> cache
            | move :: rest ->
                // printfn $"MOVE: {move}"
                let cache =
                    if seenBetter state track move then
                        cache
                    else
                        let track = track.Tick move
                        let state = state.Update move
                        solve (depth + 1) state cache track
                iter rest cache
        iter moves cache

let solution (state: State) (time: int) =
    let cache: Cache = solve 0 state Cache.empty (Track.empty time state.Closed)
    printfn $"RESULT: {cache}"
    printfn $"Result: opened={state.Opened}"
