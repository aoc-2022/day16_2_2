module day16_2_2.Solver

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

let rec solve (depth: int) (state: State) (cache: Cache) (track: Track) : Cache =
    let prefix = " " |> String.replicate depth
    // printfn $"{prefix}solve: {state} {cache} {track}"
    // printfn $"{prefix}solve {state.You} {state.Ele} {state.Opened} {track.youVisited} {track.youOpened}"

    if track.Time < 1 || state.Closed.IsEmpty then
        cache.RegisterScore track.Score
    elif beenBetter state cache track then
        cache.RegisterScore track.Score
    else
        let cache = updateCache state cache track
        let cache = updateCache state cache track
        let track = addToTrack track state
        let moves = findMoves state

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
