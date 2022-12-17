module day16_2_2.Track

type Track
    (
        time: int,
        you: string,
        ele: string,
        youVisited: Map<string, int>,
        youOpened: int,
        eleVisited: Map<string, int>,
        eleOpened: int,
        score: int,
        flows: Map<string, int>
    ) =
    member this.Time = time
    member this.youVisited = youVisited
    member this.youOpened = youOpened
    member this.eleVisited = eleVisited
    member this.eleOpened = eleOpened
    member this.Score = score

    member this.Tick((youMove, eleMove)) =
        let time : int = time - 1
        let youOpen = youMove = "o"
        let eleOpen = eleMove = "o"
        let you = if youOpen then you else youMove
        let ele = if eleOpen then ele else eleMove

        let score =
            if youOpen then
                score + (flows[you] * time)
            else
                score
        let score =
            if eleOpen then
                score + (flows[ele] * time)
            else
                score 

        let youOpened = if youOpen then youOpened + 1 else youOpened

        let youVisited =
            if youOpen then
                youVisited.Add(you, youOpened)
            else
                youVisited.Add(youMove, youOpened)

        let eleOpened = if eleOpen then eleOpened + 1 else eleOpened

        let eleVisited =
            if eleOpen then
                eleVisited.Add(ele, youOpened)
            else
                eleVisited.Add(eleMove, eleOpened)

        Track(time, you, ele, youVisited, youOpened, eleVisited, eleOpened, score, flows)

    override this.ToString() =
        $"Track({time},{you} {ele},{youVisited},{youOpened},{eleVisited},{eleOpened})"

    static member empty (time: int) (flows: Map<string, int>) =
        let AA_0 = Map.empty.Add("AA", 0)
        Track(time, "AA", "AA", AA_0, 0, AA_0, 0, 0, flows)
