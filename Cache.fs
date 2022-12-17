module day16_2_2.Cache

type CacheKey = string * string * int // you,ele,time
type CacheValue = Set<string> * int // opened,score

type Cache(cache: Map<CacheKey, CacheValue>, bestScore: int) =
    member this.BestScore = bestScore
    member this.TryFind(key: CacheKey) : Option<CacheValue> = cache.TryFind key
    member this.Add (key: CacheKey) (value: CacheValue) = Cache(cache.Add(key, value), bestScore)

    member this.RegisterScore(score: int) =
        let bestScore = max score bestScore
        Cache(cache, bestScore)

    override this.ToString() = $"Cache({cache},best={bestScore})"
    static member empty = Cache(Map.empty, 0)
