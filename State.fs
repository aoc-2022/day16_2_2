module day16_2_2.State

open day16_2_2.BaseTypes

type State(you:string, ele:string,paths:Paths, closed:Map<string,int>, opened:Set<string>) =
    member this.Paths = paths
    member this.You = you
    member this.Ele = ele 
    member this.Closed = closed
    member this.Opened = opened
    
    member this.Update ((your,eles) : string*string) =
        let youOpen = your = "o"
        let eleOpen = eles = "o"
        let closed = if youOpen then closed.Remove you else closed
        let closed = if eleOpen then closed.Remove ele else closed
        let opened = if youOpen then opened.Add you else opened
        let opened = if eleOpen then opened.Add ele else opened
        let you = if youOpen then you else your
        let ele = if eleOpen then ele else eles
        State(you,ele,paths,closed,opened)
        
    override this.ToString () = $"State({you},{ele},{closed},{opened})"


