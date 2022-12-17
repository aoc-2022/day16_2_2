module day16_2_2.Utils

let carthesianList (al: 'a list) (bl: 'b list) : ('a*'b) list =
    al |> List.collect (fun a -> bl |> List.map (fun b -> (a,b)))

