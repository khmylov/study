// From "Grokking Algorithms" book
// Greedy algorithm to __approximate__ the best way to choose radio stations to cover all states
type Station = string
type State = string

let stations: list<Station * Set<State>> =
    Map.empty
    |> Map.add "kone" (Set.ofList [ "id"; "nv"; "ut" ])
    |> Map.add "ktwo" (Set.ofList [ "wa"; "id"; "mt" ])
    |> Map.add "kthree" (Set.ofList [ "or"; "nv"; "ca" ])
    |> Map.add "kfour" (Set.ofList [ "nv"; "ut" ])
    |> Map.add "kfive" (Set.ofList [ "ca"; "az" ])
    |> Map.toList

let rec loop acc remainingStates =
    if Set.isEmpty remainingStates then acc
    else
        let (nextBest, covered) =
            stations
            |> Seq.maxBy (fun (_, states) -> Set.intersect remainingStates states |> Set.count)
        loop (Set.add nextBest acc) (remainingStates - covered)

loop Set.empty (stations |> Seq.collect snd |> Set.ofSeq)
