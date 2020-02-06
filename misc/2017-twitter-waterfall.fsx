// "Twitter waterfall" problem
// Some links:
//    http://chrisdone.com/posts/twitter-problem-loeb
//    http://brandon.si/code/writing-a-streaming-twitter-waterflow-solution/

type Wall = int
type Pool = Wall array

module RecurrentSolution = 
    // Naive and slow, based on recurrent definition of "water volume on top of ith column"

    // Gets the peak column height to the left from the given column index
    let rec leftPeak (pool: Pool) (index: int) = 
        if index <= 0 then pool.[index] 
        else max (pool.[index]) (leftPeak pool (index - 1))

    // Same for the right side
    let rec rightPeak (pool: Pool) (index: int) = 
        if index >= pool.Length - 1 then pool.[index]
        else max (pool.[index]) (rightPeak pool (index + 1))

    // Volume of water gathered on top of a given column
    let columnVolume (pool: Pool) (index: int) = 
        if index <= 0 || index >= pool.Length - 1 then 0 
        else 
            let minPeak = min (leftPeak pool index) (rightPeak pool index)
            max (minPeak - pool.[index]) 0

    let poolVolume (pool: Pool) = 
        seq { 0..pool.Length - 1 } |> Seq.sumBy (columnVolume pool)

module SinglePassSolution = 
    let poolVolume (pool: Pool) = 
        let rec loop left right maxLeft maxRight totalWater = 
            if left > right then totalWater
            elif pool.[left] >= maxLeft then
                // Keep skipping columns from the left, where water runs off
                loop (left + 1) right (pool.[left]) maxRight totalWater
            elif (pool.[right] >= maxRight) then
                // Same for right side
                loop left (right - 1) maxLeft (pool.[right]) totalWater
            else
                let columnVolume = min maxLeft maxRight - pool.[left]
                loop (left + 1) right maxLeft maxRight (totalWater + columnVolume)
        loop 0 (pool.Length - 1) 0 0 0

let test (pool: Pool) (expectedVolume: int) (solver: Pool -> int) = 
    let actualVolume = solver pool
    if actualVolume = expectedVolume then "OK"
    else sprintf "FAIL %A. Expected: %i. Actual: %i" pool expectedVolume actualVolume

let testCases = [
    [| |], 0
    [| 0 |], 0
    [| 1 |], 0
    [| 1; 1 |], 0
    [| 0; 1 |], 0
    [| 1; 0 |], 0
    [| 1; 0; 1 |], 1
    [| 5; 1; 3 |], 2
    [| 2; 5; 1; 2; 3; 4; 7; 7; 6 |], 10
    [| 0; 4; 2; 2; 1; 1; 3; 1; 1; 5 |], 17
]

let solvers = [
    "Recurrent", RecurrentSolution.poolVolume
    "Single pass", SinglePassSolution.poolVolume
]

for (solverName, solver) in solvers do
    printfn "%s:" solverName
    testCases |> Seq.iteri (fun index (pool, expectedVolume) ->
        printfn "\tTest case #%i: %s" index (test pool expectedVolume solver)
    )
    printfn ""
