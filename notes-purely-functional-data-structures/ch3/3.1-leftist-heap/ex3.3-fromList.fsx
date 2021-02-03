// add `fromList: 'a list -> 'a Heap` function
open System.Text

type 'a Heap =
  | Empty
  | Node of rank: int * element: 'a * left: 'a Heap * right: 'a Heap

let rank = function
| Empty -> 0
| Node (r, _, _, _) -> r

let makeT a b element =
  if rank a >= rank b then
    Node(rank b + 1, element, a, b)
  else
    Node(rank a + 1, element, b, a)

let rec merge = function
| Empty, Empty -> Empty
| heap1, Empty -> heap1
| Empty, heap2 -> heap2
| Node (_, elem1, left1, right1) as heap1, (Node (_, elem2, left2, right2) as heap2) ->
    if elem1 <= elem2 then
      makeT left1 (merge (right1, heap2)) elem1
    else
      makeT left2 (merge (heap1, right2)) elem2

let singleton element = Node(1, element, Empty, Empty)

let rec insert element = function
| Empty -> singleton element
| Node(_, elem2, left2, right2) as heap2 ->
  if element <= elem2 then Node(1, element, heap2, Empty)
  else makeT left2 (insert element right2) elem2

let findMin = function
| Node (_, x, _, _) -> Some x
| _ -> None

let deleteMin = function
| Node (_, _, left, right) -> Some <| merge (left, right)
| _ -> None

let printHeap h =
  let sb = StringBuilder()
  let rec loop depth = function
  | Empty -> ()
  | Node(r, elem, left, right) ->
    for _ in 0..depth do sb.Append('-') |> ignore
    sb.AppendLine $"{elem}: r{r}" |> ignore
    loop (depth + 1) left
    loop (depth + 1) right
  loop 0 h
  printfn "%s" <| sb.ToString()

// `insert` has `O(log n)` complexity
// Therefore, this fold-based implementation is `O(n * log n)`
let fromList1 xs = xs |> List.fold (fun acc elem -> insert elem acc) Empty

let fromList2 xs =
  let rec pair = function
  | [] -> []
  | [_] as t -> t
  | x1::x2::tl -> merge (x1, x2) :: pair tl
  and loop = function
  | [x] -> x
  | xs -> loop (pair xs)

  xs
  |> List.map singleton
  |> loop

printHeap <| fromList1 [2;0; 3; 16;1; 29; 30; 15; 4]
printHeap <| fromList2 [2;0; 3; 16;1; 29; 30; 15; 4]
