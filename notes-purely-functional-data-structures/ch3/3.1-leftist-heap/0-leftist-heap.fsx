// value(node) < value(child)
// rank(node) = length of right spine
// right spine = right path to empty node
// rank(left) >= rank(right)
//
// The path from root to rightmost leaf is the shortest path from root to a leaf
// Main operation is "merge", which has O(log n) time complexity
// Right subtree is smaller, so merge it with other tree

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
| heap1, Empty -> heap1
| Empty, heap2 -> heap2
| Node (_, elem1, left1, right1) as heap1, (Node (_, elem2, left2, right2) as heap2) ->
    if elem1 <= elem2 then
      makeT left1 (merge (right1, heap2)) elem1
    else
      makeT left2 (merge (heap1, right2)) elem2

let singleton element = Node(1, element, Empty, Empty)

let insert element heap =
  merge (singleton element, heap)

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

// -- tests

let heap1 =
  insert 1 (
    merge(
      insert 6 (merge (singleton 7, singleton 9)),
      insert 12 (singleton 14)
    )
  )

let heap2 =
  insert 2 (
    merge (
      insert 5 (merge (singleton 6, singleton 7)),
      insert 8 (singleton 11)
    )
  )

let heapMerged = merge (heap1, heap2)
printHeap heapMerged
