// `insert` without using `merge`

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
