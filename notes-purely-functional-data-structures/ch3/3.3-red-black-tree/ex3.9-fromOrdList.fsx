// ex. 3.9 implement `fromOrdList` function,
// which creates RB-tree from ordered list in O(n) time

type Color = | Red | Black
type 'a Tree =
  | Empty // empty node is considered to have Black color
  | Node of color : Color * left : 'a Tree * element : 'a * right : 'a Tree

let rec isMember x = function
| Empty -> false
| Node(_, left, y, right) ->
  if x < y then isMember x left
  elif x > y then isMember x right
  else true

// When we got some Black node which has Red child and Red grandchild (which violates RB-tree rules),
// we rebalance that subtree.
let balance color left element right =
  match color, left, element, right with
  | Black, Node(Red, Node(Red, a, x, b), y, c), z, d
  | Black, Node(Red, a, x, Node(Red, b, y, c)), z, d
  | Black, a, x, Node(Red, Node(Red, b, y, c), z, d)
  | Black, a, x, Node(Red, b, y, Node(Red, c, z, d)) -> Node(Red, Node(Black, a, x, b), y, Node(Black, c, z, d))
  | _ -> Node(color, left, element, right)

let insert element tree =
  let rec loop = function
  | Empty -> Node(Red, Empty, element, Empty)
  | Node(color, left, el', right) as node ->
    if element < el' then balance color (loop left) el' right
    elif element > el' then balance color left el' (loop right)
    else node

  let (Node(_, left, el', right)) = loop tree
  Node(Black, left, el', right)

let fromOrdList (items: 'a list): 'a Tree =
  // Receives a right spine built so far
  let rec balance' = function
  | [Red, v, leftSubTree] -> [Black, v, leftSubTree]
  | (Red, v1, t1)::(Red, v2, t2)::(Black, v3, t3)::xs ->
    (Black, v1, t1)::balance'((Red, v2, Node(Black, t3, v3, t2))::xs)
  | xs -> xs

  items
  // Accumulate a right spine as (color, value, leftSubTree)
  |> List.fold (fun acc x -> (Red, x, Empty)::acc |> balance') []
  |> List.fold (fun right (color, elem, left) -> Node(color, left, elem, right)) Empty

// -- tests

let print tree =
  let sb = System.Text.StringBuilder()
  let rec loop depth = function
  | Empty -> ()
  | Node(color, left, elem, right) ->
    for _ in 0..depth do sb.Append('-') |> ignore
    sb.AppendLine $"{elem}: {color}" |> ignore
    loop (depth + 1) left
    loop (depth + 1) right
  loop 0 tree
  printfn "%s" <| sb.ToString()

[1..10] |> fromOrdList |> print
