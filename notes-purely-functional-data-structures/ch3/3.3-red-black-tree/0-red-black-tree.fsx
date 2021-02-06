// - Red node can't have Red child
// - Any path from empty node to root has the same number of Black nodes
// Therefore, the longest Red-Black path is at most twice as long as the shortest Black-only path,
// i.e. tree is fairly balanced
//
// For a tree of size N, its max depth <= 2 log(N+1)

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

let t1 = [1; 1; 10; 1; 20; 4; 3; 2; 1; 15] |> List.fold(fun acc x -> insert x acc) Empty
print t1

[1..14] |> List.fold(fun acc x -> insert x acc) Empty |> print
