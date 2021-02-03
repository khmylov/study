// [And95] Let's decrease worst case amount of comparisons in `isMember` from 2d to (d+1)

let ensure condition = if not condition then failwithf ("Assertion not met")
let ensureAll condition = Seq.iter (condition >> ensure)

type 'a Tree = | Empty | Node of left: 'a Tree * value: 'a * right: 'a Tree

let isMember x = function
| Empty -> false
| Node(_, value, _) as tree ->
  let rec loop greaterOrEqual = function
  | Empty -> x = greaterOrEqual
  | Node (left, value, right) ->
    if x < value then loop greaterOrEqual left
    else loop value right
  loop value tree

let rec insert x = function
| Empty -> Node(Empty, x, Empty)
| Node (left, value, right) as n ->
  if x < value then Node(insert x left, value, right)
  elif x > value then Node(left, value, insert x right)
  else n

// -- tests

let t1 = Node(Empty, 20, Empty)
ensure (isMember 20 t1)
ensure (not <| isMember 21 t1)

let t2 =
  Empty
  |> insert 10 |> insert 11
  |> insert 9  |> insert 10
  |> insert 12 |> insert 1
  |> insert 2  |> insert 13

[1; 2; 9; 10; 11; 12; 13] |> ensureAll (fun x -> isMember x t2)
[0; 3; 4; 5; 6; 7; 8; 14; 15; -1] |> ensureAll (fun x -> not <| isMember x t2)

let t3 =
  Node(
    Node(
      Node(Empty, 2, Empty),
      3,
      Node(Empty, 4, Empty)
    ),
    5,
    Node(
      Node(Empty, 6, Empty),
      8,
      Node(Empty, 10, Empty)
    )
  )

[2; 3; 4; 5; 6; 8; 10] |> ensureAll (fun x -> isMember x t3)
[0; 1; 7; 9; 11] |> ensureAll (fun x -> not <| isMember x t3)
