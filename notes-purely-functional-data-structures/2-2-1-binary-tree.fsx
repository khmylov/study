let ensure condition = if not condition then failwithf ("Assertion not met")

type 'a Tree = | Empty | Node of left: 'a Tree * value: 'a * right: 'a Tree

let rec isMember x = function
| Empty -> false
| Node (left, value, right) ->
  if x < value then isMember x left
  elif x > value then isMember x right
  else true

let rec insert x = function
| Empty -> Node(Empty, x, Empty)
| Node (left, value, right) as n ->
  if x < value then Node(insert x left, value, right)
  elif x > value then Node(left, value, insert x right)
  else n

let t1 = Node(Empty, 20, Empty)
ensure (isMember 20 t1)
ensure (not <| isMember 21 t1)

let t2 =
  Empty
  |> insert 10 |> insert 11
  |> insert 9  |> insert 10
  |> insert 12 |> insert 1
  |> insert 2  |> insert 13

[1; 2; 9; 10; 11; 12; 13] |> Seq.iter (fun x -> ensure <| isMember x t2)
[0; 3; 6; 14; -1] |> Seq.iter (fun x -> ensure (not <| isMember x t2))