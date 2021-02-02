type Tree<'Key, 'Element> =
  | Empty
  | Node of key: 'Key * element: 'Element * left: Tree<'Key, 'Element> * right: Tree<'Key, 'Element>

let rec bind key element = function
| Empty -> Node(key, element, Empty, Empty)
| Node(key', el', left, right) ->
  if key < key' then Node(key', el', bind key element left, right)
  elif key > key' then Node(key', el', left, bind key element right)
  else Node(key, element, left, right)

let rec lookup key = function
| Empty -> None
| Node(key', element, left, right) ->
  if key < key' then lookup key' left
  elif key > key' then lookup key' right
  else Some element
