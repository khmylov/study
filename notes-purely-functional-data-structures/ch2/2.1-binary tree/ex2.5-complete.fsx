type 'a Tree = | Empty | Node of left: 'a Tree * value: 'a * right: 'a Tree

let rec complete elem = function
| 0 -> Empty
| depth -> let child = complete elem (depth - 1) in Node(child, elem, child)
