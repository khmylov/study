// ex. 3.6
// Store rank info only in root nodes
type 'a Tree = Node of element : 'a * nodes : 'a Tree list
type 'a Root = Root of rank: int * tree: 'a Tree
type 'a Heap = 'a Root list

let root (Node(elem, _)) = elem

let singleton elem = Root(0, Node(elem, []))

// Link 2 binomial trees of rank N into a single binomial tree of rank N+1
let link t1 t2 =
  let (Node(el1, ns1)) = t1
  let (Node(el2, ns2)) = t2
  if el1 <= el2 then Node(el1, t2::ns1)
  else Node(el2, t1::ns2)

// Insert a given binomial tree into a binomial heap preserving rank rules:
// trees with lower ranks are placed earlier.
// Worst case needs log(n+1) `link` calls, so it has O(log n) time complexity
let rec insTree (Root(rank, tree)) = function
| [] -> [Root(rank, tree)]
| Root(r', _)::_ as heap when rank < r' -> Root(rank, tree)::heap
| Root(_, t')::ts' -> insTree (Root(rank + 1, link tree t')) ts'

// Insert 1 element into a heap
let insert x heap = insTree (singleton x) heap

// Merging two heaps is done by going through their trees (which are in rank-increasing order),
// and linking trees of the same rank (producing a tree of rank N+1),
// which is logically similar to carrying over in binary representation
let rec merge = function
| heap1, [] -> heap1
| [], heap2 -> heap2
| (Root(r1, t1)::tail1 as heap1), (Root(r2, t2)::tail2 as heap2) ->
  if r1 < r2 then Root(r1, t1)::merge(tail1, heap2)
  elif r2 < r1 then Root(r2, t2)::merge(heap1, tail2)
  else insTree (Root(r1, link t1 t2)) (merge(tail1, tail2))

// Removes a tree with a minimal root value from a heap,
// returning removed tree and the remaining trees
let rec removeMinTree = function
| [] -> raise (System.ArgumentException())
| [t] -> t, []
| (Root(_, tree) as x)::tail ->
  let ((Root(_, tree') as removed), ts') = removeMinTree tail
  if root tree <= root tree' then x, tail else removed, x::ts'

let findMin heap = heap |> List.minBy root

// Returns a new heap with min value deleted from it
let deleteMin heap =
  // Min value is a root of some tree, which is removed from a heap
  // We need to insert children of that root back into a heap
  // By definition of binomal heap, sub-trees are stored in rank-decreasing order, while root trees are stored in rank-increasing.
  // Therefore, we just need to reverse the children of the removed root, and merge this "almost heap" with the remaining trees.
  //
  // Because we store rank only at root level now, we need to append rank info to each new "root" created from child node
  // Fortunately, we know that binomial trees store their subtrees in rank-decreasing order,
  // and any node of rank N has children for each rank in [0..N-1]
  // Therefore, we can iterate over children and keep decreasing their ranks.
  let rec toHeap rank = function
  | [] -> []
  | t::ts -> Root(rank, t)::(toHeap (rank - 1) ts)

  let (Root(rank, Node(_, children)), trees) = removeMinTree heap
  merge (children |> List.rev |> toHeap (rank - 1), trees)
