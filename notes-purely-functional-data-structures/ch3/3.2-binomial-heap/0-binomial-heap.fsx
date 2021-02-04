// Binomial tree of rank N has exactly 2^N nodes,
// and its nodes obey heap property (min or max).
// Each sub-tree of binomial tree is binomial tree as well.
//
// Binomial tree of rank N has N direct children t1..tN,
// which have ranks t(i) = N - i, i.e.
// any node has direct children of all possible lower ranks.
//
// Binomial heap is a list of binomial trees, such as:
// - there is at most 1 binomial tree of each rank
// - trees are stored in rank-increasing order
// - subtrees are stored in rank-decreasing order
//
// Binomial heaps are "similar" to binary representation of a number.
// For example, 21dec=10101bin.
// Binomial heap of 21 nodes consists of 1 tree of rank 0 (1 item), 1 tree of rank 2 (4 items), and 1 tree of rank 4 (16 items).
// Just like a binary number representation, binomial heap of size N has at most log(N+1) root trees.

type 'a Tree = Node of rank : int * element : 'a * nodes : 'a Tree list

let rank (Node(r, _, _)) = r
let root (Node(_, elem, _)) = elem

let singleton elem = Node(0, elem, [])

// Link 2 binomial trees of rank N into a single binomial tree of rank N+1
let link t1 t2 =
  let (Node(r1, el1, ns1)) = t1
  let (Node(r2, el2, ns2)) = t2
  if r1 <> r2 then raise(System.ArgumentException())
  if el1 <= el2 then Node(r1 + 1, el1, t2::ns1)
  else Node(r1 + 1, el2, t1::ns2)

// Insert a given binomial tree into a binomial heap preserving rank rules:
// trees with lower ranks are placed earlier.
// Worst case needs log(n+1) `link` calls, so it has O(log n) time complexity
let rec insTree tree = function
| [] -> [tree]
| t'::_ as heap when rank tree < rank t' -> tree::heap
| t'::ts' -> insTree (link tree t') ts'

// Insert 1 element into a heap
let insert x heap = insTree (singleton x) heap

// Merging two heaps is done by going through their trees (which are in rank-increasing order),
// and linking trees of the same rank (producing a tree of rank N+1),
// which is logically similar to carrying over in binary representation
let rec merge = function
| heap1, [] -> heap1
| [], heap2 -> heap2
| (t1::tail1 as heap1), (t2::tail2 as heap2) ->
  if rank t1 < rank t2 then t1::merge(tail1, heap2)
  elif rank t2 < rank t1 then t2::merge(heap1, tail2)
  else insTree (link t1 t2) (merge(tail1, tail2))

// Removes a tree with a minimal root value from a heap,
// returning removed tree and the remaining trees
let rec removeMinTree = function
| [] -> raise (System.ArgumentException())
| [t] -> t, []
| t::ts ->
  let (t', ts') = removeMinTree ts
  if root t <= root t' then t, ts else t', t::ts'

let findMin heap = heap |> List.minBy root

// Returns a new heap with min value deleted from it
let deleteMin heap =
  let (Node(_, _, children), trees) = removeMinTree heap
  // Min value is a root of some tree, which is removed from a heap
  // We need to insert children of that root back into a heap
  // By definition of binomal heap, sub-trees are stored in rank-decreasing order, while root trees are stored in rank-increasing.
  // Therefore, we just need to reverse the children of the removed root, and merge this "almost heap" with the remaining trees.
  merge (List.rev children, trees)
