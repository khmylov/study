// A queue may be implemented as 2 lists:
// `f` (heads forward) * `r` (tails reversed)
// enqueueing adds to `r`, and dequeueing removes from `f`
// `f` should be empty only if queue is empty
type 'a Queue = 'a list * 'a list

let empty: 'a Queue = [], []
let isEmpty (f, _) = List.isEmpty f
// O(1)
let head (x::_, _) = x
// technically O(n), but O(1) amortized
let tail = function
| [_], r -> List.rev r, []
| _::f, r -> f, r

// `snoc` is a reverse of `cons`, i.e. "add to the right side" here
let snoc (f, r) x = match f with [] -> [x], [] | _ -> f, x::r
