let rec suffixes = function
| [] -> [[]]
| _::tl as xs -> xs::(suffixes tl)

suffixes [1..4]
