open Base

type bst = Empty | Node of int * bst * bst

let empty = Empty

let value bst =
  match bst with Empty -> Error "Empty tree" | Node (v, _, _) -> Ok v

let left bst =
  match bst with Empty -> Error "Empty tree" | Node (_, left, _) -> Ok left

let right bst =
  match bst with Empty -> Error "Empty tree" | Node (_, _, right) -> Ok right

let rec insert n bst =
  match bst with
  | Empty -> Node (n, Empty, Empty)
  | Node (v, left, right) ->
      if n <= v then Node (v, insert n left, right)
      else Node (v, left, insert n right)

let rec to_list bst =
  match bst with
  | Empty -> []
  | Node (v, Empty, Empty) -> [ v ]
  | Node (v, left, right) -> to_list left @ [ v ] @ to_list right
