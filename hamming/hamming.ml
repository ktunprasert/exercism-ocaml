type nucleotide = A | C | G | T

let hamming_distance (left : nucleotide list) (right : nucleotide list) =
  match (List.length left, List.length right) with
  | l, r when l == r ->
      let rec dist_acc left right acc =
        match (left, right) with
        | l :: left, r :: right ->
            dist_acc left right (acc + if l = r then 0 else 1)
        | _ -> acc
      in
      Ok (dist_acc left right 0)
  | 0, _ -> Error "left strand must not be empty"
  | _, 0 -> Error "right strand must not be empty"
  | _ -> Error "left and right strands must be of equal length"
