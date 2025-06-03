type nucleotide = A | C | G | T

let hamming_distance (left : nucleotide list) (right : nucleotide list) =
  let len_left = List.length left in
  let len_right = List.length right in

  if len_left = len_right then
    let rec dist_acc left right acc =
      match (left, right) with
      | l :: left, r :: right ->
          dist_acc left right (acc + if l = r then 0 else 1)
      | _ -> acc
    in
    Ok (dist_acc left right 0)
  else if len_left = 0 then Error "left strand must not be empty"
  else if len_right = 0 then Error "right strand must not be empty"
  else Error "left and right strands must be of equal length"
