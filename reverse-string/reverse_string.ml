let reverse_string str =
  let rec rev str acc =
    match str with
    | "" -> acc
    | _ ->
        rev
          (String.sub str 1 (String.length str - 1))
          (String.make 1 (String.get str 0) ^ acc)
  in
  rev str ""
