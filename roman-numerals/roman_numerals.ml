let to_roman (n : int) : string =
  let rec aux current_n acc =
    match current_n with
    | n when n >= 1000 -> aux (n - 1000) ("M" :: acc)
    | n when n >= 900 -> aux (n - 900) ("CM" :: acc)
    | n when n >= 500 -> aux (n - 500) ("D" :: acc)
    | n when n >= 400 -> aux (n - 400) ("CD" :: acc)
    | n when n >= 100 -> aux (n - 100) ("C" :: acc)
    | n when n >= 90 -> aux (n - 90) ("XC" :: acc)
    | n when n >= 50 -> aux (n - 50) ("L" :: acc)
    | n when n >= 40 -> aux (n - 40) ("XL" :: acc)
    | n when n >= 10 -> aux (n - 10) ("X" :: acc)
    | n when n >= 9 -> aux (n - 9) ("IX" :: acc)
    | n when n >= 5 -> aux (n - 5) ("V" :: acc)
    | n when n >= 4 -> aux (n - 4) ("IV" :: acc)
    | n when n >= 1 -> aux (n - 1) ("I" :: acc)
    | _ -> String.concat "" (List.rev acc)
  in
  aux n []
