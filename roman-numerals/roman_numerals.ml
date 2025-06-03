let rec to_roman (n : int) : string =
  match n with
  | n when n >= 1000 -> "M" ^ to_roman (n - 1000)
  | n when n >= 900 -> "CM" ^ to_roman (n - 900)
  | n when n >= 500 -> "D" ^ to_roman (n - 500)
  | n when n >= 400 -> "CD" ^ to_roman (n - 400)
  | n when n >= 100 -> "C" ^ to_roman (n - 100)
  | n when n >= 90 -> "XC" ^ to_roman (n - 90)
  | n when n >= 50 -> "L" ^ to_roman (n - 50)
  | n when n >= 40 -> "XL" ^ to_roman (n - 40)
  | n when n >= 10 -> "X" ^ to_roman (n - 10)
  | n when n >= 9 -> "IX" ^ to_roman (n - 9)
  | n when n >= 5 -> "V" ^ to_roman (n - 5)
  | n when n >= 4 -> "IV" ^ to_roman (n - 4)
  | n when n >= 1 -> "I" ^ to_roman (n - 1)
  | _ -> ""
