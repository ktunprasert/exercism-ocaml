let leap_year = function
  | n when n mod 400 = 0 -> true
  | n when n mod 100 = 0 -> false
  | n -> n mod 4 == 0
