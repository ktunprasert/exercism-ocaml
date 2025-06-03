let square_of_sum n =
  let rec sum_acc x acc = if x = 0 then acc else sum_acc (x - 1) (acc + x) in
  let sum = sum_acc n 0 in
  sum * sum

let rec sum_of_squares n =
  match n with 1 -> 1 | n -> (n * n) + sum_of_squares (n - 1)

let difference_of_squares n = (-) (square_of_sum n) (sum_of_squares n)
