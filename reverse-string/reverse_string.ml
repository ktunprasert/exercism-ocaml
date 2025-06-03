let reverse_string str =
  String.init (String.length str) (fun i -> str.[String.length str - 1 - i])
