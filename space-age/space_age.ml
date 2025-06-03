type planet =
  | Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Neptune
  | Uranus

(*
Mercury	0.2408467
Venus	0.61519726
Earth	1.0
Mars	1.8808158
Jupiter	11.862615
Saturn	29.447498
Uranus	84.016846
Neptune	164.79132
*)

let year : float = 31557600.

let age_on planet n =
  let div =
    match planet with
    | Mercury -> 0.2408467
    | Venus -> 0.61519726
    | Earth -> 1.
    | Mars -> 1.8808158
    | Jupiter -> 11.862615
    | Saturn -> 29.447498
    | Uranus -> 84.016846
    | Neptune -> 164.79132
  in
  float n /. div /. year
