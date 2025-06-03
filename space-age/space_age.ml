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
  match planet with
  | Mercury -> float n /. 0.2408467 /. year
  | Venus -> float n /. 0.61519726 /. year
  | Earth -> float n /. year
  | Mars -> float n /. 1.8808158 /. year
  | Jupiter -> float n /. 11.862615 /. year
  | Saturn -> float n /. 29.447498 /. year
  | Uranus -> float n /. 84.016846 /. year
  | Neptune -> float n /. 164.79132 /. year
