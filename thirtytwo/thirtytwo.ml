let rec gcd a b =
  let c = if a > b then a else b in
  let d = if a > b then b else a in
  let rest = c mod d in
  if rest = 0 then d else gcd d rest

let rec gcd_sol a b =
  if b = 0 then a else gcd_sol b (a mod b)

let e = [(13,27); (20536, 7826); (33, 121)]

let _ = List.iter(fun a -> 
  Format.printf "gcd %d %d : %d\n" (fst a) (snd a) (gcd (fst a) (snd a))) e
let _ = Format.printf "\n"
let _ = List.iter(fun a -> 
  Format.printf "gcd_sol %d %d : %d\n" (fst a) (snd a) (gcd_sol (fst a) (snd a))) e
