let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)
  
let coprime n1 n2 =
  if (gcd n1 n2) = 1 then true else false

let coprime_sol n1 n2 = (gcd n1 n2 = 1)

let input = [(13, 27); (20536, 7826);(2, 7)]

let _ = List.iter
(fun e -> Format.printf "coprime %d %d : %b\n" (fst e) (snd e) (coprime (fst e) (snd e))) input
let _ = Format.printf "\n"
let _ = List.iter
(fun e -> Format.printf "coprime %d %d : %b\n" (fst e) (snd e) (coprime_sol (fst e) (snd e))) input




