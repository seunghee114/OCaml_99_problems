let is_prime n =
  let rec ans i =
    if i = n then true
    else if n mod i = 0 then false else ans (i+1)
  in if n = 1 then false else ans 2

let is_prime_sol n =
  let n = abs n in
  let rec is_not_divisor d =
    d*d > n || (n mod d <> 0 && is_not_divisor (d+1))
  in n <> 1 && is_not_divisor 2

let i = [1; 2; 7; 12; 19; 56]

let _ = List.iter(fun l -> Format.printf "%d is prime : %b\n" l (is_prime l)) i
let _ = Format.printf "\n"
let _ = List.iter(fun l -> Format.printf "%d is prime sol: %b\n" l (is_prime_sol l)) i

