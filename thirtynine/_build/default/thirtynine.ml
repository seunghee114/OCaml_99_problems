let is_prime n =
  let n  = max n (-n) in
  let rec is_not_divisor d = 
    d*d > n || (n mod d <> 0 && is_not_divisor (d+1))
  in is_not_divisor 2
let rec all_primes a b =
  if a > b then [] else
    let rest = all_primes (a+1) b in 
    if is_prime a then a::rest else rest

let _ = Format.printf "all_primes 2 7920 length : %d\n" (List.length (all_primes 2 7920))
let _ = Format.printf "all_primes 2 100 length : %d\n" (List.length (all_primes 2 100))
let _ = Format.printf "all_primes 2 1000 length : %d\n" (List.length (all_primes 2 1000))
let _ = Format.printf "all_primes 100 1000 length : %d\n" (List.length (all_primes 100 1000))

