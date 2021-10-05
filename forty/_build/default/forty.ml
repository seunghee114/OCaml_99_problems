let is_prime n =
  let n = abs n in
  let rec is_not_divisor d = 
    d*d> n || (n mod d <> 0 && is_not_divisor (d+1)) in
  n<>1 && is_not_divisor 2

let goldbach n =
  let rec ans i =
    if is_prime i then 
      if is_prime (n - i) then (i, n - i) else ans (i+1)
    else ans (i+1)
  in ans 2

let even = [28; 34; 38; 42]
let _ = List.iter(fun e -> let a = goldbach e in
                  Format.printf "goldbach %d : (%d %d)\n" e (fst a) (snd a)) even



