let is_prime n = 
  let n  = abs n in
  let rec is_not_divisor d = 
    d*d > n || (n mod d <> 0 && is_not_divisor (d+1))
  in n<>1 &&is_not_divisor 2

let factors n =
  let rec ans acc n m = 
    if n = 1 then acc
    else
      if (is_prime m) then 
        let modular = n mod m in
        let quotient = n / m in
        if modular = 0 then ans (m::acc) quotient m
        else ans acc n (m+1)
      else ans acc n (m+1)
  in List.rev (ans [] n 2)

let factors_sol n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then d::(aux d (n/d)) else aux (d+1) n 
  in aux 2 n 

let input = [315; 60; 46; 32]

let _ = List.iter(fun e -> let _ = Format.printf "factor %d :" e in
                           let a = factors e in
                           let _ = List.iter (Format.printf "%d ") a in
                           Format.printf "\n") input
let _ = Format.printf "\n"
let _ = List.iter(fun e -> let _ = Format.printf "factor_sol %d :" e in
                           let a = factors_sol e in
                           let _ = List.iter (Format.printf "%d ") a in
                           Format.printf "\n") input


