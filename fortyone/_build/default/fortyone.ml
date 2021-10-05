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

let goldbach_list a b =
  let rec ans i =
    if i > b then []
    else 
      if i mod 2 = 0 then (i, goldbach i)::(ans (i+1)) else ans (i+1)
  in ans a

let goldbach_limit _ b i =
  let rec ans j =
    if j > b then []
    else
      if j mod 2 = 0 then
        let k = goldbach j in
        if (fst k) > i then (j, k)::(ans (j+1)) else ans (j+1)
      else ans (j+1)
  in ans i

let rec goldbach_list_sol a b =
  if a > b then [] else
    if a mod 2 = 1 then goldbach_list_sol (a+1) b
    else (a, goldbach a)::goldbach_list_sol (a+2) b

let goldbach_limit_sol a b lim =
  List.filter(fun (_, (a, b)) -> a > lim && b > lim) (goldbach_list a b)

let gl = goldbach_list 9 20
let _ = Format.printf "goldbach_list 9 20 :\n"
let _ = List.iter
        (fun e ->
                Format.printf "(%d (%d %d)) " (fst e) (fst (snd e)) (snd (snd e))) gl
let _ = Format.printf "\n"

let gl2 = goldbach_limit 1 2000 50
let _ = Format.printf "goldbach_limit 1 2000 50 :\n"
let _ = List.iter
        (fun e ->
                Format.printf "(%d (%d %d)) " (fst e) (fst (snd e)) (snd (snd e))) gl2
let _ = Format.printf "\n\n"

let gls = goldbach_list_sol 9 20
let _ = Format.printf "goldbach_list_sol 9 20 :\n"
let _ = List.iter
        (fun e ->
                Format.printf "(%d (%d %d)) " (fst e) (fst (snd e)) (snd (snd e))) gls
let _ = Format.printf "\n"

let gls2 = goldbach_limit_sol 1 2000 50
let _ = Format.printf "goldbach_limit_sol 1 2000 50 :\n"
let _ = List.iter
        (fun e ->
                Format.printf "(%d (%d %d)) " (fst e) (fst (snd e)) (snd (snd e))) gls2
let _ = Format.printf "\n"


