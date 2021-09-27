let range start finish =
  let rec increase n =
    if n > finish then [] else n::(increase (n+1))
  in
  let rec decrease n =
    if n < finish then [] else n::(decrease (n-1))
  in
  if start < finish then increase start else decrease start

let range_sol a b =
  let rec aux a b =
    if a > b then [] else a::(aux (a+1) b)
  in
  if a > b then List.rev (aux b a) else aux a b

let _ = Format.printf "range 4 9 : "
let _ = List.iter (Format.printf "%d ") (range 4 9)
let _ = Format.printf "\n"

let _ = Format.printf "range 9 4 : "
let _ = List.iter (Format.printf "%d ") (range 9 4)
let _ = Format.printf "\n"

let _ = Format.printf "range_sol 4 9 : "
let _ = List.iter (Format.printf "%d ") (range_sol 4 9)
let _ = Format.printf "\n"

let _ = Format.printf "range_sol 9 4 : "
let _ = List.iter (Format.printf "%d ") (range_sol 9 4)
let _ = Format.printf "\n"





