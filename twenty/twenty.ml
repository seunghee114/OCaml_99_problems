let remove_at idx input =
  let rec remove n ans = function
    | [] -> []
    | e::l -> if n = idx then (List.rev ans) @ l else remove (n+1) (e::ans) l
  in remove 0 [] input

let rec remove_at_sol idx = function
  | [] -> []
  | e::l -> if idx = 0 then l else e::(remove_at_sol (idx-1) l)

let intList = [1; 2; 3; 4; 5]
let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  Format.printf "\n"

let _ = 
  let _ = Format.printf "remove_at 1 : " in
  let _ = List.iter (Format.printf "%d ") (remove_at 1 intList) in
  Format.printf "\n"

let _ = 
  let _ = Format.printf "remove_at_sol 1 : " in
  let _ = List.iter (Format.printf "%d ") (remove_at_sol 1 intList) in
  Format.printf "\n"







