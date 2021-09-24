let split input number =
  let rec answer one origin =
    if List.length origin < number then (origin, [])
    else
      match origin with
      | [] -> ([], [])
      | e::l as t -> if List.length one = number then (List.rev one, t) else answer (e::one) l
  in answer [] input

let split_sol input n = 
  let rec aux i acc = function
    | [] -> List.rev acc, []
    | h::t as l -> if i = 0 then List.rev acc, l else aux (i-1) (h::acc) t
  in aux n [] input

let intList = [1; 2; 3; 4; 5; 6]
let _ =
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  Format.printf "\n"

let a = split intList 3 
let _ = Format.printf "split 3 fst : " 
let _ = List.iter (Format.printf "%d ") (fst a) 
let _ = Format.printf " snd : " 
let _ = List.iter (Format.printf "%d ") (snd a) 
let _ = Format.printf "\n"

let b = split intList 7
let _ = Format.printf "split 7 fst : " 
let _ = List.iter (Format.printf "%d ") (fst b) 
let _ = Format.printf " snd : " 
let _ = List.iter (Format.printf "%d ") (snd b) 
let _ = Format.printf "\n"

let c = split_sol intList 3 
let _ = Format.printf "split_sol 3 fst : " 
let _ = List.iter (Format.printf "%d ") (fst c) 
let _ = Format.printf " snd : " 
let _ = List.iter (Format.printf "%d ") (snd c) 
let _ = Format.printf "\n"




