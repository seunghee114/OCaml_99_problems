let drop input number =
   let rec answer ans = function
     | [] -> ans
     | e::l -> if List.length ans = number-1 then (List.rev ans) @ l 
               else answer (e::ans) l
   in answer [] input

let drop_sol input number =
  let rec aux i = function
    | [] -> []
    | h::t -> if i = number then aux 1 t else h::(aux (i+1) t)
  in aux 1 input

let drop_sol_fix input number =
  let rec aux i = function
    | [] -> []
    | h::t -> if i = number then t else h::(aux (i+1) t)
  in aux 1 input


let intList = [1; 2; 3; 4; 5; 6; 7]
let _ =
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  Format.printf "\n"
let _ =
  let _ = Format.printf "drop 3 : " in
  let _ = List.iter (Format.printf "%d ") (drop intList 3) in
  Format.printf "\n"

let _ =
  let _ = Format.printf "drop_sol 3 : " in
  let _ = List.iter (Format.printf "%d ") (drop_sol intList 3) in
  Format.printf "\n"

let _ =
  let _ = Format.printf "drop_sol_fix 3 : " in
  let _ = List.iter (Format.printf "%d ") (drop_sol_fix intList 3) in
  Format.printf "\n"




