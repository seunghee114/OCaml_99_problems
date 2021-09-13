let rec rev = function
  | [] -> []
  | e::l -> rev l @ [e]

let rev_tail input =
  let rec rev_ans = function
    | [] -> []
    | e::l -> rev_ans l @ [e]
  in rev_ans input

let rev_solution input = 
  let rec aux acc = function
    | [] -> acc
    | h::t -> aux (h::acc) t in
  aux [] input

let intList = [3; 2; 4; 6; 1; 5]
let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter( Format.printf "%d ") intList in
  Format.printf "\nrev_int : "

let rev_int = rev intList
let _ = List.iter (Format.printf "%d ") rev_int 

let _ = Format.printf "\nrev_tail_int : "
let rev_tail_int = rev_tail intList
let _ = List.iter (Format.printf "%d ") rev_tail_int 

let _ = Format.printf "\nrev_solution_int : "
let rev_solution_int = rev_solution intList
let _ = List.iter (Format.printf "%d ") rev_solution_int 
let _ = Format.printf "\n"

