let slice input start finish =
  let rec answer n = function
    | [] -> []
    | e::l -> if finish = n then [e]
              else if start > n then answer (n+1) l
              else e::(answer (n+1) l)
  in answer 0 input

let slice_sol input i k =
  let rec take n = function
    | [] -> []
    | h::t -> if n = 0 then [] else h::(take (n-1) t)
  in 
  let rec drop n = function
    | [] -> []
    | _::t as l -> if n = 0 then l else drop (n-1) t
  in take (k-i+1) (drop i input)


let intList = [1; 2; 3; 4; 5; 6; 7; 8]
let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  Format.printf "\n"

let _ = 
  let _ = Format.printf "slice 2 5 : " in
  let _ = List.iter (Format.printf "%d ") (slice intList 2 5) in
  Format.printf "\n"

let _ = 
  let _ = Format.printf "slice_sol 2 5 : " in
  let _ = List.iter (Format.printf "%d ") (slice_sol intList 2 5) in
  Format.printf "\n"




