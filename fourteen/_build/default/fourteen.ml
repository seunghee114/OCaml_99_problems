let duplicate input =
  let rec answer ans = function
    | [] -> ans
    | e::l -> answer (e::(e::ans)) l
  in List.rev(answer [] input)

let rec duplicate_sol = function
  | [] -> []
  | e::l -> e::e::(duplicate_sol l)

let intList = [1; 2; 3; 3; 4]
let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  Format.printf "\n"

let _ =
  let _= Format.printf "duplicate : " in
  let _ = List.iter (Format.printf "%d ") (duplicate intList) in
  Format.printf "\n"
  
let _ =
  let _= Format.printf "duplicate_sol : " in
  let _ = List.iter (Format.printf "%d ") (duplicate_sol intList) in
  Format.printf "\n"
 


