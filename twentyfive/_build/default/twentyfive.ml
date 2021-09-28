let permutation input =
  let rec extract acc n = function
    | [] -> raise Not_found
    | e::l -> if n = 0 then (e, acc @ l) else extract (e::acc) (n-1) l
  in 
  let extract_rand input len =
    extract [] (Random.int len) input
  in
  let rec aux acc input len =
    if (List.length input) = 0 then acc
    else 
      let picked, rest = extract_rand input len
      in aux (picked::acc) rest (len-1)
  in
  let len = List.length input
  in aux [] input len

let chList = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i']
let _ = Format.printf "chList : "
let _ = List.iter(Format.printf "%c ") chList
let _ = Format.printf "\n"

let a = permutation chList 
let _ = List.iter(Format.printf "%c ") a
let _ = Format.printf "\n"






