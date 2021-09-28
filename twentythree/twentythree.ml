let rand_select input n =
  let rec extract acc n = function
    | [] -> raise Not_found
    | h::t -> if n = 0 then (h, acc @ t) else extract (h::acc) (n-1) t
  in
  let extract_rand input len =
    extract [] (Random.int len) input
  in
  let rec aux n acc input len =
    if n = 0 then acc 
    else let picked, rest = extract_rand input len 
    in aux (n-1) (picked :: acc) rest (len -1)
  in
  let len = List.length input 
  in aux (min n len) [] input len

let chList = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h'; 'i']
let _ = Format.printf "chList : "
let _ = List.iter(Format.printf "%c ") chList
let _ = Format.printf "\n"

let a = rand_select chList 3
let _ = List.iter(Format.printf "%c ") a
let _ = Format.printf "\n"






