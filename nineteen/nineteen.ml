let rotate input num =
  let rec answer ans n = function
    | [] -> []
    | e::l as t -> if n > 0 then answer (e::ans) (n-1) l
                   else t @ (List.rev ans)
  in
  if num < 0 then answer [] (List.length input + num) input
  else answer [] num input

let split input n =
  let rec aux i acc = function
    | [] -> List.rev acc , []
    | h::t as l -> if i = 0 then List.rev acc , l else aux (i-1) (h::acc) t
  in aux n [] input

let rotate_sol input n =
  let len = List.length input in
  let n = if len = 0 then 0 else (n mod len + len) mod len 
  in 
  if n = 0 then input
  else let a, b = split input n 
  in b @ a

let chList = ['a'; 'b'; 'c'; 'd'; 'e'; 'f'; 'g'; 'h']
let _ = 
  let _ = Format.printf "chList : " in
  let _ = List.iter (Format.printf "%c ") chList in
  Format.printf "\n"

let _ = 
  let a = rotate chList 3 in
  let _ = Format.printf "rotate 3 : " in
  let _ = List.iter (Format.printf "%c ") a in
  Format.printf "\n"

let _ = 
  let b = rotate chList (-2) in
  let _ = Format.printf "rotate -2 : " in
  let _ = List.iter (Format.printf "%c ") b in
  Format.printf "\n"

let _ = 
  let d = rotate_sol chList 3 in
  let _ = Format.printf "rotate_sol 3 : " in
  let _ = List.iter (Format.printf "%c ") d in
  Format.printf "\n"

let _ = 
  let c = rotate_sol chList (-2) in
  let _ = Format.printf "rotate_sol -2 : " in
  let _ = List.iter (Format.printf "%c ") c in
  Format.printf "\n"


