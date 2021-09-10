let rec length answer = function
  | [] -> answer
  | _::l -> length (answer+1) l

let length_solution l =
  let rec aux n = function
    | [] -> n
    | _::t -> aux (n+1) t
  in aux 0 l

let intList = [3; 2; 6; 4; 8; 0; 5;]
let strList = ["ab"; "cd"; "ef"]
let chList = ['w'; 'e'; 'g'; 's']

let _ = 
  let _ = Format.printf "intList : " in
  List.iter (Format.printf "%d ") intList

let _ = 
  let _ = Format.printf "\nstrList : " in
  List.iter (Format.printf "%s ") strList

let _ = 
  let _ = Format.printf "\nchList : " in
  List.iter (Format.printf "%c ") chList


let _ = Format.printf "\n\nuse length\n" 
let _ = Format.printf "intList length : %d\n" (length 0 intList)
let _ = Format.printf "strList length : %d\n" (length 0 strList)
let _ = Format.printf "chList length : %d\n" (length 0 chList)

let _ = Format.printf "\nuse length_solution\n" 
let _ = Format.printf "intList length : %d\n" (length_solution intList)
let _ = Format.printf "strList length : %d\n" (length_solution strList)
let _ = Format.printf "chList length : %d\n" (length_solution chList)


