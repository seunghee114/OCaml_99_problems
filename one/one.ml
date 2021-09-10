let rec last = function
  | [] -> None
  | e::l -> if List.length l = 0 then Some e else last l

let rec last_solution = function
  | [] -> None
  | [e] -> Some e
  | _::l -> last_solution l

let intList = [3; 6; 1; 4]
let strList = ["x"; "d"; "a"; "s"; "c"]
let chList = ['q'; 'r'; 'd']

let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  let _ = Format.printf "\n" in
  let _ = Format.printf "last element of intList : " in
  let ie = last intList in
  match ie with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%d\n" e

let _ = 
  let _ = Format.printf "strList : " in
  let _ = List.iter (Format.printf "%s ") strList in
  let _ = Format.printf "\n" in
  let _ = Format.printf "last element of strList : " in
  let se = last strList in
  match se with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%s\n" e

let _ = 
  let _ = Format.printf "chList : " in
  let _ = List.iter (Format.printf "%c ") chList in
  let _ = Format.printf "\n" in
  let _ = Format.printf "last element of chList : " in
  let ce = last chList in
  match ce with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%c\n" e



