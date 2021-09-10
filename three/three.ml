let rec at n l = 
  match l with
  | [] -> None
  | e::r -> 
      if List.length l < n then None
      else
        if n = 1 then Some e
        else at (n-1) r

let rec at_solution n = function
  | [] -> None
  | e::r -> if n = 1 then Some e else at_solution (n-1) r


let intList = [8; 2; 9; 4; 5]

let _ = Format.printf "intList : "
let _ = List.iter (Format.printf "%d ") intList
let _ = Format.printf "\n\nuse at\n"

let _ = 
  let _ = Format.printf "The 3rd element of B : " in
  let a = at 3 intList in
  match a with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%d\n" e

let _ = 
  let _ = Format.printf "The 6th element of B : " in
  let a = at 6 intList in
  match a with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%d\n" e

let _ = Format.printf "\nuse_solution at\n"
let _ = 
  let _ = Format.printf "The 3rd element of B : " in
  let a = at_solution 3 intList in
  match a with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%d\n" e

let _ = 
  let _ = Format.printf "The 6th element of B : " in
  let a = at_solution 6 intList in
  match a with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%d\n" e


let _ = Format.printf "\nuse List.nth\n"
let _ = 
  Format.printf "List.nth list 2 : %d\n" (List.nth intList 2)

