let rec last_two = function
  | [] | [_] -> None
  | [x; y] -> Some (x, y)
  | _::l -> last_two l

let intList = [3; 6; 1; 4]
let strList = ["x"; "d"; "a"; "s"; "c"]
let chList = ['q'; 'r'; 'd']

let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  let _ = Format.printf "\n" in
  let _ = Format.printf "last two element of intList : " in
  let ie = last_two intList in
  match ie with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%d %d\n" (fst e) (snd e)

let _ = 
  let _ = Format.printf "strList : " in
  let _ = List.iter (Format.printf "%s ") strList in
  let _ = Format.printf "\n" in
  let _ = Format.printf "last two element of strList : " in
  let se = last_two strList in
  match se with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%s %s\n" (fst e) (snd e)

let _ = 
  let _ = Format.printf "chList : " in
  let _ = List.iter (Format.printf "%c ") chList in
  let _ = Format.printf "\n" in
  let _ = Format.printf "last two element of chList : " in
  let ce = last_two chList in
  match ce with
  | None -> Format.printf "None\n"
  | Some e -> Format.printf "%c %c\n" (fst e) (snd e)



