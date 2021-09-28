let rec extract n input =
  if n <= 0 then [[]]
  else match input with
  | [] -> []
  | h::tl ->
      let with_h = List.map (fun l -> h::l) (extract (n-1) tl)
      in let without_h = extract n tl
in with_h @ without_h

let chList = ['a'; 'b'; 'c'; 'd']
let _ = Format.printf "chList : "
let _ = List.iter(Format.printf "%c ") chList
let _ = Format.printf "\n"

let a = extract 2 chList
let _ = Format.printf "extract 2 :\n"
let _ = List.iter(fun l -> List.iter (Format.printf "%c ") l; Format.printf "\n") a





