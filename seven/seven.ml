type 'a node =
  | One of 'a
  | Many of 'a node list

let flat input = 
  let rec ans acc = function
    | [] -> acc
    | One x::r -> ans (acc @ [x]) r
    | Many l::r -> ans (ans acc l) r
  in ans [] input

let flatten input = 
  let rec ans acc = function
    | [] -> acc
    | One x::r -> ans (x :: acc) r
    | Many l::r -> ans (ans acc l) r
  in List.rev (ans [] input)
 
let intList = [ One 1; Many[One 2; One 4; Many[ One 7; One 3;]]; One 5]
let answer = flatten intList
let _ = Format.printf "flatten : "
let _ = List.iter (Format.printf "%d ") answer
let _ = Format.printf "\n"

let answer1 = flat intList
let _ = Format.printf "flat : "
let _ = List.iter (Format.printf "%d ") answer1
let _ = Format.printf "\n"

