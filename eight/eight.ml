let rec compress = function
  | a :: (b::_ as t) -> if a = b then compress t
                        else a :: compress t
  | smaller -> smaller

let intList = [3; 3; 3; 1; 2; 2; 1; 1; 1]
let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  Format.printf "\n"
let _ = 
  let a = compress intList in
  let _ = Format.printf "compress : " in
  let _ = List.iter (Format.printf "%d ") a in
  Format.printf "\n"
  


