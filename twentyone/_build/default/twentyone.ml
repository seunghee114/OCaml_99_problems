let rec insert_at data idx = function
  | [] -> [data]
  | e::l as t -> if idx = 0 then data::t else e::(insert_at data (idx -1) l)

let strList = ["a"; "b"; "c"; "d"; "e"; "f"]
let _ = 
  let _ = Format.printf "strList : " in
  let _ = List.iter (Format.printf "%s ") strList in
  Format.printf "\n"

let _ = 
  let _ = Format.printf "insert_at hi 1 : " in
  let _ = List.iter (Format.printf "%s ") (insert_at "hi" 1 strList) in
  Format.printf "\n"

