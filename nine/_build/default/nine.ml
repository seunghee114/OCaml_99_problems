let pack input = 
  let rec pack_ans ans current = function
    | [] -> []
    | [a] -> ans @ [a::current]
    | a :: (b :: _ as t) ->  if a = b then pack_ans ans (a :: current) t
                             else pack_ans (ans @ [a :: current]) [] t
  in pack_ans [] [] input

let intList = [1; 1; 1; 2; 3; 3; 3; 3; 4; 4;]

let _ = 
  let _ = Format.printf "intList : " in
  let _ = List.iter (Format.printf "%d ") intList in
  Format.printf "\n"

let a = pack intList
let _ =
  List.iter (Format.printf "\nlist : ";
             List.iter (Format.printf "%d ")) a



