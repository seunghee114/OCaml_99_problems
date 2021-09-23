type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode input =
  let rle count x =
    if count = 0 then One x else Many (count + 1, x) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (rle count x)::acc
    |a::(b::_ as t) -> if a = b then aux (count+1) acc t
                       else aux 0 ((rle count a)::acc) t
  in List.rev (aux 0 [] input)
let chList = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'a'; 'a'; 'a']
let _ =
  let _ = Format.printf "chList : " in
  let _ = List.iter (Format.printf "%c ") chList in
  Format.printf "\n"

let encoded = encode chList
let _ = Format.printf "encoded : \n"
let _ = List.iter(fun x -> match x with 
                          | One a -> Format.printf "(1 %c) " a
                          | Many (a, b) -> Format.printf "(%d %c) " a b) encoded
let _ = Format.printf "\n"




