let encode input =
  let rec encode_ans n ans = function
    | [] -> []
    | [a] -> ans @ [(n+1, a)]
    | a:: (b :: _ as l) -> if a = b then encode_ans (n+1) ans l
    else encode_ans 0 (ans @ [(n+1, a)]) l
  in encode_ans 0 [] input

let encode_solution input = 
  let rec aux count acc = function
    | [] -> []
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (count+1) acc t
                            else aux 0 ((count+1, a) :: acc) t 
  in List.rev (aux 0 [] input)

let chList = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'a'; 'a'; 'a']
let _ = Format.printf "chList : "
let _ = List.iter (Format.printf "%c " ) chList

let encoded = encode chList
let _ = Format.printf "\n\nencoded : \n"
let _ = List.iter(fun x -> Format.printf "(%d, %c) " (fst x) (snd x)) encoded
let _ = Format.printf "\n"

let encoded_sol = encode_solution chList
let _ = Format.printf "\nencoded_sol : \n"
let _ = List.iter(fun x -> Format.printf "(%d, %c) " (fst x) (snd x)) encoded_sol
let _ = Format.printf "\n"

