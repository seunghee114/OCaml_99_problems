type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode input =
  let rec encode_ans n ans = function
    | [] -> []
    | [a] -> if (n+1) > 1 then (ans @ [Many (n+1, a)])
             else (ans @ [One a])
    | a :: (b :: _ as l) -> if a = b then encode_ans (n+1) ans l
                            else 
                              if n+1 > 1 then encode_ans 0 (ans @ [Many (n+1, a)]) l
                              else encode_ans 0 (ans @ [One a]) l
  in encode_ans 0 [] input

let encode_sol input =
  let create_tuple cnt elem =
    if cnt = 1 then One elem else Many (cnt, elem) in
  let rec aux count acc = function
    | [] -> []
    | [x] -> (create_tuple (count+1) x)::acc
    |hd ::(snd :: _ as tl) -> if hd = snd then aux (count+1) acc tl
                              else aux 0 ((create_tuple (count+1) hd)::acc) tl 
  in List.rev (aux 0 [] input)

let chList = ['a'; 'a'; 'a'; 'a'; 'a'; 'b'; 'b'; 'c'; 'a'; 'a'; 'a']
let _ = Format.printf "chList : "
let _ = List.iter (Format.printf "%c " ) chList

let encoded = encode chList
let _ = Format.printf "\n\nencoded : \n"
let _ = List.iter(fun x -> match x with 
                          | One a -> Format.printf "(1 %c) " a
                          | Many (a, b) -> Format.printf "(%d %c) " a b) encoded
let _ = Format.printf "\n"

let encoded_sol = encode_sol chList
let _ = Format.printf "\nencoded_sol : \n"
let _ = List.iter(fun x -> match x with 
                          | One a -> Format.printf "(1 %c) " a
                          | Many (a, b) -> Format.printf "(%d %c) " a b) encoded
let _ = Format.printf "\n"




