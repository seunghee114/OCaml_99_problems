type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode input =
  let rec repeat n d an =
    if n = 1 then d::an else repeat (n-1) d (d::an)
  in
  let rec decode_ans ans = function
    | [] -> ans 
    | One a :: l -> decode_ans (a::ans) l
    | Many (a, b):: l -> decode_ans (repeat a b ans) l
  in List.rev (decode_ans [] input)

let input = [Many (5, 'a'); Many(2, 'b'); One 'c'; Many(3, 'a')]
let _ = Format.printf "input : "
let _ = List.iter(fun x -> match x with 
                          | One a -> Format.printf "(1 %c) " a
                          | Many (a, b) -> Format.printf "(%d %c) " a b) input
let _ = Format.printf "\n"


let a = decode input
let _ = Format.printf "decode : "
let _ = List.iter(Format.printf "%c ") a
let _ = Format.printf "\n"

