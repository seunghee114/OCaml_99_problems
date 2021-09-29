let length_sort input =
  let rec idx n ans = function
    | [] -> ans
    | l::t -> idx (n+1) ((List.length l, n )::ans) t
  in let sorted = List.sort compare (idx 0 [] input) in
  let rec get n = function
    | [] -> []
    | e::l -> if n = 0 then e else get (n-1) l
  in
  let rec ans acc = function
    | [] -> acc
    | (_, b) :: l -> ans ((get b input)::acc) l
  in List.rev (ans [] sorted)

let frequency_sort input =
  let len = List.sort compare (List.map(fun l -> List.length l) input) in
  let rec frequency ans n = function
    | [] -> ans
    | [a] -> (n+1, a)::ans
    | b::(c::_ as t) -> if b = c then frequency ans (n+1) t
                        else frequency ((n+1, b) :: ans) 0 t
  in
  let freq = List.sort compare (frequency [] 0 len) in
  let rec ans acc = function
    | [] -> acc
    | (_, l)::t -> ans (acc @(List.filter(fun k -> List.length k = l) input)) t
  in ans [] freq



let input = [['a'; 'b'; 'c']; ['d'; 'e']; ['f'; 'g'; 'h']; ['d';'e']; ['i';'j';'k';'l']; ['m';'n']; ['o']]
let _ = Format.printf "input : "
let _ = List.iter(fun l -> let _ = Format.printf "[ " in
                           let _ = List.iter(Format.printf "%c ") l in
                           Format.printf "]") input
let _ = Format.printf "\n\n"

let len_sorted = length_sort input

let _ = Format.printf "len_sorted : "
let _ = List.iter(fun l -> let _ = Format.printf "[ " in
                           let _ = List.iter(Format.printf "%c ") l in
                           Format.printf "]") len_sorted
let _ = Format.printf "\n"

let fre_sorted = frequency_sort input

let _ = Format.printf "fre_sorted : "
let _ = List.iter(fun l -> let _ = Format.printf "[ " in
                           let _ = List.iter(Format.printf "%c ") l in
                           Format.printf "]") fre_sorted
let _ = Format.printf "\n"

let rec insert cmp e = function
  | [] -> [e]
  | h::t as l -> if cmp e h <= 0 then e::l else h::(insert cmp e t)

let rec sort cmp = function
  | [] -> []
  | h::t -> insert cmp h (sort cmp t)

let length_sort_sol input = 
  let input = List.map(fun l -> List.length l, l) input in
  let input = sort (fun a b -> compare (fst a) (fst b)) input in List.map snd input

let rle input =
  let rec aux count acc = function
    | [] -> []
    | [x] -> (x, count+1)::acc
    | a::(b::_ as t) -> if a = b then aux (count+1) acc t
                        else aux 0 ((a, count+1)::acc) t 
  in aux 0 [] input

let frequency_sort_sol input = 
  let lengths = List.map List.length input in
  let freq = rle (sort compare lengths) in
  let by_freq = List.map(fun l -> List.assoc (List.length l) freq, l) input in
  let sorted = sort (fun a b -> compare (fst a) (fst b)) by_freq in
  List.map snd sorted

let len_sorted_sol = length_sort_sol input
let _ = Format.printf "\nlen_sorted_sol : "
let _ = List.iter(fun l -> let _ = Format.printf "[ " in
                           let _ = List.iter(Format.printf "%c ") l in
                           Format.printf "]") len_sorted_sol
let _ = Format.printf "\n"

let fre_sorted_sol = frequency_sort_sol input

let _ = Format.printf "fre_sorted_sol : "
let _ = List.iter(fun l -> let _ = Format.printf "[ " in
                           let _ = List.iter(Format.printf "%c ") l in
                           Format.printf "]") fre_sorted_sol
let _ = Format.printf "\n"





