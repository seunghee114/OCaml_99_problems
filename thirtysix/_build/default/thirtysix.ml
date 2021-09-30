let is_prime n = 
  let n  = abs n in
  let rec is_not_divisor d = 
    d*d > n || (n mod d <> 0 && is_not_divisor (d+1))
  in n<>1 &&is_not_divisor 2

let encode input =
  let rec ans count acc = function
    | [] -> []
    | [x] -> (count+1, x) :: acc
    | a :: (b :: _ as t ) -> if a = b then ans (count+1) acc t
                             else ans 0 ((count+1, a) :: acc) t
  in List.rev (ans 0 [] input)

let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d = 0 then d::(aux d (n/d)) else aux (d+1) n 
  in encode (aux 2 n)

let factors_sol n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d  = 0 then 
        match aux d (n/d) with
        | (h, n) :: t when h = d -> (h, n+1) :: t
        | l -> (d, 1) :: l
      else aux (d+1) n
  in aux 2 n


let input = [315; 60; 46; 32]

let _ = List.iter(
  fun e -> let _ = Format.printf "factor %d : " e in
           let _ = List.iter(
             fun l -> Format.printf "(%d %d) " (snd l) (fst l)) (factors e) in
           Format.printf "\n") input

let _ = Format.printf "\n"

let _ = List.iter(
  fun e -> let _ = Format.printf "factor_sol %d : " e in
           let _ = List.iter(
             fun l -> Format.printf "(%d %d) " (snd l) (fst l)) (factors_sol e) in
           Format.printf "\n") input





