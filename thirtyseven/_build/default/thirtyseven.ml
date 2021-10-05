let factors n =
  let rec aux d n =
    if n = 1 then [] else
      if n mod d  = 0 then 
        match aux d (n/d) with
        | (h, n) :: t when h = d -> (h, n+1) :: t
        | l -> (d, 1) :: l
      else aux (d+1) n
  in aux 2 n

let phi_improved n =
  let fs = factors n in
  let rec mul i j =
    if j = 0 then 1 else (mul i (j-1)) * i
  in
  let rec aux = function
    | [] -> 1
    | (d, n) :: l -> (d-1) * (mul d (n-1)) * aux l
  in aux fs

let input = [315; 60; 46; 32]

let _ = List.iter(
  fun e -> let _ = Format.printf "phi_improved %d : " e in
           Format.printf "%d\n" (phi_improved e)) input

