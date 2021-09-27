let lotto_select n range =
  let _ = Random.self_init() in
  let rec answer ans = 
    if List.length ans = n then ans else answer ((Random.int range)::ans)
  in answer []

let _ = 
  let _ = Format.printf "lotto_select 6 49 : " in
  let _ = List.iter (Format.printf "%d ") (lotto_select 6 49) in
  Format.printf "\n"
  
