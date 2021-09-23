let replicate input number =
  let rec repeat n d ans =
    if n = 1 then d::ans else repeat (n-1) d (d::ans)
  in
  let rec answer ans = function
    | [] -> ans
    | e::l -> answer (repeat number e ans) l
  in List.rev(answer [] input)

let chList = ['a'; 'b'; 'c']
let _ =
  let _ = Format.printf "chList : " in
  let _ = List.iter (Format.printf "%c ") chList in
  Format.printf "\n"

let _ =
  let _ = Format.printf "replicate : " in
  let _ = List.iter (Format.printf "%c ") (replicate chList 3) in
  Format.printf "\n"




