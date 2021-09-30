let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let phi n = 
  let rec ans i count =
    if i = n then count
    else
      if (gcd n i) = 1 then ans (i+1) (count+1) else ans (i+1) count
  in ans 1 0
let coprime a b = gcd a b  = 1
let phi_sol n =
  let rec count_coprime acc d =
    if d < n then count_coprime(if coprime n d then acc+1 else acc) (d+1)
    else acc
  in if n = 1 then 1 else count_coprime 0 1

let input = [7; 10; 13; 37; 52]
let _ = List.iter(
  fun i -> Format.printf "phi %d : %d\n" i (phi i)) input
let _ = Format.printf "\n"
let _ = List.iter(
  fun i -> Format.printf "phi_sol %d : %d\n" i (phi_sol i)) input






