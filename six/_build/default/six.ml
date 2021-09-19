let is_palindrome input = 
  input = List.rev input

let chList1 = ['x'; 'm'; 'a'; 'm'; 'x']
let chList2 = ['m'; 'a'; 'x']

let _ = Format.printf "chList1 " 
let _ = List.iter (Format.printf "%c ") chList1 
let _ = Format.printf "\nis_palindrome chList1 : %b\n" (is_palindrome chList1)
 
let _ = Format.printf "\nchList2 " 
let _ = List.iter (Format.printf "%c ") chList2 
let _ = Format.printf "\nis_palindrome chList2 : %b\n" (is_palindrome chList2)

 
