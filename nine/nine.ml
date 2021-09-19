let pack input = 
  let rec pack_ans ans current = function
    | [] -> []
    | [a] -> ans @ [a::current]
    | a :: (b :: _ as t) ->  if a = b then pack_ans ans (a :: current) t
                             else pack_ans (ans @ [a :: current]) [] t
  in pack_ans [] [] input


let pack_solution input =
  let rec aux current acc = function
    | [] -> []
    | [x] -> (x :: current) :: acc
    | a :: (b :: _ as t) -> if a = b then aux (a :: current) acc t
                            else aux [] ((a :: current) :: acc) t 
  in List.rev (aux [] [] input)
