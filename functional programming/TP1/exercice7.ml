let est_premier n =
  let rec aux i =
      if i = n then true 
      else if n mod i = 0 then false
      else aux (i+1)
  in 
if n = 1 then false else aux 2;;