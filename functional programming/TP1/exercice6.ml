let rec padovan n =
  match n with
  |0 -> 0
  |1 -> 0
  |2 -> 1
  |_ -> padovan n-2 + padovan n-3;;

  

let padovan2 n =
  let rec padovanterm pavo2 pavo3 p =
    if p=n then pavo2 + pavo3
    else padovanterm (pavo2+pavo3) pavo2 (P+1)
  in match n with
  |0 -> 0
  |1 -> 0
  |2 -> 1
  |_ -> padovanterm 1 0 3;;

