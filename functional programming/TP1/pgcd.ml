 (*Entrées: deux entiers *)
 (*Sorties: deux entiers *)
 (*Préconditions: a et b sont deux entiers*)
 (*Post condition: pgcd a b est un entier positif *)


let  rec pgcd a b = 
  let absa=abs a and absb=abs b in (*Déclaratio des variables abs a et absb *)
  match absb with 
  |x when x=absa -> absa
  |x when x>absa -> pgcd absa (absb-absa);
  |_  -> pgcd (absa-absb) absb;;

(*Tests*)
let%test _ = pgcd 4 6 = 2
let%test _ = pgcd (-4) 6 = 2
let%test _ = fact 5 = 120



