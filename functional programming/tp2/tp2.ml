(******* TRIS ******)

(*  Tri par insertion **)

(*CONTRAT
Fonction qui ajoute un élément dans une liste triée, selon un ordre donné
Type : ('a->'a->bool)->'a->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : elt, l'élement à ajouter
Paramètre : l, la liste triée dans laquelle ajouter elt
Résultat : une liste triée avec les éléments de l, plus elt
*)

let rec insert ordre elt l = 
  match l with
  |[]->[e]
  |t::q when ordre elt t -> elt::l
  |_::q -> t::insert elt q;;



(* TESTS *)
let%test _ = insert (fun x y -> x<y) 3 []=[3]
let%test _ = insert (fun x y -> x<y) 3 [2;4;5]=[2;3;4;5]
let%test _ = insert (fun x y -> x > y) 6 [3;2;1]=[6;3;2;1]



(*CONTRAT
Fonction qui trie une liste, selon un ordre donné
Type : ('a->'a->bool)->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l, la liste à trier
Résultat : une liste triée avec les éléments de l
*)

let rec tri_insertion ordre l = 
  match l with  
  |[]->[]
  |t::q-> insert ordre t (tri_insertion ordre q) ;;



(* TESTS *)
let%test _ = tri_insertion (fun x y -> x<y) [] =[]
let%test _ = tri_insertion (fun x y -> x<y) [4;2;4;3;1] =[1;2;3;4;4]
let%test _ = tri_insertion (fun x y -> x > y) [4;7;2;4;1;2;2;7]=[7;7;4;4;2;2;2;1]


(*  Tri fusion **)

(* CONTRAT
Fonction qui décompose une liste en deux listes de tailles égales à plus ou moins un élément
Paramètre : l, la liste à couper en deux
Retour : deux listes
*)

let rec scinde l =  
  match l with
  |[]->[];
  |[_]->(l,[])
  |t1::t2::q -> let (l1,l2)=scinde q in (a::l1,b::l2);;


(* TESTS *)
(* Peuvent être modifiés selon l'algorithme choisi *)
let%test _ = scinde [1;2;3;4] = ([1;3],[2;4])
let%test _ = scinde [1;2;3] = ([1;3],[2])
let%test _ = scinde [1] = ([1],[])
let%test _ = scinde [] = ([],[])


(* Fusionne deux listes triées pour en faire une seule triée
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l1 et l2, les deux listes triées
Résultat : une liste triée avec les éléments de l1 et l2
*)

let rec fusionne ordre l1 l2 = 
  match (l1,l2) with 
  |([],_)->l2;
  |(_,[])->l1;
  |(t1::q1,t2::q2) when ordre t1 t2 -> t1::(fusionne ordre q1,l2)
  |(t1::q1,t2::q2) -> t2::(fusionne ordre q2,l1);;

  

(*TESTS*)
let%test _ = fusionne (fun x y -> x<y) [1;2;4;5;6] [3;4] = [1;2;3;4;4;5;6]
let%test _ = fusionne (fun x y -> x<y) [1;2;4] [3;4] = [1;2;3;4;4]
let%test _ = fusionne (fun x y -> x<y) [1;2;4] [3;4;8;9;10] = [1;2;3;4;4;8;9;10]
let%test _ = fusionne (fun x y -> x<y) [] [] = []
let%test _ = fusionne (fun x y -> x<y) [1] [] = [1]
let%test _ = fusionne (fun x y -> x<y) [] [1] = [1]
let%test _ = fusionne (fun x y -> x<y) [1] [2] = [1;2]
let%test _ = fusionne (fun x y -> x>y) [1] [2] = [2;1]


(* CONTRAT
Fonction qui trie une liste, selon un ordre donné
Type : ('a->'a->bool)->'a list -> 'a list
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l, la liste à trier
Résultat : une liste triée avec les éléments de l
*)

let rec tri_fusion ordre l =
  match l with
  |[]->[]
  |_-> let (l1,l2) = scinde l in fusionne ordre (tri_fusion ordre l1) (tri_fusion ordre l2 );;



(* TESTS *)
let%test _ = tri_fusion (fun x y -> x<y) [] =[]
let%test _ = tri_fusion (fun x y -> x<y) [4;2;4;3;1] =[1;2;3;4;4]
let%test _ = tri_fusion (fun x y -> x > y) [4;7;2;4;1;2;2;7]=[7;7;4;4;2;2;2;1]


(*  Parsing du fichier *)
open Lexing

(* Affiche un quadruplet composé 
- du sexe des personnes ayant reçu ce prénom : 1 pour les hommes, 2 pour les femmes
- du prénom
- de l'année
- du nombre de fois où ce prénom a été donné cette année là
*)
let print_stat (sexe,nom,annee,nb) =
  Printf.eprintf "%s,%s,%d,%d%!\n" (if (sexe=1) then "M" else "F") nom annee nb

(* Analyse le fichier nat2016.txt (stratistique des prénoms entre 1900 et 2016) 
 et construit une liste de quadruplet (sexe,prénom,année,nombre d'affectation)
*)
let listStat = 
  let input = open_in "/mnt/n7fs/ens/tp_guivarch/pf/nat2016.txt" in 
  let filebuf = Lexing.from_channel input in
  Parser.main Lexer.token filebuf
  

(* Analyse le fichier nathomme2016.txt (stratistique des prénoms d'homme commençant par un A ou un B entre 1900 et 2016) 
 et construit une liste de quadruplets (sexe,prénom,année,nombre d'affectations)
*)
let listStatHomme = 
  let input = open_in "/mnt/n7fs/ens/tp_guivarch/pf/nathomme2016.txt" in
  let filebuf = Lexing.from_channel input in
  Parser.main Lexer.token filebuf

  let ordre1 (,_,_,_,x) (,_,_,_,y) = x>y;;

  let tristathomme_insertion = tri_insertion ordre 1 listStatHomme;;

  let tristathomme_fusion = tri_fusion ordre 1 listStatHomme;;
(*Exercice 8 : terminaison de l'algo de tri_fusion*)

  (* CONTRAT
Fonction qui décompose une liste en deux listes de tailles égales à plus ou moins un élément
Paramètre : l, la liste à couper en deux
Retour : deux listes
*)

let  scinde_term l =  
  let n = List.length l in
  let rec scinde_term_ aux acc l1 l2 =
    if acc = 0 then (l1,l2)
    else scinde_term_aux (acc-1) (List.hd l2::l1) (List.tl l1)
  in scinde_term_aux [n/2] [] l;;


let%test _ = scinde_term [1;2;3;4] = ([1;3],[2;4])
let%test _ = scinde_term [1;2;3] = ([1;3],[2])
let%test _ = scinde_term [1] = ([1],[])
let%test _ = scinde_term [] = ([],[])

(* Fusionne deux listes triées pour en faire une seule triée
Paramètre : ordre  ('a->'a->bool), un ordre sur les éléments de la liste
Paramètre : l1 et l2, les deux listes triées
Résultat : une liste triée avec les éléments de l1 et l2
*)

let rec fusionne_term  ordre l1 l2 = 
  let n =min(List.length l1, List.length l2) in 
  let rec fusionne_term_aux acc l  l1 l2 =
    if acc = 0 then l 
    if ordre List.hd l2 List.hd l1 then fusionne_term_aux (acc-1) (List.hd l1 :: l ) (List.tl l1) (l2)
    else fusionne_term_aux (acc-1) (List.hd l2::l) (l1) (List.tl l2)
  in fusionne_term_aux n [] l1 l2;;


  
  (* Sélectionne les n premiers élements d'une liste 
Paramètre : n un entier positif 
Paramètre : l une liste 
Résultat : la liste l réduite à ses n premiers élements ( si la longueur de la liste est inférieur à n on garde la liste complète)
*)

let select n l = 
  let select_aux aux1 aux2 l =
    if aux1=n || aux2=l then aux2
    else select_aux (aux1+1) (aux2@[List.hd l] ) (List.tl l )
  in select_aux 0 [] l ;;
  

