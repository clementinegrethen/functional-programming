(******************************************************************************)
(*                                                                            *)
(*      fonction de décomposition pour les chaînes de caractères              *)
(*                                                                            *)
(*   signature : decompose_chaine : string -> char list = <fun>               *)
(*                                                                            *)
(*   paramètre(s) : une chaîne de caractères                                  *)
(*   résultat     : la liste des caractères composant la chaîne paramètre     *)
(*                                                                            *)
(******************************************************************************)
val decompose_chaine : string -> char list 

(******************************************************************************)
(*                                                                            *)
(*      fonction de recomposition pour les chaînes de caractères              *)
(*                                                                            *)
(*   signature : recompose_chaine : char list -> string = <fun>               *)
(*                                                                            *)
(*   paramètre(s) : une liste de caractères                                   *)
(*   résultat     : la chaîne des caractères composant la liste paramètre     *)
(*                                                                            *)
(******************************************************************************)
val recompose_chaine : char list -> string

(******************************************************************************)
(*                                                                            *)
(*      fonction de décomposition pour les entiers                            *)
(*                                                                            *)
(*   signature : decompose_int : int -> int list = <fun>                      *)
(*                                                                            *)
(*   paramètre(s) : un entier                                                 *)
(*   résultat     : la liste des chiffres composant l'entier                  *)
(*                                                                            *)
(******************************************************************************)
val decompose_int : int -> int list 


(******************************************************************************)
(*                                                                            *)
(*      fonction de recomposition pour les entiers                            *)
(*                                                                            *)
(*   signature : recompose_int : int list -> int = <fun>                      *)
(*                                                                            *)
(*   paramètre(s) : une liste d'entiers                                       *)
(*   résultat     : entier "valant" la liste paramètre                        *)
(*   Exemple      : recompose_int [2;5;8] = 258                               *)
(*                  recompose_int [121;23;3] = 121233                         *)
(******************************************************************************)
val recompose_int : int list -> int