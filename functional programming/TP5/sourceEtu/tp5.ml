(* Evaluation des expressions simples *)


(* Module abstrayant les expressions *)
module type ExprSimple =
sig
  type t
  val const : int -> t
  val plus : t -> t -> t
  val mult : t-> t -> t
end

(* Module réalisant l'évaluation d'une expression *)
module EvalSimple : ExprSimple with type t = int =
struct
  type t = int
  let const c = c
  let plus e1 e2 = e1 + e2
  let mult e1 e2 = e1 * e2
end


(* Solution 1 pour tester *)
(* A l'aide de foncteur *)

(* Définition des expressions *)
module ExemplesSimples (E:ExprSimple) =
struct
  (* 1+(2*3) *)
  let exemple1  = E.(plus (const 1) (mult (const 2) (const 3)) )
  (* (5+2)*(2*3) *)
  let exemple2 =  E.(mult (plus (const 5) (const 2)) (mult (const 2) (const 3)) )
end

(* Module d'évaluation des exemples *)
module EvalExemples =  ExemplesSimples (EvalSimple)

let%test _ = (EvalExemples.exemple1 = 7)
let%test _ = (EvalExemples.exemple2 = 42)

module PrintSimple : ExprSimple with type t = string =
struct
  type t = string
  let const c = string_of_int c
  let plus e1 e2 = "("^e1^"+"^e2^")"
  let mult e1 e2 = "("^e1^"*"^e2^")"
end


module CompteSimple : ExprSimple with type t = int  =
struct
  type t = int 
  let const c = 1
  let plus e1 e2 = e1 + e2 + 1
  let mult e1 e2 = e1 + e2 + 1
end


module type ExprVar =
sig
  type t
  val def : string ->t->t->t
  val var: string->t
end


module type Expr =
struct
  include (ExprSimple)
  include(ExprVar with t:=t)

end

module PrintVar : ExprVar with type t = string =
struct

  type t = string
  let def str e1 e2 = "let "^str^" = "^e1^" in "^e2
  let var str = str
end

module type Print =
struct
  include (PrintSimple)
  include(PrintVar )

end
type tenv = (string*int) list ;;

module EvalVar : ExprVar with type t=int=
struct
  type t =int
  val def str e1 e2 = 