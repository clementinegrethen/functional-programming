
module type Regle =
sig
  type tid = int
  type td
  val id : tid
  val init : td -> td list
  val appliquer : td -> td list
end



module type ArbreReecriture =
sig
  (*
  type tid = int
  type td
  type arbre_reecriture = ...

  val creer_noeud : ...

  val racine : ...
  val fils : ..

  val appartient : td -> arbre_reecriture -> bool
  *)
end

let retract =fun p ->
  let n=List.length p and   i= ref 1 and l=ref [] in let  lg=ref n  in 
  while  (List.nth p (!lg-1) ==(0.)) &&  (!i <n) do
    i:=!i+1;
    lg:=!lg-1;
  sdone;
  for j=(n-(!i)) downto 0 do
    l:= (List.nth p j)::!l
  done;
  !l