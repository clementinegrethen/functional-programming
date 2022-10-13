let ex (x,y,z) i =
  match i with
  |1 --> x
  |2 --> y
  |3 -->z
  |_ --> failwith "erreur ";;

