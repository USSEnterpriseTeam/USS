open Spoc
open Skeleton

let _ =
  Random.self_init ();
  let a = Vector.create Vector.float64 50_000
  and b = Vector.create Vector.float64 50_000
  in
  
  for i = 0 to Vector.length a - 1 do
    Spoc.Mem.set a i ((Random.float 64.) -. 64.);
    Spoc.Mem.set b i ((Random.float 64. -. 64.));
  done;
  
  (* produit scalaire *)
  let c = reduce (kern a b -> ( Math.Float64.add a  b) )
    (map2 (kern a b -> Math.Float64.mul a b) a b )
  in
  
  Printf.printf "res = %f\n" c;

(*
  let a = Vector.create Vector.float32 50;;
  let b = Vector.create Vector.float32 50;;
  for i = 0 to Vector.length a - 1 do
  Spoc.Mem.set a i 1.;
  Spoc.Mem.set b i (float_of_int (Vector.length b - i))  
  done;;

  let c = map2Reduce (kern a b -> a +. b) (kern a b -> a +. b) a b;;
  Printf.printf "res= %f\n" c;;

*)
