open Spoc
open Kirc

open Skeleton

let a = Vector.create Vector.float32 10;;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 100. in
  Spoc.Mem.set a i (r);
done;;

let b = map2 (kern a b -> a +. b +. 1.) a a;;

match b with
| Skeleton.VRetour (x) ->
   for i = 0 to Vector.length x - 1 do
     Printf.printf "%f -> %f\n" (Spoc.Mem.get a i) (Spoc.Mem.get x i);
   done
| Skeleton.RNull ->
   Printf.printf "cette fonction ne retourne rien"
       
