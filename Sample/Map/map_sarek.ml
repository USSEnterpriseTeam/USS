open Spoc
open Kirc

open Skeleton
open Transform

let a = Vector.create Vector.float64 10;;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 100. in
  Spoc.Mem.set a i (r);
done;;

let b = mapTest(kern a -> a *. 1.) a;;

for i = 0 to Vector.length a - 1 do
  Printf.printf "%f -> " (Spoc.Mem.get a i);
  Printf.printf "%f \n" (Spoc.Mem.get b i);
done;;
