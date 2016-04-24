open Spoc
open Kirc

open Skeleton

let a = Vector.create Vector.float32 10;;
let b = Vector.create Vector.float32 10;;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 100. in
  let r2 = Random.float 100. in
  Spoc.Mem.set a i (r);
  Spoc.Mem.set b i (r2);
done;;

let a = map (kern a -> a +. 1.) a;;
let c = map2(kern (a) (b) -> a +. b) a b;;

for i = 0 to Vector.length a - 1 do
  Printf.printf "{%f, %f} -> " (Spoc.Mem.get a i) (Spoc.Mem.get b i);
  Printf.printf "%f \n" (Spoc.Mem.get c i);
done;;

