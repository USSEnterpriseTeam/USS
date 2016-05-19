open Spoc
open Skeleton

let a = Vector.create Vector.float32 17;;
for i = 0 to Vector.length a - 1 do
  Spoc.Mem.set a i ((float_of_int i) +. 1.)
done;;

let b = stencil (kern a b -> a +. b) a 2;;
for i = 0 to Vector.length b - 1 do
  Printf.printf "%f\n" (Spoc.Mem.get b i)
done;;
  
