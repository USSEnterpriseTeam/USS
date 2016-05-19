open Spoc
open Skeleton

let a = Vector.create Vector.float32 (4096 * 4096);;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 1000. in
  Spoc.Mem.set a i (r)
done;;
sort(kern a b -> if (a >. b) then 1. else 0.) a;;
     
