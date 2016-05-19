open Spoc
open Skeleton

let a = Vector.create Vector.float32 32;;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 100. in
  Printf.printf "%f, " r;
  Spoc.Mem.set a i (r)
done;;
Printf.printf "\n";;
sort(kern a b -> if (a >. b) then 0. else 1.) a;;

for i = 0 to Vector.length a - 1 do
  Printf.printf "%f, " (Spoc.Mem.get a i)
done;;
Printf.printf "\n";;
