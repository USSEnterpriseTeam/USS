open Spoc
open Skeleton


let a = generate(kern i ->  2 + i) 10;;

for i = 0 to Vector.length a - 1 do
  Printf.printf "%f, " (Spoc.Mem.get a i);
done;;

Printf.printf "\n";



 
