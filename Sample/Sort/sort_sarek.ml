open Spoc
open Skeleton


let merge a p q = 
  let d = 1 lsl (p - q) in
  for i = 0 to Vector.length a - 1 do
    let up = ((i lsr p) land 2) == 0 in
    if ((i land d) == 0) && ((Spoc.Mem.get a i) > (Spoc.Mem.get a (i lor d))) == up then
      (
	let t = Spoc.Mem.get a i in
	(Spoc.Mem.set a i (Spoc.Mem.get a (i lor d)));
	(Spoc.Mem.set a (i lor d) (t))
      )
  done
;;

let bitonic_sort logn a =
  for i = 0 to logn - 1 do
    for j = 0 to i do
      merge a i j
    done
  done
;;




let a = Vector.create Vector.float32 (16777216);;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 1000. in
  Spoc.Mem.set a i (r)
done;;

(* sort(kern a b -> if (a >. b) then 1. else 0.) a;; *)
let begin_time = Unix.gettimeofday();; 
let logn = 4 in
bitonic_sort logn a;;
let end_time = Unix.gettimeofday();;
Printf.printf ("time %f\n") (end_time -. begin_time);;





