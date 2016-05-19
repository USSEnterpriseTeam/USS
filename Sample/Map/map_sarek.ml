open Spoc
open Skeleton

(* let a = generate (kern i ->  i +. 1.) 8388608;; *)
(* let x = map(kern a -> a *. 2.) a;; *)




let size = ref 256 in
while (!size <= 1048576 * 10) do
  let begin_time_gen = Unix.gettimeofday() in
  let a = Vector.create Vector.float32 !size in
  for i = 0 to Vector.length a - 1 do
    Spoc.Mem.set a i ((float_of_int i) +. 1.)
  done;
  let end_time_gen = Unix.gettimeofday() in
  let b = Vector.create Vector.float32 !size in
  for i = 0 to Vector.length a - 1 do
    Spoc.Mem.set b i ((Spoc.Mem.get a i) *. 2.)
  done;    
  let end_time = Unix.gettimeofday() in
  Printf.printf ("taille : %d\n") !size;
  Printf.printf ("temps skel gen = %f\n") (end_time_gen -. begin_time_gen);
  Printf.printf ("temps skel map = %f\n") (end_time -. end_time_gen);
  size := !size * 2
done;;
 








