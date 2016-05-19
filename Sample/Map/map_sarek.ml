open Spoc
open Kirc

open Skeleton

let a = Vector.create Vector.float32 1000000;;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 100. in
  Spoc.Mem.set a i (r);
done;;

let begin_exec_time = Unix.gettimeofday() in
let x = map2 (kern a b -> a +. b) a a in
let end_exec_time = Unix.gettimeofday() in
Printf.printf "Time %f" (end_exec_time -. begin_exec_time);;
(*
for i = 0 to Vector.length x - 1 do
  Printf.printf "%f -> %f\n" (Spoc.Mem.get a i) (Spoc.Mem.get x i);
done
*)    
