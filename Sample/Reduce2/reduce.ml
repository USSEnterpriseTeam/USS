open Spoc
open Kirc
open Skeleton

(* let total = kern a tmp n -> *)
(*   let open Std in *)
(*   let t = thread_idx_x in *)
(*   let start = 2 * block_idx_x * block_dim_x in *)
  
(*   (if(start + t) < n then *)
(*       tmp.[<t>] =. a.[<start + t>] *)
(*    else *)
(*       tmp.[<t>] =. 0. *)
(*   ); *)
(*   (if (start + block_dim_x + t) < n then *)
(*       tmp.[<block_dim_x + t>] =. a.[<start + block_dim_x + t>] *)
(*    else *)
(*       tmp.[<block_dim_x + t>] =. 0. *)
(*   ); *)
(*   let mutable stride = block_dim_x in *)
(*   stride := stride >> 1; *)
(* ;; *)


let a = Vector.create Vector.float32 500000;;

for i = 0 to Vector.length a - 1 do 
  Spoc.Mem.set a i 1.;
done;;

let c = reduce2 (kern a b -> (a +. b)) a;;

Printf.printf "res = %f\n" c;
