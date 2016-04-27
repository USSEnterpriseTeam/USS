open Spoc
open Kirc

open Skeleton

(*
let reduce = kern a n ->
  let open Std in
  let open Math.Float32 in
  let idx = global_thread_id in
  let mutable n2 = n in
  let mutable n3 = 0 in
  let mutable pos = 0 in

  a.[<idx>] <- 1.;

  for i = 0 to 4 do
    n3 := n2 / 2;
    (if (n2 mod 2) = 0 then
      n2 := n3 + 1;
    else
      n2 := n3;
    );
    if idx < n2 then
      if ((n2 < n3) || (n2 > n3)) && (idx > 0) then
        pos := idx + n3;
      else
        pos := idx + n2;
  done
;;

*)

(*Kirc.gen reduce;;*)

let a = Vector.create Vector.float32 10;;

for i = 0 to Vector.length a - 1 do 
  Spoc.Mem.set a i 1.;
done;;

let a = reduce (kern a -> a +. 1.) a;;