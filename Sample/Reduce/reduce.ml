open Spoc
open Kirc

open Skeleton


(*let reduce = kern a n ->
  let open Std in
  let open Math.Float32 in
  let idx = global_thread_id in
  let mutable n2 = n_local in
  let mutable n3 = n_local in
  let mutable pos = 0 in

  while n3 > 0 do
    n3 := n2 / 2;
    (if (n2 mod 2) = 0 then
      n2 := n3 + 1;
    else
      n2 := n3;
    );
    if idx < n2 then
      if (((n2 < n3) || (n2 > n3)) && (idx > 0)) then
        pos := idx + n3;
      else if n2 = n3 then
        pos := idx + n2;
  done

  b[0] += tmp[0]
;;



Kirc.gen reduce;;
*)
let a = Vector.create Vector.float32 200;;

for i = 0 to Vector.length a - 1 do 
  Spoc.Mem.set a i 1.;
done;;

let c = reduce (kern a b -> a *. b) a;;

Printf.printf "res = %f\n" c;;

(*for i = 0 to Vector.length a - 1 do
  Printf.printf "%f" (Spoc.Mem.get a i)
done;;
Printf.printf "\n";;*)

