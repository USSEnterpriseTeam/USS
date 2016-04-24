open Spoc
open Kirc

let reduce = kern a n ->
  let open Std in
  let open Math.Float32 in
  let idx = global_thread_id in
  let N = n in
  let N2 = 0 in
  
  N2 = N / 2;
  if (N mod 2) = 0 then
    N = N2 + 1;
  else
    N = N2;
  a.[<idx>] <- a.[<idx>] + 1
  
;;

Kirc.gen reduce;;