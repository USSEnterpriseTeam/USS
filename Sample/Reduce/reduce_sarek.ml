open Spoc
open Kirc

let reduce = kern a b n ->
  let open Std in
  let idx = global_thread_id in
  let mutable N = 0 in
  N := 1
;;
  
  
