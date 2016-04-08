let map = kern a b n ->
  let open Std in
  let x = global_id_x in
  if(x < n) then
    b.[<x>] <- add a.[<x>] 1;
;;

Kirc.gen map;;
