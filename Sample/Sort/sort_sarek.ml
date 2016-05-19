open Spoc
open Kirc


let sort = kern a n start end_ ->
  let open Std in
  let mutable idx = global_thread_id in
  let mutable pivot = 0 in
  let mutable l = 0 in
  let mutable r = 0 in
  start.[<idx>] = idx;
  end_.[<idx>] = (n - 1);
  while (idx >= 0) do
    (l := start.[<idx>];
     r := end_.[<idx>];
     if (l < r) then
       (pivot := a.[<l>];
	while l < r do
	  while (a.[<r>] >= pivot && l < r) do
	    r := r - 1
	  done;
	  if l < r then
	    (a.[<l>] = a.[<r>];
	     l := l + 1;
	    );
	  while (a.[<l>] < pivot && l < r) do
	    l := l + 1;
	  done;
	  if l < r then
	    (a.[<r>] = a.[<l>];
	     r := r - 1;
	    );
	  a.[<l>] = pivot;
	  start.[<(idx + 1)>] = (l + 1);
	  end_.[<(idx + 1)>] = end_.[<idx>];
	  end_.[<idx>] = l;
	  idx := (idx + 1);
	  (*let a = end_.[<idx>] .- start.[<idx>] in
	    let b = end_.[<(idx - 1)>] .- start.[<(idx - 1)>] in*)
	  if l < r then
	    ((*let tmp = start.[<idx>] in*)
	     start.[<idx>] .= start.[<(idx - 1)>];
	     start.[<idx - 1>] .= 0.;
	     (*let tmp2 = end_.[<idx>] in*)
	     end_.[<idx>] .= end_.[<(idx - 1)>];
	     end_.[<(idx - 1)>] .= 0.;
	    )	 
	done)
     else
       idx := idx - 1;
    )
  done
;;

let a = Vector.create Vector.float32 10;;

for i = 0 to Vector.length a - 1 do
  let r = Random.float 100. in
  Spoc.Mem.set a i (r);
done;;


