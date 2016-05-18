open Spoc
open Kirc

let vec_add = kern a b c n ->
  let open Std in
  let open Math.Float64 in
  let idx = global_thread_id in
  if idx < n then
    c.[<idx>] <- add a.[<idx>] b.[<idx>]
;;

let devices = Devices.init();;
let dev = ref devices.(0);;
let n = 10;;
let v1 = Vector.create Vector.float64 n;;
let v2 = Vector.create Vector.float64 n;;
let v3 = Vector.create Vector.float64 n;;

for i = 0 to Vector.length v1 - 1 do
  Spoc.Mem.set v1 i (Random.float 100.);
  Spoc.Mem.set v2 i (Random.float 100.);
done;;

let block = {Spoc.Kernel.blockX = 1024; Spoc.Kernel.blockY = 1; Spoc.Kernel.blockZ = 1};;
let grid = {Spoc.Kernel.gridX = (n+1024-1)/1024; Spoc.Kernel.gridY = 1; Spoc.Kernel.gridZ = 1};;

Kirc.gen vec_add;;
Kirc.run vec_add (v1, v2, v3, n) (block, grid) 0 !dev;;
for i = 0 to Vector.length v3 - 1 do
  Printf.printf "res[%d] = %f\n" i (Spoc.Mem.get v3 i)
done;;
Printf.printf "\n";;

Kirc.run vec_add (v1, v2, v3, n) (block, grid) 1 !dev;;
for i = 0 to Vector.length v3 - 1 do
  Printf.printf "res[%d] = %f\n" i (Spoc.Mem.get v3 i)
done;;
Printf.printf "\n";;

