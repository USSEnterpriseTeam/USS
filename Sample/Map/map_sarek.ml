open Spoc
open Kirc

open Skeleton
open Transform

let a = Vector.create Vector.int32 10;;

let b = mapTest(kern a -> a + 1) a;;
let length = Vector.length b in  
Printf.printf "%d\n" length;;
