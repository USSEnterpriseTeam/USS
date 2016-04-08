open Spoc
open Kirc

open Skeleton

let a = Vector.create Vector.int32 10;; 
mapTest (kern a -> a + 1) a;;
