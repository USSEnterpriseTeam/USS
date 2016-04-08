open Spoc
open Kirc
open Kirc_Ast


let rec print_l (l : (string*string) list) =
  match l with
  | (ancien, nouveau)::queue -> Printf.printf "%s -> %s" ancien nouveau; print_l queue
  | [] -> Printf.printf "\n"


let a_to_vect = function
  | IntVar (i,s)  -> (new_int_vec_var (i) "b")
  | FloatVar (i,s) -> (new_float_vec_var (i) s)
  | a  -> print_ast a; failwith "a_to_vect"

let a_to_vect_name a name =
  match a with
  | IntVar (i, _) -> (new_int_vec_var (i) name)
  | FloatVar (i, _) -> (new_float_vec_var (i) name)
  | a -> print_ast a; failwith "a_to_vect name"
    
let a_to_return_vect k1 k2 idx= 
  match k1 with
  | IntVar (i,s)  ->  (set_vect_var (get_vec (var i s) idx) (k2) )
  | FloatVar (i,s)  ->  (set_vect_var (get_vec (var i s) idx) (k2) )
  | _  -> failwith "error a_to_return_vect"


let param_name (var: k_ext) =
  match var with
  | VecVar (t, i, s) -> s
  | IntVar (i, s) -> s
  | FloatVar(i, s) -> s
  | a -> print_ast a; failwith "TODO"   


let retour_args (var: k_ext) (ret) =
  let rec aux elem = 
    match elem with
    | Concat (a, b) ->
       (match b with
       | Concat _ ->
	  let suite = aux b in
	  (concat (a) suite)
       | Empty ->  (concat (a_to_vect_name (fst ret) "_ret_val") (empty_arg()))
       | a -> print_ast a; failwith "malformed skeleton" 
       )
    | a -> print_ast a; failwith "not enough args in user kernel -> retour_args"
  in aux (var)
    
let translation_create (skel_args: k_ext) (user_args: k_ext) (ret) =
  let new_args args1 args2 = 
    match (args1, args2) with
    | (Params p1, Params p2) ->
       let rec aux p1 p2 =
	 (match (p1, p2) with
	 | (Concat (a1, b1), Concat(a2, b2)) ->
	    (match (b1, b2) with
	    | (Concat _, Concat _) ->
	       let ancien = param_name a1 in
	       let nouveau = param_name a2 in
	       let (list, ret) = (aux b1 b2) in
	       ((ancien, nouveau) :: list, concat (a_to_vect_name a2 ancien) ret)
	    | (suite, Empty) ->
	       let ancien = param_name a1 in
	       let nouveau = param_name a2 in
	       let ret = retour_args suite ret in
	       ((ancien, nouveau) :: [],  concat (a_to_vect_name a2 ancien) ret)
	    | (x1, x2) -> print_ast x1; print_ast x2; failwith "translation_create"
	    )
	 | _ -> failwith "")
       in aux p1 p2
    |_ -> failwith "malformed params"
  in
  let (list, ret) = new_args skel_args user_args in
  (list, params (ret))
       
let param_creation (body: k_ext) (ret)  =
  match body with
  | Kern(args, _) ->
     (let new_args =
       match args with
       | Params p ->
	  (match p with
	  | Concat (Concat _, Concat _) ->  failwith "error multiple args" (*demander pk?*)
	  | Concat (a, Empty) -> params (concat (a_to_vect a) (concat (a_to_vect (fst ret)) (empty_arg())))
	  | _ -> print_ast args; failwith "type error")
       | _ -> failwith "error args"
      in new_args
     )
  | _ -> failwith "malformerd kernel map"

let body_creation (body: k_ext) (ret) =
  match body with
  | Kern (_, body) ->
     (let (cuda_name, opencl_name) = ("blockIdx.x*blockDim.x+threadIdx.x", "get_global_id(0)") in  
      let n_body =
	let rec aux current = 
	  (match current with
	  | Return (a) -> a_to_return_vect (fst ret) (aux a) ((intrinsics cuda_name opencl_name))
	  | Seq (a,b) -> seq a (aux b)
	  | Local (a,b) -> Local (a, aux b)
	  | Plus (a,b)  -> Plus (aux a, aux b)
	  | Min (a,b)  -> Min  (aux a, aux b)
	  | Mul (a,b)  -> Mul  (aux a, aux b)
	  | Div (a,b)  -> Div  (aux a, aux b)
	  | Mod (a,b)  -> Mod  (aux a, aux b)
	  | LtBool (a,b)  -> LtBool (aux a, aux b)
	  | GtBool (a,b)  -> GtBool (aux a, aux b)
	  | Ife (a,b,c)  -> Ife (aux a, aux b, aux c)
	  | Int a -> Int a
	  | IntId (v,i)  -> 
	     if i = 0 then
	       IntVecAcc(IdName ("spoc_var" ^ (string_of_int i)),
			 Intrinsics ((cuda_name, opencl_name)) )
	     else
	       current
	  | a -> print_ast a; assert false)
	in aux body
      in n_body
     )
  | _ -> failwith "malformed kernel map"

     
let res_creation (ker2, k) (k1) (param, body) (k3) =
  (ker2,
   {
     ml_kern = Tools.map(k1) (snd k3);
     body = Kern(param, body);
     ret_val = Unit, Vector.int32;
     extensions = k.extensions;
   })
    

let thread_creation (device) (vec_in) =
  let open Kernel in
  let block = {blockX = 1; blockY = 1; blockZ = 1}
  and grid = {gridX = 1; gridY = 1; gridZ = 1} in
  begin
    let open Devices in (
      match device.Devices.specific_info with
      | Devices.CudaInfo cI ->
	 if Vector.length vec_in < (cI.maxThreadsDim.x) then (
	   grid.gridX <- 1;
	   block.blockX <- (Vector.length vec_in)
	 ) else (
	   block.blockX <- cI.maxThreadsDim.x;
	   grid.gridX <- (Vector.length vec_in) / cI.maxThreadsDim.x;
	 )
      | Devices.OpenCLInfo oI ->
	 if Vector.length vec_in < oI.Devices.max_work_item_size.Devices.x then (
           grid.gridX <- 1;
           block.blockX <- Vector.length vec_in
	 ) else (
           block.blockX <- oI.Devices.max_work_item_size.Devices.x;
           grid.gridX <- (Vector.length vec_in) / block.blockX
	 )
    )
  end;
  (block, grid)
    
let mapTest ((ker: ('a, 'b, ('c -> 'd), 'e, 'f) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'h) Vector.vector) =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     let param_test = params (concat (new_int_vec_var (0) "var0")
				(concat (new_int_var (1) "n")
				   (concat (new_int_vec_var (1) "var1") (empty_arg())))) in
    let (l,test) = translation_create param_test param k3 in
    print_ast test;
    print_l l
  | _ -> failwith "malformed Kernel"
  

(*let param_list = param_creation k2 k3 in
  let body = body_creation k2 k3 in
  let res = res_creation ker k1 (param_list,body) k3 in
  (*let length = Vector.length vec_in in
  let vec_out = (Vector.create (snd k3) ~dev:device length) in*)
  Mem.to_device vec_in device;
  
  (* Demander explication a partir de la *)
  let target =
    match device.Devices.specific_info with
    | Devices.CudaInfo _ -> Devices.Cuda
    | Devices.OpenCLInfo _ -> Devices.OpenCL in
  ignore(gen ~only:target res);
  let spoc_ker, kir_ker = res in
  let open Kernel in
    spoc_ker#compile ~debug:true device;*)
  
