open Spoc
open Kirc
open Kirc_Ast

let rec print_l (l : (string*string) list) =
  match l with
  | (ancien, nouveau)::queue -> Printf.printf "%s -> %s" ancien nouveau; print_l queue
  | [] -> Printf.printf "\n"

let arg_of_vec v  =
  match Vector.kind v with
  | Vector.Int32 _ -> Kernel.VInt32 v
  | Vector.Float32 _ -> Kernel.VFloat32 v
  | Vector.Float64 _ -> Kernel.VFloat64 v
  | Vector.Int64 _ -> Kernel.VInt64 v
  | _ -> assert false

let arg_of_int i =
  Kernel.Int32 i
    
let a_to_vect_name a name =
  match a with
  | IntVar (i, _) -> (new_int_vec_var (i) name)
  | FloatVar (i, _) -> (new_float_vec_var (i) name)
  | a -> print_ast a; failwith "a_to_vect name"
    
(*
  Retourne le nom d'une declaration de variable
*)
let param_name (var: k_ext) =
  match var with
  | VecVar (t, i, s) -> s
  | IntVar (i, s) -> s
  | FloatVar(i, s) -> s
  | a -> print_ast a; failwith "TODO"   

(*
  Ajoute les arguments supplementaire du squelette
  @param var les arguments du squelette restant
  @param ret le type de retour du kernel
*)
let suite_args (var: k_ext) (ret) =
  let rec aux elem = 
    match elem with
    | Concat (a, b) ->
       (match b with
       | Concat _ ->
	  let (list, suite) = aux b in	  
	  (list, concat (a) suite)
       | Empty ->
	  let name = param_name a in	  
	  (("ret_val", name) :: [], concat (a_to_vect_name (fst ret) name) (empty_arg()))
       | a -> print_ast a; failwith "malformed skeleton" 
       )
    | a -> print_ast a; failwith "not enough args in user kernel -> retour_args"
  in aux (var)


(*
  Change les parametres du squelette par des parametre avec le bon type
  @param skel_args les arguments du squelette
  @param user_args les arguments de l'ast utilisateur
  @param ret le type de retour du kernel
*)
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
	       ((nouveau, ancien) :: list, concat (a_to_vect_name a2 ancien) ret)
	    | (suite, Empty) ->
	       let ancien = param_name a1 in
	       let nouveau = param_name a2 in
	       let (list, ret) = suite_args suite ret in
	       ((nouveau, ancien) :: list,  concat (a_to_vect_name a2 ancien) ret)
	    | (x1, x2) -> print_ast x1; print_ast x2; failwith "translation_create"
	    )
	 | _ -> failwith "")
       in aux p1 p2
    |_ -> failwith "malformed params"
  in
  let (list, ret) = new_args skel_args user_args in
  (list, params (ret))
    
(*
  Recupere un element dans les parametres du Skel en fonction du nom dans les parametres de l'ast utilisateur
  @param name le nom dans l'ast utilisateur
  @param params les parametres du noeud Skel
  @param trans la table de traduction
*)
let translate_id (name: string) (params: k_ext) (trans: (string * string) list) =
  let rec find list  =
    match list with
    | (nouveau, ancien)::suite ->
       if(nouveau = name) then
	 ancien
       else find suite
    | [] -> Printf.printf "%s" name; failwith "cannot find var"
  in
  let ancien = find trans in
  let rec find_ancien param =
    match param with
    | Concat (a, b) ->
       (match a with
       | IntVecAcc ( IntId (name, _), _) ->
	  if (ancien = name) then
	    a
	  else
	    ( match b with
	    | Concat _ -> find_ancien b
	    | a -> print_ast a; failwith "find_ancien cannot find in params"
	    )
       | a -> print_ast a; failwith "find_ancien"
       )
    | a -> print_ast a; failwith "find_ancien"
  in find_ancien params

(*
  Remplace un noeud de type Skel par l'ast de l'utilisateur
  @param les parametres du noeud Skel 
  @param user_body l'ast de l'utilisateur
  @param trans la table de traduction
*)
let fill_skel_node (params: k_ext) (user_body: k_ext) (trans: (string * string) list) =
  let n_skel =
    let rec aux current =
      ( match current with
      | Return (a) ->
	 let ret = translate_id "ret_val" params trans in
	 set_vect_var (ret) (aux a)
      | Seq(a, b) -> seq a (aux b)
      | Local (a, b) -> Local (a, aux b)
      | Plus (a, b) -> Plus (aux a, aux b)
      | Min (a, b) -> Min (aux a, aux b)
      | Mul (a, b) -> Mul (aux a, aux b)
      | Div (a, b) -> Div (aux a, aux b)
      | Mod (a, b) -> Mod (aux a, aux b)
      | Plusf (a, b) -> Plusf (aux a, aux b)
      | Mulf (a, b) -> Mulf (aux a, aux b)
      | Minf (a, b) -> Minf (aux a, aux b)
      | Divf (a, b) -> Divf (aux a, aux b)
      | LtBool (a, b) -> LtBool (aux a, aux b)
      | GtBool (a, b) -> GtBool (aux a, aux b)
      | If (a, b) -> If (aux a, aux b)
      | Ife (a, b, c) -> Ife (aux a, aux b, aux c)
      | Int a -> Int a
      | Float a -> Float a
      | IntId (v, i) -> translate_id v params trans
      | a -> print_ast a; assert false
      )
    in aux user_body
  in n_skel

(*
  Parcours un squelette et remplace ce qu'il faut
  @param skel_body le squelette
  @param user_body l'ast renseigne par l'utilisateur
  @param trans la table de translation des variable
  @return le corp de l'ast
*)
let skel_body_creation (skel_body: k_ext) (user_body: k_ext) (trans: (string * string) list) =
  let n_body =
    let rec aux current =
      (match current with
      | Seq(a, b) -> seq a (aux b)
      | Local (a, b) -> Local (a, aux b)
      | Plus (a, b) -> Plus (aux a, aux b)
      | Min (a, b) -> Min (aux a, aux b)
      | Mul (a, b) -> Mul (aux a, aux b)
      | Div (a, b) -> Div (aux a, aux b)
      | Mod (a, b) -> Mod (aux a, aux b)
      | LtBool (a, b) -> LtBool (aux a, aux b)
      | GtBool (a, b) -> GtBool (aux a, aux b)
      | If (a, b) -> If (aux a, aux b)
      | Ife (a, b, c) -> Ife (aux a, aux b, aux c)
      | Int a -> Int a
      | IntId (v, i) -> IntId (v, i)
      | Id (v) -> Id (v)
      | Skel (a) -> fill_skel_node a user_body trans
      | IdName (v) -> IdName (v) 
      | a -> print_ast a; assert false
      )
    in aux skel_body
  in n_body
  
(*
  Creer le kernel compilable
*)
let res_creation (ker2, k) (ml_kern) (k1) (param, body) (k3) =
  (ker2,
   {
     ml_kern = ml_kern;
     body = Kern(param, body);
     ret_val = Unit, Vector.int32;
     extensions = k.extensions;
   })
    

(*
  Calcule la taille des block necessaire sur un tableau a une entree
  @retour (block, grid)
*)
let thread_creation (device) (length: int) =
  let open Kernel in
  let block = {blockX = 1; blockY = 1; blockZ = 1}
  and grid = {gridX = 1; gridY = 1; gridZ = 1} in
  begin
    let open Devices in (
      match device.Devices.specific_info with
      | Devices.CudaInfo cI ->
	 if length < (cI.maxThreadsDim.x) then (
	   grid.gridX <- 1;
	   block.blockX <- (length)
	 ) else (
	   block.blockX <- cI.maxThreadsDim.x;
	   grid.gridX <- (length) / cI.maxThreadsDim.x;
	 )
      | Devices.OpenCLInfo oI ->
	 if length < oI.Devices.max_work_item_size.Devices.x then (
           grid.gridX <- 1;
           block.blockX <- length
	 ) else (
           block.blockX <- oI.Devices.max_work_item_size.Devices.x;
           grid.gridX <- (length) / block.blockX
	 )
    )
  end;
  (block, grid)

(*
  Retourne l'Ast d'un squelette de map
*)
let map_skel =
     let (cuda_name, opencl_name) = ("blockIdx.x*blockDim.x+threadIdx.x", "get_global_id(0)") in  
     let params = params (concat (new_int_vec_var (0) "a")
				(concat (new_int_var (1) "n")
				   (concat (new_int_vec_var (2) "b") (empty_arg()))))
     in
     let id_a = IntId  ("a", (0)) in
     let id_b = IntId ("b", (2)) in
     let id_n = IntId ("n", (1)) in
     let id_x = IntId ("x", (4)) in
     let vec_acc_a = IntVecAcc (id_a, id_x) in
     let vec_acc_b = IntVecAcc (id_b, id_x) in
     let skel_args = Skel (Concat ( vec_acc_a, ( Concat ( vec_acc_b, (empty_arg()) )))) in
     let body = Local ( (Decl (new_int_var (4) "x") ),
			     Seq
			       (Set (id_x , Intrinsics ((cuda_name, opencl_name)) ),
				(If (LtBool ((id_x, id_n)),
				     skel_args)
				))) in
     (params, body)

let map2_skel =
  let (cuda_name, opencl_name) = ("blockIdx.x*blockDim.x+threadIdx.x", "get_global_id(0)") in  
  let params = params (concat (new_int_vec_var (0) "a")
			 (concat (new_int_vec_var (1) "b")
			    (concat (new_int_var (2) "n")
			       (concat (new_int_vec_var (3) "c") (empty_arg())))))
  in
  let id_a = IntId  ("a", (0)) in
  let id_b = IntId ("b", (1)) in
  let id_c = IntId ("c", (3)) in
  let id_n = IntId ("n", (2)) in
  let id_x = IntId ("x", (4)) in
  let vec_acc_a = IntVecAcc (id_a, id_x) in
  let vec_acc_b = IntVecAcc (id_b, id_x) in
  let vec_acc_c = IntVecAcc (id_c, id_x) in 
  let skel_args = Skel (Concat ( vec_acc_a, ( Concat ( vec_acc_b, ( Concat (vec_acc_c, (empty_arg()) )))))) in
  let body = Local ( (Decl (new_int_var (4) "x") ),
		     Seq
		       (Set (id_x , Intrinsics ((cuda_name, opencl_name)) ),
			(If (LtBool ((id_x, id_n)),
			     skel_args)
			))) in
  (params, body)
  

let map2  ((ker: ('a, 'b,('c -> 'd -> 'e), 'f,'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in1 : ('c, 'i) Vector.vector) (vec_in2 : ('d, 'k) Vector.vector) : ('e, 'm) Vector.vector =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     let (skel_param, skel_body) = map2_skel in
     let (trans, final_params) = translation_create skel_param param k3 in
     let final_body = skel_body_creation skel_body body trans in

     let ml_kern = (let map2 = fun f k a b ->
       let c = Vector.create k (Vector.length a) in
       for i = 0 to (Vector.length a - 1) do
	 Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get b i))
       done;
       c
		    in map2 (k1) (snd k3)) in
       
     let res = res_creation ker ml_kern k1 (final_params, final_body) k3 in
     let length1 = Vector.length vec_in1 in
     let length2 = Vector.length vec_in2 in
     let length = if (length1 < length2) then length1 else length2 in
     let vec_out = (Vector.create (snd k3) ~dev:device length) in
     Mem.to_device vec_in1 device;
     Mem.to_device vec_in2 device;

     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in
     ignore(gen ~only:target res); 
     (*affiche l'ast je sais pas pk.
     *)

     let spoc_ker, kir_ker = res in
     let open Kernel in
     spoc_ker#compile ~debug:false device;
     let (block, grid ) = thread_creation device length in
     let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
     let offset = ref 0 in
     (* Passage des parametre au kernel *)

     (match device.Devices.specific_info with
     | Devices.CudaInfo cI ->
	let extra = Kernel.Cuda.cuda_create_extra 2 in
	Kernel.Cuda.cuda_load_arg offset extra device bin 0 (arg_of_vec vec_in1);
	Kernel.Cuda.cuda_load_arg offset extra device bin 1 (arg_of_vec vec_in2);
	Kernel.Cuda.cuda_load_arg offset extra device bin 2 (arg_of_int length);
	Kernel.Cuda.cuda_load_arg offset extra device bin 3 (arg_of_vec vec_out);
	Kernel.Cuda.cuda_launch_grid offset bin grid block extra device.Devices.general_info 0;
     | Devices.OpenCLInfo _ ->
	let clFun = bin in
	let offset = ref 0
	in
	Kernel.OpenCL.opencl_load_arg offset device clFun 0 (arg_of_vec vec_in1);
	Kernel.OpenCL.opencl_load_arg offset device clFun 1 (arg_of_vec vec_in2);
	Kernel.OpenCL.opencl_load_arg offset device clFun 2 (arg_of_int length);
	Kernel.OpenCL.opencl_load_arg offset device clFun 3 (arg_of_vec vec_out);
	Kernel.OpenCL.opencl_launch_grid clFun grid block device.Devices.general_info 0
     );
     vec_out		
  | _ -> failwith "malformed Kernel"

     
    
let map ((ker: ('a, 'b, ('c -> 'd), 'e, 'f) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'h) Vector.vector) =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let (skel_param, skel_body) = map_skel  in

     (*Transformation des parametres et recuperation de la table de traduction*)
     let (trans, final_params) = translation_create skel_param param k3 in

     (* Transformation de l'ast du squelette en fonction de l'ast de l'utilisateur *)
     let final_body = skel_body_creation skel_body body trans in

     (* Creation de l'element compilable par Spoc *)
     let res = res_creation ker (Tools.map(k1) (snd k3)) k1 (final_params, final_body) k3 in
     let length = Vector.length vec_in in

     (*Generation du vecteur de sortie*)
     let vec_out = (Vector.create (snd k3) ~dev:device length) in
     Mem.to_device vec_in device;

     
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in

     
     ignore(gen ~only:target res); 
     (*affiche l'ast je sais pas pk.
     *)

     let spoc_ker, kir_ker = res in
     let open Kernel in
     spoc_ker#compile ~debug:false device;
     let (block, grid ) = thread_creation device length in
     let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
     let offset = ref 0 in
     (* Passage des parametre au kernel *)
     (match device.Devices.specific_info with
     | Devices.CudaInfo cI ->
	let extra = Kernel.Cuda.cuda_create_extra 2 in
	Kernel.Cuda.cuda_load_arg offset extra device bin 0 (arg_of_vec vec_in);
	Kernel.Cuda.cuda_load_arg offset extra device bin 1 (arg_of_int length);
	Kernel.Cuda.cuda_load_arg offset extra device bin 2 (arg_of_vec vec_out);
	Kernel.Cuda.cuda_launch_grid offset bin grid block extra device.Devices.general_info 0;
     | Devices.OpenCLInfo _ ->
	let clFun = bin in
	let offset = ref 0
	in
	Kernel.OpenCL.opencl_load_arg offset device clFun 0 (arg_of_vec vec_in);
	Kernel.OpenCL.opencl_load_arg offset device clFun 1 (arg_of_int length);
	Kernel.OpenCL.opencl_load_arg offset device clFun 2 (arg_of_vec vec_out);
	Kernel.OpenCL.opencl_launch_grid clFun grid block device.Devices.general_info 0
       );
     vec_out		
  | _ -> failwith "malformed Kernel"


  
