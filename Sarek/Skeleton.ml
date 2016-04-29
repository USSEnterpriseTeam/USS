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


let a_to_simple_name a name =
  match a with
  | IntVar(i, _) -> (new_int_var (i) name)
  | FloatVar(i, _) -> (new_float_var (i) name)
  | a -> print_ast a; failwith "a_to_simple"

    
let a_to_simple_name_int a name i =
  match a with
  | IntVar(_, _) -> (new_int_var (i) name)
  | FloatVar(_, _) -> (new_float_var (i) name)
  | a -> print_ast a; failwith "a_to_simple"

    
let a_to_vect_name a name =
  match a with
  | IntVar (i, _) -> (new_int_vec_var (i) name)
  | FloatVar (i, _) -> (new_float_vec_var (i) name)
  | a -> print_ast a; failwith "a_to_vect name"

let a_to_vect_name_int a name i =
  match a with
  | IntVar (_, _) -> (new_int_vec_var (i) name)
  | FloatVar (_, _) -> (new_float_vec_var (i) name)
  | a -> print_ast a; failwith "a_to_vect name"
    
(*
  Retourne le nom d'une declaration de variable
*)
let rec param_name (var: k_ext) =
  match var with
  | VecVar (t, i, s) -> s
  | IntVar (i, s) -> s
  | IntId (s, i) -> s
  | FloatVar(i, s) -> s
  | IntVecAcc (a, b) -> param_name a
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

let suite_args_simple (var: k_ext) (ret) =
  let rec aux elem =
    match elem with
    | Concat (a, b) ->
       (match b with
       | Concat _ ->
	  let (list, suite) = aux b in	  
	  (list, concat (a) suite)
       | Empty ->
	  let name = param_name a in	  
	  (("ret_val", name) :: [], concat (a_to_simple_name (fst ret) name) (empty_arg()))
       | a -> print_ast a; failwith "malformed skeleton" 
       )
    | a -> print_ast a; failwith "not enough args in user kernel -> retour_args"
  in aux (var)

let type_of_param (param: k_ext) (nb: int) =
  match param with
  | Params (a) ->
     let cpt = ref 0 in
     let rec aux p = 
       (match p with
       | Concat (a1, b1) ->
	  cpt := !cpt + 1;
	  if(!cpt == nb + 1) then a1
	  else aux b1	      
       | _ -> failwith "type_of_param"
       )
     in
     aux a
  | _ -> failwith "type_of_param"      
  
(*
  Creer une table de translation pour les parametres d'un noeud de tye skel
  @param skel_args le noeud de type Skel
  @param user_args le noeud de type Params
*)
let translation_create_only (skel_args: k_ext) (user_args: k_ext) =
  let translation args1 args2 =
    match (args1, args2) with
    | (p1, Params p2) ->
       let rec aux p1 p2 =
	 (match (p1, p2) with
	 | (Concat (a1, b1), Concat(a2, b2)) ->
	    (match (b1, b2) with
	    | (Concat _, Concat _) ->
	       let nouveau = param_name a2 in
	       let list = (aux b1 b2) in
	       (nouveau, a1) :: list
	    | (Empty, Empty) ->
	       let nouveau = param_name a2 in
	       (nouveau, a1) :: []
	    | (a, Empty) ->
	       print_ast a; failwith "Too much args in skel node";
	    | (Empty, a) ->
	       print_ast a; failwith "Not enough args in skel node";
	    | (a, b) ->
	       print_ast a; print_ast b; failwith "translation_create_only";
	    )
	 | (a, b) -> print_ast a; print_ast b; failwith "translation_create_only";
	 )
       in aux p1 p2
    | (a, b) -> print_ast a; print_ast b; failwith "translation_create_only";
  in
  translation skel_args user_args
    
(*
  Recupere un element dans les parametres du Skel en fonction du nom dans les parametres de l'ast utilisateur
  @param name le nom dans l'ast utilisateur
  @param params les parametres du noeud Skel
  @param trans la table de traduction
*)
let translate_id (name: string) (trans: (string * k_ext) list) =
  let rec find list  =
    match list with
    | (nouveau, ancien)::suite ->
       if(nouveau = name) then
	 ancien
       else find suite
    | [] -> Printf.printf "%s" name; failwith "cannot find var"
  in
  find trans

(*
  Remplace un noeud de type Skel par l'ast de l'utilisateur
  @param les parametres du noeud Skel 
  @param user_body l'ast de l'utilisateur
  @param trans la table de traduction
*)
let fill_skel_node (params: k_ext) (user_params: k_ext) (user_body: k_ext) (ret: k_ext) =
  let rec print_list (l: (string*k_ext) list) =
    match l with
    | [] -> Printf.printf ("\n");
    | t ->
       let head = List.hd t in
       Printf.printf ("(%s)") (fst head);
       print_ast (snd head); 
       print_list (List.tl t)
  in
  let trans = translation_create_only params user_params in
  print_list trans;
  let n_skel =
    let rec aux current =
      ( match current with
      | Return (a) ->
	(match ret with
	| IntId(_,_) -> Set (ret, (aux a))
	| IntVecAcc(_,_) -> set_vect_var (ret) (aux a)
	| a -> print_ast a; failwith "Translation create"
	)
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
      | EqBool (a, b) -> EqBool (aux a, aux b)
      | If (a, b) -> If (aux a, aux b)
      | Ife (a, b, c) -> Ife (aux a, aux b, aux c)
      | Int a -> Int a
      | Float a -> Float a
      | IntId (v, i) -> translate_id v trans	 
      | a -> print_ast a; assert false
      )
    in aux user_body
  in n_skel


let translate_info (var: k_ext) (trans: (string * k_ext) list) =
  let get_name var =
    match var with
    | IntVar (_, name) -> name
    | FloatVar (_, name) -> name
    | UnitVar (_, name) -> name
    | DoubleVar (_, name) -> name
    | BoolVar (_, name) -> name
    | VecVar (_, _, name) -> name
    | IdName (name) -> name
    | a -> print_ast a; failwith "translate_info";
  in
  let name = get_name var in
  let rec find_name name list var =
    match list with
    | (ancien, ret)::queue ->
       if(ancien = name) then ret
       else find_name name queue var
    | [] -> var
  in find_name name trans var
  
(*
  Parcours un squelette et remplace ce qu'il faut
  @param skel_body le squelette
  @param user_body l'ast renseigne par l'utilisateur
  @param trans la table de translation des variable
  @return le corp de l'ast
*)
let skel_body_creation (skel_body: k_ext) (user_params: k_ext) (user_body: k_ext)  (trans: (string * k_ext) list) =
  let n_body =
    let rec aux current =
      (match current with
      | Seq(a, b) -> seq a (aux b)
      | Local (a, b) -> Local (aux a, aux b)
      | Decl (a) -> translate_info a trans
      | Plus (a, b) -> Plus (aux a, aux b)
      | Min (a, b) -> Min (aux a, aux b)
      | Mul (a, b) -> Mul (aux a, aux b)
      | Div (a, b) -> Div (aux a, aux b)
      | Mod (a, b) -> Mod (aux a, aux b)
      | LtBool (a, b) -> LtBool (aux a, aux b)
      | GtBool (a, b) -> GtBool (aux a, aux b)
      | EqBool (a, b) -> EqBool (aux a, aux b)
      | If (a, b) -> If (aux a, aux b)
      | Ife (a, b, c) -> Ife (aux a, aux b, aux c)
      | Int a -> Int a
      | IntId (v, i) -> IntId (v, i)
      | Id (v) -> Id (v)
      | Skel (a, b) -> fill_skel_node a user_params user_body b
      | IdName (v) -> IdName (v) 
      | DoLoop (a, b, c, d) -> DoLoop (aux a, aux b, aux c, aux d)
      | Acc (a, b) -> Acc (aux a, aux b)
      | Return (a) -> Return (aux a)
      | And (a, b) -> And (aux a, aux b)
      | Or (a, b) -> Or (aux a, aux b)
      | IntVecAcc (a,b) -> IntVecAcc (aux a, aux b)
      | Empty -> Empty
      | While (a,b) -> While (aux a, aux b)
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
     let skel_args = Skel (Concat ( vec_acc_a, ( empty_arg()) ), vec_acc_b) in
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
  let skel_args = Skel (Concat ( vec_acc_a, ( Concat ( vec_acc_b,  (empty_arg()) ))), vec_acc_c) in
  let body = Local ( (Decl (new_int_var (4) "x") ),
		     Seq
		       (Set (id_x , Intrinsics ((cuda_name, opencl_name)) ),
			(If (LtBool ((id_x, id_n)),
			     skel_args)
			))) in
  (params, body)

let generate_indice_skel =
  let (cuda_name, opencl_name) = ("blockIdx.x*blockDim.x+threadIdx.x", "get_global_id(0)") in
  let params = params (concat (new_int_var (0) "n")
			 (concat (new_int_vec_var (1) "a") (empty_arg())))
  in
  let id_n = IntId ("n", (0)) in
  let id_a = IntId ("a", (1)) in
  let id_x = IntId ("x", (2)) in
  let vec_acc_a = IntVecAcc(id_a, id_x) in
  let skel_args = Skel ( Concat (id_n, (empty_arg ()) ), vec_acc_a) in
  let body = Local ( (Decl (new_int_var (4) "x") ),
		     Seq
		       (Set (id_x , Intrinsics ((cuda_name, opencl_name)) ),
			(If (LtBool ((id_x, id_n)),
			     skel_args)
			)))
  in
  (params, body)

let create_var (var: k_ext) (trans: (string * k_ext * bool) list) =
  let name = param_name var in
  let rec find_type name list =
    match list with
    | (value, a, b)::queue ->
       if (value = name) then (a, b)
       else find_type name queue
    | [] -> failwith "create_var"
  in
  let (type_info, is_vec) = find_type (name) trans in
  match var with
  | IntVar(i, _) ->
     if (is_vec) then a_to_vect_name_int type_info name i
     else a_to_simple_name_int type_info name i
  | FloatVar (i, _) ->
     if (is_vec) then a_to_vect_name_int type_info name i
     else a_to_simple_name_int type_info name i
  | VecVar (_, i, _) ->
     if (is_vec) then a_to_vect_name_int type_info name i
     else a_to_simple_name_int type_info name i
  | _ -> failwith "create_var"

     
let param_creation (skel_param: k_ext) (trans: (string * k_ext * bool) list) =
  match skel_param with
  | Params (a) ->
     let rec aux p =
       (match p with
       | Concat (a1, b1) ->
	  (match b1 with
	  | Concat (_, _) ->
	     (concat (create_var a1 trans) (aux b1))
	  | Empty ->
	     (concat (create_var a1 trans) (empty_arg()))
	  | _ -> failwith "param_creation"
	  )
       | _ -> failwith "param_creation"
       )
     in params (aux a)     
  | _ -> failwith "param_creation"
     
let map ((ker: ('a, 'b, ('c -> 'd), 'e, 'f) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'h) Vector.vector) =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let (skel_param, skel_body) = map_skel  in
     let a_info = ("a", type_of_param param 0, true) in
     let n_info = ("n", new_int_var (0) "n", false) in
     let b_info = ("b", fst k3, true) in
     (*Transformation des parametres et recuperation de la table de traduction*)
     let final_params = param_creation skel_param (a_info :: n_info :: b_info :: []) in

     (* Transformation de l'ast du squelette en fonction de l'ast de l'utilisateur *)
     let final_body = skel_body_creation skel_body param body  [] in

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



let map2  ((ker: ('a, 'b, ('c -> 'd -> 'e), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in1 : ('c, 'i) Vector.vector) (vec_in2 : ('d, 'k) Vector.vector) : ('e, 'm) Vector.vector =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     let (skel_param, skel_body) = map2_skel in
     let a_info = ("a", type_of_param param 0, true) in
     let b_info = ("b", type_of_param param 1, true) in
     let n_info = ("n", new_int_var (0) "n", false) in
     let c_info = ("c", fst k3, true) in

     (*Transformation des parametres et recuperation de la table de traduction*)
     let final_params = param_creation skel_param (a_info :: b_info :: n_info :: c_info :: []) in
    
     let final_body = skel_body_creation skel_body param body [] in

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


     let spoc_ker, kir_ker = res in
     let open Kernel in
     spoc_ker#compile ~debug:false device;
     let (block, grid ) = thread_creation device length in
     let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
     let offset = ref 0 in

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


let generate ((ker: ('a, 'b, (int -> 'd), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init()).(0)) (size_in: int) = 
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     let (skel_param, skel_body) = generate_indice_skel  in       
     let n_info = ("n", new_int_var (0) "n", false) in
     let a_info = ("a", fst k3, true) in	  
     let final_params = param_creation skel_param (n_info :: a_info :: []) in     	  
     let final_body = skel_body_creation skel_body param body [] in
     let ml_kern = (let generate = fun f k n ->
       let c = Vector.create k (n) in
       for i = 0 to (n - 1) do
	 Mem.unsafe_set c i ( f (i))
       done;
       c
		    in generate (k1) (snd k3)) in
     
     let res = res_creation ker ml_kern k1 (final_params, final_body) k3 in     
     let vec_out = (Vector.create (snd k3) ~dev:device size_in) in
     
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in

     
     ignore(gen ~only:target res); 
     (*affiche l'ast je sais pas pk.*)

     let spoc_ker, kir_ker = res in
     let open Kernel in
     spoc_ker#compile ~debug:false device;
     let (block, grid ) = thread_creation device size_in in
     let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
     let offset = ref 0 in
     (* Passage des parametre au kernel *)
     (match device.Devices.specific_info with
     | Devices.CudaInfo cI ->
	let extra = Kernel.Cuda.cuda_create_extra 2 in
	Kernel.Cuda.cuda_load_arg offset extra device bin 0 (arg_of_int size_in);
	Kernel.Cuda.cuda_load_arg offset extra device bin 1 (arg_of_vec vec_out);
	Kernel.Cuda.cuda_launch_grid offset bin grid block extra device.Devices.general_info 0;
     | Devices.OpenCLInfo _ ->
	let clFun = bin in
	let offset = ref 0
	in
	Kernel.OpenCL.opencl_load_arg offset device clFun 0 (arg_of_int size_in);
	Kernel.OpenCL.opencl_load_arg offset device clFun 1 (arg_of_vec vec_out);
	Kernel.OpenCL.opencl_launch_grid clFun grid block device.Devices.general_info 0
       );
     vec_out		
  | _ -> failwith "malformed Kernel"
  
    
(*
  Retourne l'Ast d'un squelette de reduce
*)
let reduce_skel =
  let (cuda_name, opencl_name) = ("blockIdx.x*blockDim.x+threadIdx.x", "get_global_id(0)") in  
  let params = params (concat (new_int_vec_var (0) "a")
			 (concat (new_int_var (1) "n")
			    (concat (new_int_vec_var (7) "b") (empty_arg()))))
  in
  let id_a = IntId ("a", (0)) in
  let id_n = IntId ("n", (1)) in
  let id_x = IntId ("idx", (2)) in
  let id_n2 = IntId ("n2", (3)) in
  let id_n3 = IntId ("n3", (4)) in
  let id_pos = IntId ("pos", (5)) in
  let id_i = IntId ("i", (6)) in
  let id_b = IntId ("b", (7)) in
  let id_tmp = IntId ("spoc_var0", (8)) in
  let vec_acc_tmp1 = IntVecAcc (id_tmp, id_x) in
  let vec_acc_tmp2 = IntVecAcc (id_tmp, id_pos) in
  let vec_acc_a = IntVecAcc (id_a, id_x) in
  let skel_args = Skel (Concat ( vec_acc_tmp1, ( Concat ( vec_acc_tmp2,  (empty_arg())))), vec_acc_tmp1) in

  let body = Local ( Local (
    Decl (IdName("tmp")),
    Local(
      Decl (new_int_var (2) "idx"),
      Local ( 
	Decl (new_int_var (3) "n2"),
	Local ( 
	  Decl (new_int_var (4) "n3"),
	  Local (
            Decl (new_int_var (5) "pos"),
	    Seq (
    	      Set (id_x , Intrinsics ((cuda_name, opencl_name)) ),
	      Seq (
		Acc (IntVecAcc (id_tmp, id_x), vec_acc_a),
		Seq (
		  SyncThread,
		  Seq (
		    Set (id_n2, id_n),
		    Seq (
		      Set (id_n3, id_n),
		      Seq (
			Set (id_pos, Int (0)),
			
			While ( 
			  GtBool (id_n3, Int (0)),
			  Seq (
			    Acc (id_n3, Div (id_n2, Int (2))),
			    Seq (
			      Ife (
				EqBool (Mod (id_n2, Int (2)), Int (0)),
				Acc (id_n2, id_n3),
				Acc (id_n2, Plus (id_n3, Int (1)))
			      ),
			      If (
				LtBool (id_x, id_n2),
				Ife (
				  And (
				    Or (LtBool (id_n2, id_n3), 
					GtBool (id_n2, id_n3)),
				    GtBool (id_x, Int (0))
				  ),
				  Seq (
				    Acc (id_pos, Plus (id_x, id_n3)),
				    skel_args
				  ),
				  If (
				    EqBool (id_n2, id_n3),
				    Seq (
				      Acc (id_pos, Plus (id_x, id_n2)),
				      skel_args
				    )
				  )
				)
			      )
			    )
			  )
			)
		      )
		    )
		  )
		)
	      )
	    )
	  )
	)
      )
    )
  ),
    Seq (
      SyncThread,
      Seq (
	Acc (IntVecAcc (id_b, (Int (0))), IntVecAcc (id_tmp, (Int 0))),
	Empty
      )
    )
  )
		     in
		   (params, body)

		     
let rec print_list list = 
  if list != [] then
    let (t1, t2) = List.hd list in
    Printf.printf "(%s %s), " t1 t2;
    print_list (List.tl list)
  else
    Printf.printf "\n"
;;

let reduce ((ker: ('a, 'b, ('c -> 'd -> 'e), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'i) Vector.vector) : 'e =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let (skel_param, skel_body) = reduce_skel  in
     let a_info = ("a", type_of_param param 0, true) in
     let n_info = ("n", new_int_var (0) "n", false) in
     let b_info = ("b", fst k3, true) in

     (*Transformation des parametres et recuperation de la table de traduction*)
     let final_params = param_creation skel_param (a_info :: n_info :: b_info :: []) in

     (* Transformation de l'ast du squelette en fonction de l'ast de l'utilisateur *)
     let final_body = skel_body_creation skel_body param body [("tmp", Arr(0, (Int (Vector.length vec_in)), EFloat32, Shared))] in

     (* TODO : VERSION CPU (ici code du map pour exemple) *)
     let ml_kern = (let reduce = fun f k a b ->
       let c = Vector.create k (Vector.length a) in
       for i = 0 to (Vector.length a - 1) do
	 Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get b i))
       done;
       c
		    in reduce (k1) (snd k3)) in
       
     let res = res_creation ker ml_kern k1 (final_params, final_body) k3 in
     

     (* Creation de l'element compilable par Spoc *)
     let length = Vector.length vec_in in

     (*Generation du vecteur de sortie*)
     let vec_out = (Vector.create (snd k3) ~dev:device 1) in
     Mem.to_device vec_in device;
     
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in

     
     ignore(gen res); 
     (*affiche l'ast je sais pas pk.
     *)

     let spoc_ker, kir_ker = res in
     let open Kernel in
     spoc_ker#compile ~debug:true device;
     let (block, grid ) = thread_creation device length in
     let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
     let offset = ref 0 in
     (* Passage des parametres au kernel *)
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
     Spoc.Mem.get vec_out 0		
  | _ -> failwith "malformed Kernel"



(*
  Retourne l'Ast d'un squelette de reduce2
*)
let reduce2_skel =
  let (cuda_name, opencl_name) = ("blockIdx.x*blockDim.x+threadIdx.x", "get_global_id(0)") in  
  let (cu_threadIdx, cl_threadIdx) = ("threadIdx.x", "get_local_id(0)") in
  let (cu_blockIdx, cl_blockIdx) = ("blockIdx.x", "get_group_id(0)") in
  let (cu_gridDim, cl_gridDim) = ("gridDim.x", "get_num_groups(0)") in
  let (cu_blockDim, cl_blockDim) = ("blockDim.x", "get_local_size(0)") in

  let params = params (concat (new_int_vec_var (0) "a")
			 (concat (new_int_var (1) "n")
			    (concat (new_int_vec_var (2) "b") (empty_arg()))))
  in
  let id_a = IntId ("a", (0)) in
  let id_n = IntId ("n", (1)) in
  let id_b = IntId ("b", (2)) in
  let id_tid = IntId ("tid", (3)) in
  let id_i = IntId ("i", (4)) in
  let id_gridSize = IntId ("gridSize", (5)) in
  let id_pos = IntId ("pos", (6)) in
  
  let skel_args_1 = Skel (Concat ( IntVecAcc (id_shared, id_tid), ( Concat ( IntVecAcc (id_a, id_i), ( Concat ( IntVecAcc (id_a, id_pos),  (empty_arg())))))), IntVecAcc (id_shared, id_tid)) in
  let skel_args_2 = Skel (Concat ( IntvecAcc (id_shared, id_tid), ( Concat ( IntVecAcc (id_a, id_i), Concat ( IntVecAcc (id_a, id_pos), (empty_arg()))))), IntVecAcc (id_shared, id_tid)) in
  let skel_args_3 = Skel (Concat ( IntVecAcc (id_shared, id_tid), ( Concat ( IntVecAcc (id_shared, id_pos), (empty_arg())))), IntVecAcc (id_shared, id_tid)) in

  let body = Local (Decl( IdName ("shared")),
		    Local (
		      Decl (new_int_var (3) "tid"),
			Local (
			  Decl (new_int_var (4) "i"),
			  Local (
			    Decl (new_int_var (5) "gridSize"),
			    Local (
			      Decl (new_int_var (6) "pos"),
			      Seq (
				Set (id_x, Intrinsics ((cuda_name, opencl_name))),
				Seq (
				  Set (id_tid, Intrinsics ((cu_threadIdx, cl_threadIdx))),
				  Seq (
				    Set (id_i, 
					 Plus (
					   Mul (Intrinsics ((cu_blockIdx, cl_blockIdx)),
						Mul (Intrinsics ((cu_blockDim, cl_blockDim)),
						     Int (2)),
					   ),
					   id_tid
					 )
				    ),
				    Seq (
				      Set (id_gridSize, 
					   Mul (
					     Intrinsics ((cu_blockDim, cl_blockDim)),
					     Mul (
					       Int (2),
					       Intrinsics ((cu_gridDim, cl_gridDim))
					     )
					   )
				      ),
				      Seq (
					Acc (IntVecAcc (id_shared, id_tid), Int (0)),
					Seq (
					  While (
					    LtBool (id_i, id_n),
					    Seq (
					      Set (id_pos, Plus (id_i, Intrinsics ((cu_blockDim, cl_blockDim)))),
					      Seq (
						Acc (
						  IntVecAcc (id_shared, id_tid), 
						  Plus (
						    IntVecAcc (id_shared, id_tid),
						    Plus (
						      IntVecAcc (id_a, id_i),
						      IntVecAcc (id_a, id_pos)
						    )
						  )
						),
						Set (id_i, Plus (id_i, id_gridSize))
					      )
					    )
					  ),
					  Seq (
					    SyncThread,
					    Seq (
					      If (
						LtBool (id_tid, IdName ("CONSTANTE_1")),
						Acc (IntVecAcc (id_shared, id_tid),
						     Plus (
						       IntVecAcc (id_shared, id_tid),
						       IntVecAcc (id_shared, Plus (id_tid, IdName ("CONSTANTE_1")))
						     )),
					      ),
					      Seq (
						SyncThread,
						Seq (
						  If (
						    Ltbool (id_tid, Int (32)),
						    Acc (IntVecAcc (id_shared, id_tid),
							 Plus (
							   IntVecAcc (id_shared, id_tid),
							   IntvecAcc (id_shared, Plus (id_tid, IdName ("CONSTANTE_2")))
							 )
						    )
						  ),
						  If (
						    EqBool (id_tid, Int (0)),
						    Acc (IntVecAcc (id_b, Intrinsics ((cu_blockIdx, cl_blockIdx))),
							 IntVecAcc (id_shared, Int (0))
						    )
						  )
						)
					      )
					    )
					  )
					)
				      )
				    )
				  )
				)
			      )
			    )
			  )
			)
		      )
		    )
  )
						    
in
  (params, body)

		     
let reduce ((ker: ('a, 'b, ('c -> 'd -> 'e), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'i) Vector.vector) : 'e =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let (skel_param, skel_body) = reduce_skel  in
     let a_info = ("a", type_of_param param 0, true) in
     let n_info = ("n", new_int_var (0) "n", false) in
     let b_info = ("b", fst k3, true) in

     (*Transformation des parametres et recuperation de la table de traduction*)
     let final_params = param_creation skel_param (a_info :: n_info :: b_info :: []) in

     (* Transformation de l'ast du squelette en fonction de l'ast de l'utilisateur *)
     let final_body = skel_body_creation skel_body param body [("tmp", Arr(0, (Int (Vector.length vec_in)), EFloat32, Shared))] in

     let ml_kern = (let reduce = fun f k a b ->
       let c = Vector.create k (Vector.length a) in
       for i = 0 to (Vector.length a - 1) do
	 Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get b i))
       done;
       c
		    in reduce (k1) (snd k3)) in
       
     let res = res_creation ker ml_kern k1 (final_params, final_body) k3 in
     

     (* Creation de l'element compilable par Spoc *)
     let length = Vector.length vec_in in

     (*Generation du vecteur de sortie*)
     let vec_out = (Vector.create (snd k3) ~dev:device 1) in
     Mem.to_device vec_in device;
     
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in

     
     ignore(gen res); 
     (*affiche l'ast je sais pas pk.
     *)

     let spoc_ker, kir_ker = res in
     let open Kernel in
     spoc_ker#compile ~debug:true device;
     let (block, grid ) = thread_creation device length in
     let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
     let offset = ref 0 in
     (* Passage des parametres au kernel *)
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
     Spoc.Mem.get vec_out 0		
  | _ -> failwith "malformed Kernel"
     
