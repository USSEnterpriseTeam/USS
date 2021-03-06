open Spoc
open Kirc
open Kirc_Ast

type ('a, 'b) skel_kernel =
  {
    kirc_kern : 'a;
    lauch_infos : 'b;
  }

type ('a, 'b) param =
  | VParam of ('a, 'b) Vector.vector
  | IParam of int
  | FParam of float

type ('a, 'b) retour =
  | VRetour of ('a, 'b) Vector.vector
  | RNull
      
type args_type =
  | VChar 
  | VFloat32 
  | VFloat64 
  | VComplex32 
  | VInt32 
  | VInt64
  | Int32

let arg_type_of_vec v =
  match Vector.kind v with
  | Vector.Int32 _ -> VInt32
  | Vector.Float32 _ -> VFloat32
  | Vector.Float64 _ -> VFloat64
  | Vector.Int64 _ -> VInt64
  | _ -> VInt64

let arg_type_of_vec_kind v =
  match v with
  | Vector.Int32 _ -> VInt32
  | Vector.Float32 _ -> VFloat32
  | Vector.Float64 _ -> VFloat64
  | Vector.Int64 _ -> VInt64
  | _ -> VInt64
     
     
type ('a, 'b) info_args =
  | SizeOf of int      
  | Param of args_type * int
  | NewVec of args_type * int
  | NewSizedVec of args_type * int
      
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

let get_arg_vec_kind kind =
  match kind with
  | VFloat32 -> (Vector.float32)
  | VFloat64 -> Obj.magic (Vector.float64)
     
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
  | DoubleVar (i, _) -> (new_double_vec_var (i) name)
  | a -> print_ast a; failwith "a_to_vect name"

let a_to_vect_name_int a name i =
  match a with
  | IntVar (_, _) -> (new_int_vec_var (i) name)
  | FloatVar (_, _) -> (new_float_vec_var (i) name)
  | DoubleVar (i, _) -> (new_double_vec_var (i) name)
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
  | DoubleVar(i, s) -> s
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
      | Double a -> Double a
      | App (a,b) -> App (aux a, Array.map aux b)
      | IntId (v, i) -> translate_id v trans
      | Intrinsics a -> Intrinsics a
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
      | Seq(a, b) -> seq (aux a) (aux b)
      | Set(a, b) -> Set (a, aux b)
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
      | a -> a
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

let skel_kern_creation (kern: ('a, 'b, 'c, 'd, 'e) sarek_kernel) (lauch_infos: (('a, 'b) info_args) list) =
  ({
    kirc_kern = kern;
    lauch_infos = lauch_infos;
  })

let find_size params id =
  let nb = ref 0 in
  let rec aux l id =
    match l with
    | head::queue ->
       nb := !nb + 1;
      if((!nb - 1) = id) then
	head	 
      else	 
        aux queue id
    | [] -> assert false;
  in
  let p = aux params id in
  match p with
  | VParam(v) ->
     Vector.length v
  | a -> failwith "find_size"

let get_param params id = 
  let nb = ref 0 in
  let rec aux l id =
    nb := !nb + 1;
    match l with
    | head::queue ->
       if((!nb - 1) = id) then
	 head	 
       else
	 aux queue id
    | [] -> Printf.printf "%d -> %d" id !nb; assert false
  in
  aux params id
    
let arg_of_arg_info arg elem =
  match elem with
  | VParam(v) ->
     (match arg with
     | VChar -> Kernel.VChar v
     | VFloat32 -> Kernel.VFloat32 v
     | VFloat64 -> Kernel.VFloat64 v
     | VComplex32 -> Kernel.VComplex32 v
     | VInt32 -> Kernel.VInt32 v
     | VInt64 -> Kernel.VInt64 v)
  | IParam (i) ->
     (match arg with
     | Int32 -> arg_of_int i
     | _ -> assert false
     ) 
  | _ -> assert false     


     
let move_to_device param device =
  match param with
  | VParam (v) -> Mem.to_device v device
  | IParam (x) -> ()
  | FParam (x) -> ()
     


let pow_2_sup (x: int) =
  let d = log (float_of_int (x - 1)) /. (log 2.) in
  let i = int_of_float d in
  int_of_float (2. ** (float_of_int i +. 1.)) 
    
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
	 block.blockX <- min cI.maxThreadsDim.x length;	 
	 grid.gridX <- length / (block.blockX);
	 if(length mod (block.blockX) > 0) then
	   grid.gridX <- grid.gridX + 1
      | Devices.OpenCLInfo oI ->
	 block.blockX <- min (*oI.Devices.max_work_item_size.Devices.x*) 4096 length;	 
	 grid.gridX <- length / (block.blockX);	 
	 if(length mod (block.blockX) > 0) then
	   grid.gridX <- grid.gridX + 1
    )
  end;
  Printf.printf "block : %d, grid : %d\n" block.blockX grid.gridX;
  (block, grid)    

let thread_creation_pow2 (device) (length: int) =
  let open Kernel in
  let block = {blockX = 1; blockY = 1; blockZ = 1}
  and grid = {gridX = 1; gridY = 1; gridZ = 1} in
  begin
    let open Devices in (
      match device.Devices.specific_info with
      | Devices.CudaInfo cI ->
	 let threads = pow_2_sup (length) in
	 let open Int32 in
	 block.blockX <- min cI.maxThreadsDim.x threads;	 
	 grid.gridX <- length / (block.blockX);
	 if(length mod (block.blockX) > 0) then
	   grid.gridX <- grid.gridX + 1
      | Devices.OpenCLInfo oI ->
	 let threads = pow_2_sup (length) in
	 let open Int32 in
	 block.blockX <- min (*oI.Devices.max_work_item_size.Devices.x*) 4096 threads;
	 grid.gridX <- length / (block.blockX);	 
	 if(length mod (block.blockX) > 0) then
	   grid.gridX <- grid.gridX + 1
    )
  end;
  Printf.printf "block : %d, grid : %d\n" block.blockX grid.gridX;
  (block, grid)    

    
let load_cuda_args param_infos param device bin =
  let length_call = ref 0 in
  let offset = ref 0 in
  let nb = ref 0 in
  let extra = Kernel.Cuda.cuda_create_extra 2 in
  let rec create_params param_infos params = 
    match param_infos with
    | (SizeOf(id))::queue ->
       let length = find_size params id in
       length_call := length;  
       Kernel.Cuda.cuda_load_arg offset extra device bin !nb (arg_of_int length);
       nb := !nb + 1;
       create_params queue params
    | (Param (arg, id))::queue ->
       Kernel.Cuda.cuda_load_arg offset extra device bin !nb (arg_of_arg_info arg (get_param params id));       
      nb := !nb + 1;
      create_params queue params 
    | (NewVec(arg, id))::queue ->
       let length = find_size params id in
       let new_vec = Vector.create (get_arg_vec_kind arg) length ~dev:device in
       Kernel.Cuda.cuda_load_arg offset extra device bin !nb (arg_of_vec new_vec);
       nb := !nb + 1;
       let (a, b, c, outs) = create_params queue params in
       (a, b, c, VRetour(new_vec))
    | (NewSizedVec(arg, length))::queue ->
       let new_vec = Vector.create (get_arg_vec_kind arg) length ~dev:device in
       Kernel.Cuda.cuda_load_arg offset extra device bin !nb (arg_of_vec new_vec);
       nb := !nb + 1;
       let (a, b, c, outs) = create_params queue params in
       (a, b, c, VRetour(new_vec))
    | [] -> (offset, extra, !length_call, RNull)
  in
  create_params param_infos param

let load_opencl_args param_infos param device clFun =
  let length_call = ref 0 in
  let offset = ref 0 in
  let nb = ref 0 in
  let rec create_params param_infos params = 
    match param_infos with
    | (SizeOf(id))::queue ->
       let length = find_size params id in
       length_call := length;       
       Kernel.OpenCL.opencl_load_arg offset device clFun !nb (arg_of_int length);
       nb := !nb + 1;
       create_params queue params
    | (Param (arg, id))::queue ->
       Kernel.OpenCL.opencl_load_arg offset device clFun !nb (arg_of_arg_info arg (get_param params id));
      nb := !nb + 1;
      create_params queue params
    | (NewVec(arg, id))::queue ->
       let length = find_size params id in
       let new_vec = Vector.create (get_arg_vec_kind arg) length ~dev:device in
       Kernel.OpenCL.opencl_load_arg offset device clFun !nb (arg_of_vec new_vec);
       nb := !nb + 1;
       let (b, c, outs) = create_params queue params in
       (b, c, VRetour(new_vec))
    | (NewSizedVec(arg, length))::queue ->
       let new_vec = Vector.create (get_arg_vec_kind arg) length ~dev:device in
       Kernel.OpenCL.opencl_load_arg offset device clFun !nb (arg_of_vec new_vec);
       nb := !nb + 1;
       let (b, c, outs) = create_params queue params in
       (b, c, VRetour(new_vec))
    | [] -> (offset, !length_call, RNull)
  in create_params param_infos param
  
let run_skel (kern: ('a, 'b) skel_kernel) (params: (('c, 'd) param) list) (device) =
  let (ker, infos) = (kern.kirc_kern, kern.lauch_infos) in
  let rec aux params = 
    match params with
    | p::queue ->
       move_to_device p device;
      aux queue
    | [] -> []
  in
  let _ = aux params in
  let open Kernel in
  let (spoc_ker, kir_ker) = ker in
  let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
  match device.Devices.specific_info with
  | Devices.CudaInfo cI ->
     let (offset, extra, length, outs) = load_cuda_args infos params device bin in
     let (block, grid) = thread_creation device length in     
     Kernel.Cuda.cuda_launch_grid offset bin grid block extra device.Devices.general_info 0;
     outs
  | Devices.OpenCLInfo _ ->
     let (offset, length, outs) = load_opencl_args infos params device bin in
     let (block, grid) = thread_creation device length in
     Kernel.OpenCL.opencl_launch_grid bin grid block device.Devices.general_info 0;
     outs


let run_skel_pow2 (kern: ('a, 'b) skel_kernel) (params: (('c, 'd) param) list) (device) =
  let (ker, infos) = (kern.kirc_kern, kern.lauch_infos) in
  let rec aux params = 
    match params with
    | p::queue ->
       move_to_device p device;
      aux queue
    | [] -> []
  in
  let _ = aux params in
  let open Kernel in
  let (spoc_ker, kir_ker) = ker in
  let bin = (Hashtbl.find (spoc_ker#get_binaries ()) device) in
  match device.Devices.specific_info with
  | Devices.CudaInfo cI ->
     let (offset, extra, length, outs) = load_cuda_args infos params device bin in
     let (block, grid) = thread_creation_pow2 device length in     
     Kernel.Cuda.cuda_launch_grid offset bin grid block extra device.Devices.general_info 0;
     outs
  | Devices.OpenCLInfo _ ->
     let (offset, length, outs) = load_opencl_args infos params device bin in
     let (block, grid) = thread_creation_pow2 device length in
     Kernel.OpenCL.opencl_launch_grid bin grid block device.Devices.general_info 0;
     outs
       

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
  let body = 
    Local ( 
      (Decl (
	new_int_var (4) "x") 
      ),
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
  let skel_args = Skel ( Concat (id_x, (empty_arg ()) ), vec_acc_a) in
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
    | [] -> Printf.printf "%s" name; failwith "create_var"
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
     
let generate_from_skel (ast_user) (ast_skel) (param_info: (string * k_ext * bool) list) (trans: (string * k_ext) list) =
  match (ast_user) with
  | Kern (param, body) ->
     let (skel_param, skel_body) = ast_skel in
     let final_params = param_creation skel_param param_info in
     let final_body = skel_body_creation skel_body param body trans in
     (final_params, final_body)
  | a -> print_ast a; assert false
    
let map ((ker: ('a, 'b, ('c -> 'd), 'e, 'f) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'h) Vector.vector) =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     let begin_gen_ = Unix.gettimeofday() in
     let a_info = ("a", type_of_param param 0, true) in
     let n_info = ("n", new_int_var (0) "n", false) in
     let b_info = ("b", fst k3, true) in
     let final_ast = generate_from_skel k2 map_skel (a_info :: n_info :: b_info :: []) [] in
     let res = res_creation ker (Tools.map(k1) (snd k3)) k1 final_ast k3 in
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in

     
     ignore(gen ~only:target res); 
     let spoc_ker, kir_ker = res in
     let info_param1 = Param(arg_type_of_vec vec_in, 0) in
     let info_param2 = SizeOf(0) in
     let info_param3 = NewVec(arg_type_of_vec_kind (snd k3), 0) in
     let final_kern = skel_kern_creation res (info_param1 :: info_param2 :: info_param3 :: []) in
     let open Kernel in 
     spoc_ker#compile ~debug:false device;
     let begin_exec_ = Unix.gettimeofday() in
     Printf.printf ("Temps skel gen = %f\n") (begin_exec_ -. begin_gen_);
     let param1 = VParam(vec_in) in  
     let ret = (run_skel final_kern (param1 :: []) device) in
     let end_exec_ = Unix.gettimeofday() in
     Printf.printf ("Temps skel exec = %f\n") (end_exec_ -. begin_exec_);
     (match (ret) with
     | VRetour(x) -> x
     | _ -> assert false)
  | _ -> failwith "malformed Kernel"


let map2_ml_kern k1 k3 =
  let map2 = fun f k a b ->
    let c = Vector.create k (Vector.length a) in
    for i = 0 to (Vector.length a - 1) do
      Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get b i))
    done;
    c
  in map2 (k1) (snd k3)

  
let map2  ((ker: ('a, 'b, ('c -> 'd -> 'e), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in1 : ('c, 'i) Vector.vector) (vec_in2 : ('d, 'k) Vector.vector)  =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     let begin_time = Unix.gettimeofday() in
     let a_info = ("a", type_of_param param 0, true) in
     let b_info = ("b", type_of_param param 1, true) in
     let n_info = ("n", new_int_var (0) "n", false) in
     let c_info = ("c", fst k3, true) in
     let final_ast = generate_from_skel k2 map2_skel (a_info :: b_info :: n_info :: c_info :: []) [] in
     
     let res = res_creation ker (map2_ml_kern k1 k3) k1 final_ast k3 in
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in
     
     ignore(gen ~only:target res);      
     let spoc_ker, kir_ker = res in
     let info_param1 = Param(arg_type_of_vec vec_in1, 0) in
     let info_param2 = Param(arg_type_of_vec vec_in2, 1) in
     let info_param3 = SizeOf(0) in
     let info_param4 = NewVec(arg_type_of_vec_kind (snd k3), 0) in
     let final_kern = skel_kern_creation res (info_param1 :: info_param2 :: info_param3 :: info_param4 :: []) in
     let open Kernel in 
     spoc_ker#compile ~debug:false device;
     let end_time = Unix.gettimeofday() in
     Printf.printf "transformation time %.3f\n" (end_time -. begin_time);
     let begin_exec_time = Unix.gettimeofday() in
     let param1 = VParam(vec_in1) in
     let param2 = VParam(vec_in2) in
     let ret = (run_skel final_kern (param1 :: param2 :: []) device) in
     let end_exec_time = Unix.gettimeofday() in
     Printf.printf "execution time %.3f\n" (end_exec_time -. begin_exec_time);
     (match ret with
     | VRetour(x) -> x
     | _ -> assert false)
  | _ -> failwith "malformed Kernel"


let generate ((ker: ('a, 'b, (float -> 'd), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init()).(0)) (size_in: int) = 
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     let begin_time = Unix.gettimeofday() in
     let (skel_param, skel_body) = generate_indice_skel  in       
     let n_info = ("n", new_int_var (0) "n", false) in
     let a_info = ("a", fst k3, true) in	  
     let final_params = param_creation skel_param (n_info :: a_info :: []) in     	  
     let final_body = skel_body_creation skel_body param body [] in
     let ml_kern = (let generate = fun f k n ->
       let c = Vector.create k (n) in
       for i = 0 to (n - 1) do
	 Mem.unsafe_set c i ( f (float_of_int i))
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
     let end_time = Unix.gettimeofday() in
     Printf.printf ("temps skel generation = %f\n") (end_time -. begin_time);
     let begin_exec_ = Unix.gettimeofday() in
     let (block, grid ) = thread_creation device size_in in
     Printf.printf ("nb bloc %d") (block.blockX * grid.gridX);
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
     let end_exec_ = Unix.gettimeofday() in
     Printf.printf ("Temps skel exec = %f\n") (end_exec_ -. begin_exec_);
     vec_out		
  | _ -> failwith "malformed Kernel"
     
     
(*
  Retourne l'Ast d'un squelette de reduce
*)
let reduce_skel =
  let (cu_name, cl_name) = ("blockIdx.x*blockDim.x+threadIdx.x", "get_global_id(0)") in  
  let (cu_threadx, cl_threadx) = ("threadIdx.x", "get_local_id(0)") in
  let (cu_floor, cl_floor) = ("floor", "floor") in
  let params = params ( concat (new_int_vec_var (0) "a")
			  (concat (new_int_var (9) "n_i")
		             ( concat (new_int_vec_var (7) "b") (empty_arg()))))
  in
  let id_a     = IntId ("a",   (0)) in
  let id_n     = IntId ("n",   (1)) in
  let id_x     = IntId ("idx", (2)) in
  let id_n2    = IntId ("n2",  (3)) in
  let id_n3    = IntId ("n3",  (4)) in
  let id_pos   = IntId ("pos", (5)) in
  let id_i     = IntId ("i",   (6)) in
  let id_b     = IntId ("b",   (7)) in
  let id_local = IntId ("idl", (9)) in
  let id_tmp   = IntId ("spoc_var0", (8)) in
  let vec_acc_tmp1 = IntVecAcc (id_tmp, id_local) in
  let vec_acc_tmp2 = IntVecAcc (id_tmp, id_pos) in
  let vec_acc_a = IntVecAcc (id_a, id_x) in
  let skel_args = Skel (Concat ( vec_acc_tmp1, ( Concat ( vec_acc_tmp2,  (empty_arg())))), vec_acc_tmp1) in
  let final_skel_args = Skel (Concat ( IntVecAcc (id_b, Int 0), (Concat ( IntVecAcc (id_tmp, Int 0), (empty_arg())))), IntVecAcc (id_b, Int 0)) in

  let body = Local ( Local (
    Decl (IdName("tmp")),
    Local(
      Decl (new_int_var (2) "idx"),
      Local (
	Decl (new_int_var (9) "idl"),
	Local ( 
	  Decl (new_int_var (1) "n"),
	  Local (
	    Decl (new_float_var (3) "n2"),
	    Local ( 
	      Decl (new_float_var (4) "n3"),
	      Local (
		Decl (new_int_var (5) "pos"),
		Seq (
    		  Set (id_x , Intrinsics ((cu_name, cl_name)) ),
		  Seq (
		    Set (id_local, Intrinsics ((cu_threadx, cl_threadx))),
		    Seq (
		      Acc (IntVecAcc (id_tmp, id_local), vec_acc_a),
		      Seq (
			SyncThread,
			Seq (
			  Set (id_n, Decl(IdName ("int_n"))),
			  Seq (
			    Set (id_n2, id_n),
			    Seq (
			      Set (id_n3, id_n),
			      Seq (
				Set (id_pos, Int (0)),
				
				While ( 
				  GtBool (id_n3, Int (0)),
				  Seq (
				    Acc (
				      id_n3,
				      App (Intrinsics ((cu_floor, cl_floor)),
					   (Array.make 1 (Div (id_n2, Int (2))))
				      )
				    ),
				    Seq (
				      Ife (
					EqBool (Mod (CastInt(id_n2), Int (2)), Int (0)),
					Acc (id_n2, id_n3),
					Acc (id_n2, Plus (id_n3, Int (1)))
				      ),
				      If (
					LtBool (id_local, id_n2),
					Ife (
					  And (
					    Or (LtBool (id_n2, id_n3), 
						GtBool (id_n2, id_n3)),
					    GtBool (id_local, Int (0))
					  ),
					  Seq (
					    Acc (id_pos, Plus (id_local, id_n3)),
					    skel_args
					  ),
					  If (
					    EqBool (id_n2, id_n3),
					    Seq (
					      Acc (id_pos, Plus (id_local, id_n2)),
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
	  )
	)
      )
    )
  ),
		     Seq (
		       SyncThread,
		       Seq (
			 If (EqBool (id_x, Int 0),
			     Seq (
			       Acc (IntVecAcc (id_b, Int 0), IntVecAcc (id_tmp, Int 0)),
			       Empty
			     )
			 ),		       
			 Seq (
			   If (And (EqBool(id_local, Int 0), GtBool(id_x, Int 0)),
			       Seq (
				 final_skel_args,
				 Empty
			       )
			   ),
			   Empty
			 ) 
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

let reduceTest ((ker: ('a, 'b, ('c -> 'd -> 'e), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'i) Vector.vector) : 'e =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let begin_time = Unix.gettimeofday() in
     let (skel_param, skel_body) = reduce_skel  in
     let a_info = ("a", type_of_param param 0, true) in
     let n_info = ("n_i", new_int_var (0) "n_i", false) in
     let b_info = ("b", fst k3, true) in
     
     (* On recupere la taille du shared (egale au nombre de threads dans un bloc) *)
     let (block, grid) = thread_creation device (Vector.length vec_in) in
     let tmp_var = ("tmp", Arr(0, (Int (block.blockX)), EFloat32, Shared)) in
     let n_var = ("int_n", Int (block.blockX)) in
     let final_ast = generate_from_skel k2 reduce_skel (a_info :: n_info :: b_info :: []) (tmp_var :: n_var :: []) in

     (* TODO : VERSION CPU (ici code du map pour exemple) *)
     let ml_kern = (let reduce = fun f k a b ->
       let c = Vector.create k (Vector.length a) in
       for i = 0 to (Vector.length a - 1) do
	 Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get b i))
       done;
       c
		    in reduce (k1) (snd k3)) in
     
     let res = res_creation ker ml_kern k1 final_ast k3 in
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in

     
     ignore(gen ~only:target res); 
     
     let open Kernel in
     let spoc_ker, kir_ker = res in
     let info_param1 = Param(arg_type_of_vec vec_in, 0) in
     let info_param2 = SizeOf(0) in
     let info_param3 = NewSizedVec(arg_type_of_vec_kind (snd k3), 1) in
     let final_kern = skel_kern_creation res (info_param1 :: info_param2 :: info_param3 :: []) in
     spoc_ker#compile ~debug:true device;
     let end_time = Unix.gettimeofday() in
     Printf.printf "transformation time %.3f\n" (end_time -. begin_time);
     let begin_exec_time = Unix.gettimeofday() in
     let param1 = VParam(vec_in) in
     let retour = (run_skel final_kern (param1 :: []) device) in
     let end_exec_time = Unix.gettimeofday() in
     Printf.printf "execution time %.3f\n" (end_exec_time -. begin_exec_time);
     (match retour with
     | VRetour(x) -> Spoc.Mem.get x 0
     | _ -> assert false)
  | _ -> failwith "malformed Kernel"


let stencil_skel =
  let params = params (concat (new_int_vec_var (0) "a")
			 (concat (new_int_var (1) "ray")
			    (concat (new_int_var (2) "n")
			       (concat (new_int_vec_var (3) "b")
				  (empty_arg())))))
  in
  let cu_index = "blockIdx.x * blockDim.x + threadIdx.x" in
  let cl_index = "get_global_id(0)" in
  let cu_thread_x = "threadIdx.x" in
  let cu_blockDim = "blockDim.x" in
  let cl_blockDim = "get_local_size(0)" in
  let cl_thread_x = "get_local_id(0)" in
  let id_a = IntId("a", (0)) in
  let id_b = IntId("b", (3)) in
  let id_ray = IntId ("ray", (1)) in
  let id_n = IntId ("n", (2)) in
  let id_tmp = IntId("spoc_var0", (4)) in
  let id_si = IntId("s_index", (5)) in
  let id_x = IntId("id_x", (6)) in
  let id_l = IntId ("id_l", (7)) in
  let id_res = IntId ("result", (8)) in
  let id_i = IntId ("i", (9)) in
  let block_dim = Intrinsics ((cu_blockDim, cl_blockDim)) in
  let skel_args = Skel (Concat (id_res, (Concat (IntVecAcc (id_tmp, Plus (id_si, id_i)), empty_arg()))), id_res) in
  let body = Local ( Local (
    Decl (IdName ("tmp")),
    Local (
      Decl(new_int_var (5) "s_index"),
      Local (
	Decl (new_int_var (6) "id_x"),
	Local (
	  Decl (new_int_var (8) "result"),
	  Local (
	    Decl (new_int_var (7) "id_l"),
	    Local (
	      Decl (new_int_var (9) "i"),
	      Seq (
		Set (id_l, Intrinsics ((cu_thread_x, cl_thread_x))), 
		Seq (
		  Set (id_si, Plus (id_ray, Intrinsics ((cu_thread_x, cl_thread_x)))),
		  Seq (
		    Set (id_x, Intrinsics ((cu_index, cl_index))),
		    Ife (LtBool(id_x, id_n),			 
			 Seq (
			   If (LtBool (id_l, id_ray),
				 Seq (
				   Ife (GtBool (Min (id_x, id_ray), Int 0),
					Acc (IntVecAcc (id_tmp, Min(id_si, id_ray)),
					     IntVecAcc (id_a, Min (id_x, id_ray))),			   
					Acc (IntVecAcc (id_tmp, Min (id_si, id_ray)),
					     IntVecAcc (id_a, Min (id_n, Plus (id_ray, id_l))))
				   ),
				   Ife (GtEBool (Plus (id_x, block_dim), id_n),
					Acc (IntVecAcc (id_tmp, Plus (id_si, block_dim)),
					     IntVecAcc (id_a, id_l)),
					Acc (IntVecAcc (id_tmp, Plus (id_si, block_dim)),
					     IntVecAcc (id_a, Plus (id_x, block_dim))
					)			   
				   )
				 )
			   ), 			 	    
			   Seq (
			     Acc (IntVecAcc (id_tmp, id_si), IntVecAcc (id_a, id_x)),
			     Seq (
			       SyncThread,
			       Seq (
				 Set (id_res, Int 0),
				 Seq (
				   Seq (
				     Set (id_i, Min( Int 0, id_ray)),
				     While (LtEBool (id_i, id_ray),
					    Seq (
					      skel_args,
					      Set (id_i, Plus (id_i, Int 1))
					    )
				     )
				   ),
				   Seq (Acc (IntVecAcc (id_b, id_x), id_res), Empty)
				 )
			       )
			     )
			   )
			 ),
			 SyncThread
		    )	    		    	 
		  )
		)
		)
	      )
	    )
	)
      )
    )      
  ),Empty
  )    
  in
  (params, body)  

let stencil ((ker: ('a, 'b, ('c -> 'd -> 'e), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init()).(0)) (vec_in: ('c, 'i) Vector.vector) (ray: int) : ('e, 'j) Vector.vector =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let begin_time = Unix.gettimeofday() in
     let a_info = ("a", type_of_param param 0, true) in
     let ray_info = ("ray", new_int_var (1) "ray", false) in
     let n_info = ("n", new_int_var (2) "n", false) in
     let b_info = ("b", fst k3, true) in
     
     (* On recupere la taille du shared (egale au nombre de threads dans un bloc) *)
     let (block, grid) = thread_creation device (Vector.length vec_in) in
     let tmp_var = ("tmp", Arr(0, (Int (block.blockX + 2 * ray)), EFloat32, Shared)) in
     let final_ast = generate_from_skel k2 stencil_skel (a_info :: ray_info :: n_info :: b_info :: []) (tmp_var :: []) in

     (* TODO : VERSION CPU (ici code du map pour exemple) *)
     let ml_kern = (let reduce = fun f k a b ->
       let c = Vector.create k (Vector.length a) in
       for i = 0 to (Vector.length a - 2) do
	 Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get a (i + 1)))
       done;
       c
		    in reduce (k1) (snd k3)) in
     
     let res = res_creation ker ml_kern k1 final_ast k3 in
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in
     
     ignore(gen ~only:target res); 
     
     let open Kernel in
     let spoc_ker, kir_ker = res in
     let info_param1 = Param (arg_type_of_vec vec_in, 0) in
     let info_param2 = Param (Int32, 1) in
     let info_param3 = SizeOf(0) in
     let info_param4 = NewVec(arg_type_of_vec_kind (snd k3),  0) in
     let final_kern = skel_kern_creation res (info_param1 :: info_param2 :: info_param3 :: info_param4 :: []) in
     spoc_ker#compile ~debug:true device;
     let end_time = Unix.gettimeofday() in
     Printf.printf "transformation time %.3f\n" (end_time -. begin_time);
     let begin_exec_time = Unix.gettimeofday() in
     let param1 = VParam (vec_in) in
     let param2 = IParam (ray) in
     let retour = (run_skel final_kern (param1 :: param2 :: []) device) in
     let end_exec_time = Unix.gettimeofday() in
     Printf.printf "execution time %f\n" (end_exec_time -. begin_exec_time);
     (match retour with
     | VRetour(x) -> x           
     | _ -> assert false)	  
  | _ -> failwith "malformed Kernel"
     
let reduce2_skel = 
  let params = params ( concat (new_int_vec_var (0) "a")
			  (concat (new_int_var (1) "n")
		             ( concat (new_int_vec_var (2) "b") (empty_arg()))))
  in
  let cu_start = "2 * blockIdx.x * blockDim.x" in
  let cl_start = "2 * get_group_id(0) * get_local_size(0)" in
  let cu_thread_x = "threadIdx.x" in
  let cl_thread_x = "get_local_id(0)" in
  let cu_blockDim = "blockDim.x" in
  let cl_blockDim = "get_local_size(0)" in
  let cu_blockId = "blockIdx.x" in
  let cl_blockId = "get_group_id(0)" in
  let id_a = IntId ("a",   (0)) in
  let id_b = IntId ("b",   (2)) in
  let id_n = IntId ("n",   (1)) in
  let id_tmp   = IntId ("spoc_var0", (3)) in
  let idl = IntId ("idl", (4)) in
  let id_start = IntId("start", (5)) in
  let id_stride = IntId("stride", (6)) in
  let block_dim = Intrinsics ((cu_blockDim, cl_blockDim)) in
  let block_Id = Intrinsics ((cu_blockId, cl_blockId)) in
  let vec_acc_tmp1 = IntVecAcc (id_tmp, idl) in
  let vec_acc_tmp2 = IntVecAcc (id_tmp, Plus(idl, id_stride)) in
  let skel_args = Skel (Concat (vec_acc_tmp1, (Concat (vec_acc_tmp2, empty_arg()))), IntVecAcc (id_tmp, idl)) in
  let body = Local ( Local (
    Decl (IdName("tmp")),
    Local(
      Decl (new_int_var (5) "start"),
      Local (
	Decl (new_int_var (4) "idl"),
	Seq (
	  Decl (new_int_var (6) "stride"),
	  Seq (
    	    Set (id_start , Intrinsics ((cu_start, cl_start)) ),
	    Seq (
	      Set (id_stride, block_dim),
	      Seq (
		Set (idl, Intrinsics ((cu_thread_x, cl_thread_x))),
		Seq ( Ife (LtBool (Plus (id_start, idl), id_n),
			   Seq (Acc (IntVecAcc (id_tmp, idl), IntVecAcc (id_a, Plus (idl, id_start))),
				Empty),
			   Seq (Acc (IntVecAcc (id_tmp, idl), Int (0)),
				Empty)
		),
		      Seq (
			Ife (LtBool(Plus( id_start , Plus (idl, block_dim)),
				    id_n),
			     Seq (Acc (IntVecAcc (id_tmp, Plus (idl, block_dim)), IntVecAcc (id_a, Plus(id_start, Plus (idl, block_dim)))
			     ),
				  Empty),
			     Seq (Acc (IntVecAcc (id_tmp, Plus (idl, block_dim)), Int(0)), Empty)
			),
			Seq (
			  While (GtBool (id_stride, Int(0)),
				 Seq (
				   SyncThread,
				   Seq (
				     If (LtBool(idl, id_stride),
					 Seq (skel_args, Empty)
				     ),
				     Set (id_stride, RightBit (id_stride, Int (1)))
				   )
				 )
			  ),
			  Seq (
			    If (EqBool (idl, Int(0)),
				Seq (Acc(IntVecAcc (id_b, block_Id), IntVecAcc (id_tmp, Int(0))), Empty) 
			    ), Empty
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
  ), Empty
  )
  in
  (params, body)
    
    
let reduce ((ker: ('a, 'b, ('c -> 'd -> 'e), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'i) Vector.vector) : 'e  =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let begin_time = Unix.gettimeofday() in
     let a_info = ("a", type_of_param param 0, true) in
     let n_info = ("n", new_int_var (0) "n", false) in
     let b_info = ("b", fst k3, true) in
     
     (* On recupere la taille du shared (egale au nombre de threads dans un bloc) *)
     let (block, grid) = thread_creation_pow2 device (Vector.length vec_in) in
     let tmp_var = ("tmp", Arr(0, (Int (block.blockX * 2)), EFloat32, Shared)) in
     let final_ast = generate_from_skel k2 reduce2_skel (a_info :: n_info :: b_info :: []) (tmp_var :: []) in

     (* TODO : VERSION CPU (ici code du map pour exemple) *)
     let ml_kern = (let reduce = fun f k a b ->
       let c = Vector.create k (Vector.length a) in
       for i = 0 to (Vector.length a - 2) do
	 Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get a (i + 1)))
       done;
       c
		    in reduce (k1) (snd k3)) in
     
     let res = res_creation ker ml_kern k1 final_ast k3 in
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in
     
     ignore(gen ~only:target res); 
     
     let open Kernel in
     let spoc_ker, kir_ker = res in
     let info_param1 = Param(arg_type_of_vec vec_in, 0) in
     let info_param2 = SizeOf(0) in
     let info_param3 = NewSizedVec(arg_type_of_vec_kind (snd k3), grid.gridX) in
     let final_kern = skel_kern_creation res (info_param1 :: info_param2 :: info_param3 :: []) in
     spoc_ker#compile ~debug:true device;
     let end_time = Unix.gettimeofday() in
     Printf.printf "transformation time %.3f\n" (end_time -. begin_time);
     let begin_exec_time = Unix.gettimeofday() in
     let param1 = VParam(vec_in) in
     let retour = (run_skel_pow2 final_kern (param1 :: []) device) in
     let end_exec_time = Unix.gettimeofday() in
     Printf.printf "execution time %f\n" (end_exec_time -. begin_exec_time);
     (match retour with
     | VRetour(x) ->	
	let param1 = VParam(x) in
	let final_retour = (run_skel_pow2 final_kern (param1 :: []) device) in
	(match final_retour with
	  VRetour(z) -> Spoc.Mem.get z 0
	| _ -> assert false)      
     | _ -> assert false)	  
  | _ -> failwith "malformed Kernel"

        
let bitonic_sort_skel =
  let params = Params (concat (new_int_vec_var (0) "a")			 
			 (concat (new_int_var (1) "j")
			    ( concat (new_int_var (2) "k")
				(concat (new_int_var (6) "n")
				   (empty_arg())))))
  in
  let cu_thread = "blockIdx.x * blockDim.x + threadIdx.x" in
  let cl_thread = "get_global_id(0)" in
  let id_x = IntId("id_x", (3)) in
  let id_a = IntId ("a", (0)) in
  let id_j = IntId ("j", (1)) in
  let id_k = IntId ("k", (2)) in
  let id_ixj = IntId ("ixj", (4)) in
  let id_n = IntId ("n", (6)) in
  let id_tmp = IntId ("tmp", (5)) in
  let id_test = IntId ("test", (7)) in
  
  let skel_args = Skel (Concat (IntVecAcc (id_a, id_x), (Concat (IntVecAcc(id_a, id_ixj), empty_arg()))), id_test) in
  let body = Local (Local ( Decl (new_int_var (3) "id_x"),Local (Decl (new_int_var (4) "ixj"), Local (Decl (IdName ("tmp")),
	Local (Decl (BoolVar (7, "test")),
	  Seq (Set (id_x, Intrinsics ((cu_thread, cl_thread))),
	    If (LtBool (id_x, id_n),
		Seq (Set (id_ixj, ExpBit (id_x, id_j)),
		  If (And (GtEBool (id_ixj, id_x),
			   LtBool (id_ixj, id_n)),
		      Seq ( skel_args, Seq (
			  If(And (EqBool (AndBit(id_x, id_k), Int 0), Not (id_test)),
			     Seq (
			       Set (id_tmp, IntVecAcc (id_a, id_x)),
			       Seq (
				 Acc (IntVecAcc (id_a, id_x), IntVecAcc(id_a, id_ixj)),
				 Acc (IntVecAcc (id_a, id_ixj), id_tmp)			     
			       )
			     )			  
			  ),
			  If(And (Not(EqBool( AndBit(id_x, id_k), Int 0)), id_test),
			     Seq (Set (id_tmp, IntVecAcc (id_a, id_x)),
			       Seq (
				 Acc (IntVecAcc (id_a, id_x), IntVecAcc (id_a, id_ixj)),
				 Acc (IntVecAcc (id_a, id_ixj), id_tmp)
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
  ), Empty
  )
  in
  (params, body)
    
let sort ((ker: ('a, 'b, ('c -> 'c -> 'd), 'f, 'g) sarek_kernel)) ?dev:(device=(Spoc.Devices.init ()).(0)) (vec_in : ('c, 'i) Vector.vector) =
  let ker2, k = ker in
  let (k1, k2, k3) = (k.ml_kern, k.body, k.ret_val) in
  match k2 with
  | Kern (param, body) ->
     (* Recuperation d'un ast squelette de map*)
     let begin_time = Unix.gettimeofday() in
     let a_info = ("a", type_of_param param 0, true) in
     let j_info = ("j", new_int_var (1) "j", false) in
     let k_info = ("k", new_int_var (2) "k", false) in
     let n_info = ("n", new_int_var (3) "n", false) in

     let trans = ("tmp", a_to_simple_name_int (type_of_param param 0) "tmp" (7) ) in
     let final_ast = generate_from_skel k2 bitonic_sort_skel (a_info :: n_info :: j_info :: k_info :: []) (trans :: []) in     
     let ml_kern = (let reduce = fun f k a b ->
       let c = Vector.create k (Vector.length a) in
       for i = 0 to (Vector.length a - 2) do
	 Mem.unsafe_set c i ( f (Mem.unsafe_get a i) (Mem.unsafe_get a (i + 1)))
       done;
       c
		    in reduce (k1) (snd k3)) in
     
     let res = res_creation ker ml_kern k1 final_ast k3 in
     let target =
       match device.Devices.specific_info with
       | Devices.CudaInfo _ -> Devices.Cuda
       | Devices.OpenCLInfo _ -> Devices.OpenCL in
     
     ignore(gen ~only:target res); 
     
     let open Kernel in
     let spoc_ker, kir_ker = res in
     let info_param1 = Param(arg_type_of_vec vec_in, 0) in
     let info_param2 = Param(Int32, 1) in
     let info_param3 = Param(Int32, 2) in
     let info_param4 = SizeOf(0) in
     
     let final_kern = skel_kern_creation res (info_param1 :: info_param2 :: info_param3 :: info_param4 :: []) in
     spoc_ker#compile ~debug:true device;
     let end_time = Unix.gettimeofday() in
     Printf.printf "transformation time %.3f\n" (end_time -. begin_time);
     let begin_exec_time = Unix.gettimeofday() in
     let n = Vector.length vec_in in
     let k = ref 2 in
     while !k <= n do
       let j = ref (!k lsr 1) in
       while !j > 0 do
	 let param1 = VParam (vec_in) in
	 let param2 = IParam (!j) in
	 let param3 = IParam (!k) in
	 run_skel_pow2 final_kern (param1 :: param2 :: param3 :: []) device;
	 j := !j lsr 1
       done;
       k := (!k lsl 1)
     done;
     
     let end_exec_time = Unix.gettimeofday() in
     Printf.printf "execution time %f\n" (end_exec_time -. begin_exec_time)     
  | _ -> failwith "malformed Kernel"
	  

     
let mapReduce ((ker1: ('a, 'b, ('c -> 'd), 'f, 'g) sarek_kernel)) (ker2: ('h, 'i, ('d -> 'k -> 'l), 'm, 'n) sarek_kernel) ?dev:(device=(Spoc.Devices.init()).(0)) (vec_in: ('c, 'o) Vector.vector) : 'l =
  let map_res = map ker1 vec_in in
  let reduce_res = reduce (ker2) map_res in
  reduce_res

let map2Reduce ((ker1: ('a, 'b, ('c -> 'd -> 'p), 'f, 'g) sarek_kernel)) (ker2: ('h, 'i, ('p -> 'k -> 'l), 'm, 'n) sarek_kernel) ?dev:(device=(Spoc.Devices.init()).(0)) (vec_in1: ('c, 'o) Vector.vector) (vec_in2: ('d, 'q) Vector.vector) : 'l =
  let map_res = map2 ker1 vec_in1 vec_in2 in
  let reduce_res = reduce (ker2) map_res in
  reduce_res
  
    
