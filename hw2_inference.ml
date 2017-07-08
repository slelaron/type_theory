open Hw1;;
open Hw2_unify;;

type simp_type = S_Elem of string | S_Arrow of simp_type * simp_type

let rec algebraic_term_of_simp_type x = match x with
	S_Elem var -> Var var
	| S_Arrow (fst, snd) -> Fun ("Arrow", (algebraic_term_of_simp_type fst) :: (algebraic_term_of_simp_type snd) :: []);;	

let rec simp_type_of_algebraic_term x = match x with
	Var var -> S_Elem var
	| Fun ("Arrow", fst :: snd :: []) -> S_Arrow (simp_type_of_algebraic_term fst, simp_type_of_algebraic_term snd)
	| _ -> failwith "Can't convert";;

module StringMap = Map.Make(String);;
module IntMap = Map.Make(struct type t = int let compare = compare end);;
module StringSet = Set.Make(String);;	

let get_free_vars term = 
	let rec get_free_vars' term was added added_list = 
		match term with
			App (a, b) -> 
				let added, added_list = get_free_vars' a was added added_list in
					get_free_vars' b was added added_list
			| Var a -> if StringSet.mem a was then (added, added_list) else (StringSet.add a added, a :: added_list)
			| Abs (var, last) -> 
				get_free_vars' last (StringSet.add var was) added added_list in
		snd (get_free_vars' term StringSet.empty StringSet.empty []);;

let infer_simp_type x = 
	let rec infer_simp_type' x map free_id id = 
		match x with
			Abs (var, term) -> 
				let index = if StringMap.mem var free_id then StringMap.find var free_id else 0 in
					let new_map = StringMap.add var index map in
						let new_free_id = StringMap.add var (index + 1) free_id in
							let sys, t, new_free_id, id = infer_simp_type' term new_map new_free_id id in
								(sys, S_Arrow (S_Elem ("L" ^ var ^ (string_of_int index)), t), new_free_id, id)
			| App (fterm, sterm) ->
				let now_id = id in
					let fsys, ftype, new_free_id, id = infer_simp_type' fterm map free_id (id + 1) in
						let ssys, stype, new_free_id, id = infer_simp_type' sterm map new_free_id id in
							let new_var = S_Elem ("M" ^ (string_of_int now_id)) in
								((ftype, (S_Arrow (stype, new_var))):: (fsys @ ssys), new_var, new_free_id, id)
			| Var var -> 
				([], S_Elem ("L" ^ var ^ (string_of_int (StringMap.find var map))), free_id, id) in
		let free_vars = get_free_vars x in
			let initial_map init = List.fold_left (fun prev now -> StringMap.add now init prev) StringMap.empty free_vars in	
				let (simple_system, res_type, upper_layer, _) = infer_simp_type' x (initial_map 0) (initial_map 1) 0 in
					let first, second = List.split simple_system in
						let after_first = List.map algebraic_term_of_simp_type first 
						and after_second = List.map algebraic_term_of_simp_type second in
							let system = List.combine after_first after_second in
								let solution = solve_system system in
									match solution with
										Some solution ->
											let solved_type = simp_type_of_algebraic_term (apply_substitution solution (algebraic_term_of_simp_type res_type)) in
												let map_of_solution = List.fold_left (fun prev (key, value) -> StringMap.add key value prev) StringMap.empty solution in
													let solution_for_free = 
														List.map 
															(fun now -> 
																let str_now = "L" ^ now ^ "0" in
																	if StringMap.mem str_now map_of_solution then
																		(now, simp_type_of_algebraic_term (StringMap.find str_now map_of_solution))
																	else
																		(now, S_Elem str_now))
															free_vars in
														Some (solution_for_free, solved_type)
										| None -> None;;

let rec string_of_simple_type tp = match tp with	
	S_Elem elem -> elem 
	| S_Arrow (fst, snd) -> "(" ^ (string_of_simple_type fst) ^ "->" ^ (string_of_simple_type snd) ^ ")";;

let check_infer_simple_type l = 
	let result = infer_simp_type l in
		match result with
			None -> "Bad lambda"
			| Some (solution, solved_type) -> 
				"Type of lambda: " ^ (string_of_simple_type solved_type) ^ "\n" ^
				"Free variables: \n" ^ (String.concat "\n" (List.map (fun (name, tp) -> name ^ ": " ^ (string_of_simple_type tp)) solution));;

print_string ((check_infer_simple_type (lambda_of_string "\\a.\\b.\\c.\\d.a (b (c d))")) ^ "\n");;
print_string ((check_infer_simple_type (lambda_of_string "\\s.\\t.s (t (\\p.\\q.q)(\\r.\\s.r))t")) ^ "\n");;

type hm_lambda = HM_Var of string | HM_Abs of string * hm_lambda | HM_App of hm_lambda * hm_lambda | HM_Let of string * hm_lambda * hm_lambda
type hm_type = HM_Elem of string | HM_Arrow of hm_type * hm_type | HM_ForAll of string * hm_type

let rec algebraic_term_of_hm_type x = match x with
	HM_Elem var -> Var var
	| HM_Arrow (fst, snd) -> Fun ("Arrow", (algebraic_term_of_hm_type fst) :: (algebraic_term_of_hm_type snd) :: [])
	| HM_ForAll (var, last) -> Fun ("ForAll", (Var var) :: (algebraic_term_of_hm_type last) :: []);;

let rec hm_type_of_algebraic_term x = match x with
	Var var -> HM_Elem var
	| Fun ("Arrow", fst :: snd :: []) -> HM_Arrow (hm_type_of_algebraic_term fst, hm_type_of_algebraic_term snd)
	| Fun ("ForAll", (Var var) :: last :: []) -> HM_ForAll (var, hm_type_of_algebraic_term last)
	| _ -> failwith "Can't convert";;

let map_of_list lst = 
	List.fold_left (fun prev (key, value) -> StringMap.add key value prev) StringMap.empty lst;;

let apply_substitution_to_hm_type map tp =
	let new_map = StringMap.map algebraic_term_of_hm_type map in
		hm_type_of_algebraic_term (apply_substitution_with_map new_map (algebraic_term_of_hm_type tp));;

let sub_composition sub1 sub2 = 
	StringMap.fold (fun key value prev -> StringMap.add key value prev) sub2 (StringMap.map (fun x -> apply_substitution_to_hm_type sub2 x) sub1);;

let rec substitute_in_hm_type x map new_var = match x with
	HM_ForAll (var, last) -> substitute_in_hm_type last (StringMap.add var (HM_Elem ("V" ^ (string_of_int new_var))) map) (new_var + 1)
	| another -> (apply_substitution_to_hm_type map another, new_var);;

let substitute_in_context context map = StringMap.map (fun x -> apply_substitution_to_hm_type map x) context;;

let get_hm_free_vars tp =
	let rec get_hm_free_vars' tp was answer lst = match tp with
		HM_Elem elem -> if StringSet.mem elem was then (answer, lst) else if StringSet.mem elem answer then (answer, lst) else (StringSet.add elem answer, elem :: lst)
		| HM_Arrow (fst, snd) -> 
			let (answer, lst) = get_hm_free_vars' fst was answer lst in
				get_hm_free_vars' snd was answer lst
		| HM_ForAll (var, last) ->
			get_hm_free_vars' last (StringSet.add var was) answer lst in
	snd (get_hm_free_vars' tp StringSet.empty StringSet.empty []);;

let add_quantifiers tp context = 
	let free_in_context = 
		List.fold_left 
			(fun prev now -> StringSet.add now prev)
			StringSet.empty 
			(List.concat (List.map get_hm_free_vars (snd (List.split (StringMap.bindings context))))) in
		let free_vars = get_hm_free_vars tp in
			let good_list = List.filter (fun x -> not (StringSet.mem x free_in_context)) free_vars in
				List.fold_left (fun prev now -> HM_ForAll (now, prev)) tp good_list;;

let algorithm_w x = 
	let rec algorithm_w' term context new_var =
		match term with
			HM_Var var -> 
				let type_of_var = StringMap.find var context in
					let new_type, new_var = substitute_in_hm_type type_of_var StringMap.empty new_var in
						Some (StringMap.empty, new_type, new_var)
			| HM_App (fst, snd) ->
				let fresult = algorithm_w' fst context new_var in
					(match fresult with
						None -> None
						| Some (fsub, ftype, new_var) ->
							let sresult = algorithm_w' snd (substitute_in_context context fsub) new_var in
								match sresult with 
									None -> None
									| Some (ssub, stype, new_var) -> 
										let now_var = HM_Elem ("V" ^ (string_of_int new_var)) in
											let solution = solve_system [(algebraic_term_of_hm_type (apply_substitution_to_hm_type ssub ftype), algebraic_term_of_hm_type (HM_Arrow (stype, now_var)))] in
												match solution with
													None -> None
													| Some sol -> 
														let vsub = StringMap.map hm_type_of_algebraic_term (map_of_list sol) in
															Some (sub_composition vsub (sub_composition ssub fsub), apply_substitution_to_hm_type vsub now_var, new_var + 1))
			| HM_Abs (var, last) -> 
				let now_var = HM_Elem ("V" ^ (string_of_int new_var)) in
					let result = algorithm_w' last (StringMap.add var now_var (StringMap.remove var context)) (new_var + 1) in
						(match result with
							None -> None
							| Some (sub, tp, new_var) -> Some (sub, HM_Arrow (apply_substitution_to_hm_type sub now_var, tp), new_var))
			| HM_Let (var, fst, snd) -> 
				let fresult = algorithm_w' fst context new_var in
					match fresult with
						None -> None 
						| Some (fsub, ftype, new_var) -> 
							let fcontext = substitute_in_context (StringMap.remove var context) fsub in
								let scontext = StringMap.add var (add_quantifiers ftype (substitute_in_context fsub context)) fcontext in
									let sresult = algorithm_w' snd scontext new_var in
										match sresult with
											None -> None 
											| Some (ssub, stype, new_var) -> Some (sub_composition ssub fsub, stype, new_var) in
		let (map, new_var) = List.fold_left (fun (prev, next) now -> (StringMap.add now (HM_Elem ("V" ^ string_of_int next)) prev, next + 1)) (StringMap.empty, 0) (get_hm_free_vars x) in
			let (sub, tp, _) = algorithm_w' x map new_var in
				(sub, tp);;