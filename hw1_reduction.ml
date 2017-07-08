open Hw1;;

module IntMap = Map.Make(struct type t = int let compare = compare end);;
module StringMap = Map.Make(String);;
module StringSet = Set.Make(String);;

(*let rec free_vars x = match x with 
	Var a -> [a]
	| App (a, b) -> (free_vars a) @ (free_vars b)
	| Abs (a, b) -> 
		let predicate character = not (a = character) in
				List.filter predicate (free_vars b);;*)

let free_vars x = 
	let rec free_vars' x was set lst = match x with
		Var var -> if StringSet.mem var was then (set, lst) else if StringSet.mem var set then (set, lst) else (StringSet.add var set, var :: lst) 
		| App (a, b) -> 
			let set, lst = free_vars' a was set lst in
				free_vars' b was set lst
		| Abs (var, last) -> 
			free_vars' last (StringSet.add var was) set lst in
		snd (free_vars' x StringSet.empty StringSet.empty []);;

let rec substitute a b x = match a with
	Var v -> if x = v then b else Var v
	| Abs (v, d) -> if x = v then Abs (v, d) else Abs (v, (substitute d b x))
	| App (c, d) -> App ((substitute c b x), (substitute d b x));;

let rec normal_beta_reduction_with_bool a = match a with
	Var x -> (Var x, false)
	| Abs (x, p) -> let res = normal_beta_reduction_with_bool p in
		(Abs (x, (fst res)), (snd res))
	| App (p, q) -> match p with
		Abs (v, p) -> (substitute p q v, true)
		| _ -> match (normal_beta_reduction_with_bool p) with
			(res, true) -> (App (res, q), true)
			| (res, false) -> 
				let first, second = normal_beta_reduction_with_bool q in
					(App (res, first), second);;

let normal_beta_reduction a = fst (normal_beta_reduction_with_bool a);;

let rec make_unique a prev equal = match prev with
	None -> (match a with 
		[] -> []
		| a :: last -> a :: (make_unique last (Some a) equal))
	| Some el -> match a with
		[] -> []
		| a :: last -> if (equal a el) then (make_unique last prev equal) else a :: (make_unique last (Some a) equal);;

let unique a = 
	let my_comp c d = (String.compare c d) < 0 in
		make_unique (merge_sort_with_comp a my_comp) None (=);;

let rec get_all_dependences_rec a v = match a with
		Var var -> 
			if var = v then ([], true) else ([], false)
		| App (a, b) -> 
			let lst1, val1 = (get_all_dependences_rec a v) and lst2, val2 = (get_all_dependences_rec b v) in
				(lst1 @ lst2, val1 || val2)
		| Abs (var, b) -> if var = v then ([], false) else	
			let res = get_all_dependences_rec b v in
				if (snd res) = true then (var :: (fst res), true) else res;;

let get_all_dependences a v = fst (get_all_dependences_rec a v);;

let rec has_same_rec a b comp = match a, b with
	q :: alast, w :: blast -> 
		(let res = comp q w in
			if res == 0 then true else if res < 0 then (has_same_rec alast b comp) else (has_same_rec a blast comp))
	| _, _ -> false;;

let has_same a b =  
	let first = unique a and second = unique b in
		has_same_rec first second (String.compare);;

let free_to_subst a b v = 
	let my_free_vars = free_vars b and dependences = get_all_dependences a v in
		not (has_same my_free_vars dependences);;

let rec is_normal_form a = match a with
	Var x -> true
	| Abs (a, b) -> is_normal_form b
	| App (a, b) -> match a with 
		Abs (c, d) -> false
		| _ -> (is_normal_form a) && (is_normal_form b);;

let rec is_alpha_equivalent a b = match a, b with
	Abs(e, q), Abs (c, d) -> is_alpha_equivalent (substitute q (Var c) e) d
	| App (a, b), App (c, d) -> (is_alpha_equivalent a c) && (is_alpha_equivalent b d)
	| Var a, Var b -> a = b
	| _, _ -> false;;

type graph_lambda = GApp of int * int | GAbs of string * int | GVar of string;;

let rec from_lambda_to_graph_impl l map new_node used_nodes = 
	let lambda_name = string_of_lambda1 l in
		if (StringMap.mem lambda_name used_nodes) = true then 
			(StringMap.find lambda_name used_nodes, map, used_nodes, new_node) 
		else
			match l with
				App (a, b) -> 
					let number1, map, used_nodes, new_node = from_lambda_to_graph_impl a map new_node used_nodes in
						let number2, map, used_nodes, new_node = from_lambda_to_graph_impl b map new_node used_nodes in
							let new_map = IntMap.add new_node (GApp (number1, number2)) map in
								let new_used_nodes = StringMap.add lambda_name new_node used_nodes in
									(new_node, new_map, new_used_nodes, new_node + 1)
				| Abs (a, b) -> 
					let number, map, used_nodes, new_node = from_lambda_to_graph_impl b map new_node  used_nodes in
						let new_map = IntMap.add new_node (GAbs (a, number)) map in
							let new_used_nodes = StringMap.add lambda_name new_node used_nodes in
								(new_node, new_map, new_used_nodes, new_node + 1)
				| Var a -> 
					let new_map = IntMap.add new_node (GVar a) map in
						let new_used_nodes = StringMap.add lambda_name new_node used_nodes in
							(new_node, new_map, new_used_nodes, new_node + 1);;

let from_lambda_to_graph l = 
	let index, map, _, new_node = from_lambda_to_graph_impl l IntMap.empty 0 StringMap.empty in
		(index, map, new_node);;

let rec from_graph_to_lambda index map = match IntMap.find index map with
	GApp (first, second) -> App (from_graph_to_lambda first map, from_graph_to_lambda second map) 
	| GAbs (var, last) -> Abs (var, from_graph_to_lambda last map)
	| GVar var -> Var var;;

let rec substitute_in_node_internal what substitution new_index v map used_nodes = 
	if (IntMap.mem what used_nodes) = true then 
		let res, sub = IntMap.find what used_nodes in
			(res, new_index, map, used_nodes, sub) 
	else
		match IntMap.find what map with
			GApp (first, second) -> 
				let fresult, new_index, map, used_nodes, fsub = substitute_in_node_internal first substitution new_index v map used_nodes in
					let sresult, new_index, map, used_nodes, ssub = substitute_in_node_internal second substitution new_index v map used_nodes in
						if fsub = true || ssub = true then 
							let new_map = IntMap.add new_index (GApp (fresult, sresult)) map in
								let new_used_nodes = IntMap.add what (new_index, true) used_nodes in
									(new_index, new_index + 1, new_map, new_used_nodes, true)
						else
							let new_used_nodes = IntMap.add what (what, false) used_nodes in
								(what, new_index, map, new_used_nodes, false)
			| GAbs (var, last) -> 
				if var = v then 
					let new_used_nodes = IntMap.add what (what, false) used_nodes in
						(what, new_index, map, new_used_nodes, false)
				else
					let result, new_index, map, used_nodes, sub = substitute_in_node_internal last substitution new_index v map used_nodes in
						if sub = true then
							let new_map = IntMap.add new_index (GAbs (var, result)) map in
								let new_used_nodes = IntMap.add what (new_index, true) used_nodes in
									(new_index, new_index + 1, new_map, new_used_nodes, true)
						else
							let new_used_nodes = IntMap.add what (what, false) used_nodes in
								(what, new_index, map, new_used_nodes, false)
			| GVar var -> 
				if var = v then 
					let new_used_nodes = IntMap.add what (substitution, true) used_nodes in		
						(substitution, new_index, map, new_used_nodes, true)
				else
					let new_used_nodes = IntMap.add what (what, false) used_nodes in
						(what, new_index, map, new_used_nodes, false);;

let substitute_in_node what substitution new_index v map = 
	let result, new_index, map, _, _ = substitute_in_node_internal what substitution new_index v map IntMap.empty in
		(result, new_index, map);;

let rename_vars l  = 
	let rec rename_vars' term map new_var = 
		match term with
			Var var -> if StringMap.mem var map then (Var ("V" ^ (string_of_int (StringMap.find var map))), new_var) else (Var var, new_var)
			| Abs (var, last) -> 
				let result, new_new_var = rename_vars' last (StringMap.add var new_var map) (new_var + 1) in
					(Abs ("V" ^ (string_of_int new_var), result), new_new_var)
			| App (fst, snd) -> 
				let fresult, new_var = rename_vars' fst map new_var in
					let sresult, new_var = rename_vars' snd map new_var in
						(App (fresult, sresult), new_var) in
		fst (rename_vars' l StringMap.empty 0);;

let reduce_to_normal_form a = 
	let initial_index, initial_map, new_index = from_lambda_to_graph (rename_vars a) in
		let rec big_loop index map new_index = 
			match IntMap.find index map with
				GApp (first, second) -> 
					let rec loop first map new_index = 
						match IntMap.find first map with
							GAbs (var, last) -> 
								let last, new_index, map = substitute_in_node last second new_index var map in
									((last, map), true, new_index)
							| _ -> 
								let (child, map), result, new_index = big_loop first map new_index in
									if result = true then 
										loop child map new_index 
									else 
										((child, map), result, new_index) in
					let (fchild, map), result, new_index = loop first map new_index in 
						if result = true then 
							((fchild, map), result, new_index) 
						else
							let rec second_loop second map new_index = 
								let (schild, map), result, new_index = big_loop second map new_index in
									if result = true then 
										second_loop schild map new_index 
									else
										let new_map = IntMap.add index (GApp (fchild, schild)) map in
											((index, new_map), false, new_index) in
								second_loop second map new_index
				| GAbs (var, last) -> 
					let rec loop last map new_index = 
						let (child, map), result, new_index = big_loop last map new_index in
							if result = true then 
								loop child map new_index 
							else
								let new_map = IntMap.add index (GAbs (var, child)) map in
									((index, new_map), false, new_index) in
						loop last map new_index
				| GVar var -> ((index, map), false, new_index) in
			let rec loop index map new_index = 
				let (child, map), result, new_index = big_loop index map new_index in
					if result = true then 
						loop child map new_index 
					else 
						from_graph_to_lambda child map in
				loop initial_index initial_map new_index;;

(*let reduce_to_normal_form a = 
	let initial_index, initial_map, new_node = from_lambda_to_graph a in
		let rec normal_beta_reduction_graph index map new_node = 
			(*print_string ((string_of_int index) ^ "\n");*)
			match IntMap.find index map with
				GApp (first, second) -> (match IntMap.find first map with
					GAbs (var, last) -> 
						let last_copy, new_node, map = copy_node last second new_node var map in
							(last_copy, map, new_node, true)
					| _ -> 
						let new_index, map, new_node, result = normal_beta_reduction_graph first map new_node in
							if result = true then 
								let new_map = IntMap.add index (GApp (new_index, second)) map in
									(index, new_map, new_node, true)
							else
								let new_index, map, new_node, result = normal_beta_reduction_graph second map new_node in
									let new_map = IntMap.add index (GApp (first, new_index)) map in
										(index, new_map, new_node, result))
				| GAbs (var, last) -> 
					let new_index, map, new_node, result = normal_beta_reduction_graph last map new_node in
						let new_map = IntMap.add index (GAbs (var, new_index)) map in
							(index, new_map, new_node, result)
				| GVar var -> 
					(index, map, new_node, false) in
			let rec loop index map new_node =
				(*print_string ((string_of_lambda (from_graph_to_lambda index map)) ^ "\n\n");*)
				let new_index, map, new_node, result = normal_beta_reduction_graph index map new_node in
					if result = true then
						loop new_index map new_node
					else
						from_graph_to_lambda new_index map in
				loop initial_index initial_map new_node;;*)