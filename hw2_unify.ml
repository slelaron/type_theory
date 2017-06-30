type algebraic_term = Var of string | Fun of string * (algebraic_term list)

let system_to_equation x = failwith "Not implemented";;
let apply_substitution x y = 
	let rec func a b = 
		let str, term = b in
			match a with
				Var v -> if v = str then term else Var v
				| Fun (name_of_func, term_list) -> Fun (name_of_func, List.fold_left (::) [] (List.map func term_list b)) in
	List.fold_left func y x;;

let check_solution x y = failwith "Not implemented";;

(*
let get_new_equation x y = match x y with
	Var a, Var b -> *)

let solve_system x = failwith "Not implemented";;