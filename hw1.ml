type peano = Z | S of peano;;
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;
type 'a list = Nil | Cons of 'a * 'a list;;

let rec int_of_peano p = match p with
    Z -> 0
  	| S x -> 1 + int_of_peano x;;

let rec peano_of_int p = match p with
	0 -> Z
	| _ -> S (peano_of_int (p - 1));;

let inc x = S x;;

let rec add x y = match y with
	Z -> x
	| S z -> inc (add x z);;

let rec mul x y = match y with
	Z -> Z
	| S z -> add (mul x z) x;;

let rec bigger_or_equal x y = match x, y with
	Z, Z -> 1
	| z, Z -> 1
	| Z, z -> 0
	| (S x, S y) -> bigger_or_equal x y

let rec sub x y = match x, y with 
	x, Z -> x
	|S x, S y -> sub x y
	|_, _ -> failwith("x is less than y");;

let rec div x y = if bigger_or_equal x y = 1 then inc (div (sub x y) y) else Z;;

let rec power x y = match y with
	Z -> S (Z)
	| S z -> mul (power x z) x;;
                     
let rec reverse x y = match x with
	[] -> y
	| a :: b -> reverse b (a :: y);;

let rec string_of_list x = match x with 
	[] -> "\n"
	| a :: b -> string_of_int (int_of_peano a) ^ " " ^ (string_of_list b);;

let rec string_of_list_int x = match x with
	[] -> "\n"
	| a :: b -> string_of_int (a) ^ " " ^ (string_of_list_int b);;

let rev x = reverse x [];;

let rec list_length x = match x with
	[] -> 0
	| a :: b -> (list_length b) + 1;;

let rec divide_in_the_middle lst another cnt size = match lst with
	a :: b -> if cnt < size / 2 then divide_in_the_middle b (a :: another) (cnt + 1) size else ((rev another), lst)
	| [] -> ([], [])

let get_two_parts lst = divide_in_the_middle lst [] 0 (list_length lst);;

let rec merge a b comp result = match a, b with
	[], [] -> (rev result)
	| [], c :: d -> merge [] d comp (c :: result)
	| c :: d, [] -> merge d [] comp (c :: result)
	| la :: lb, ra :: rb -> if (comp la ra) then merge lb b comp (la :: result) else merge a rb comp (ra :: result);;

let rec merge_sort_with_comp x comp = match x with
	[] -> []
	| a :: [] -> x
	| _ -> 
		let (a, b) = get_two_parts x in
			merge (merge_sort_with_comp a comp) (merge_sort_with_comp b comp) comp [];;

let merge_sort x = merge_sort_with_comp x (<);;

let rec read_abs x iter res = match x.[iter] with
	'.' -> (res, iter + 1)
	| _ -> read_abs x (iter + 1) (res ^ Char.escaped x.[iter]);;

let rec read_var x iter res = match x.[iter] with
	' ' | ')' | '#' | '(' -> (res, iter)
	| _ -> read_var x (iter + 1) (res ^ Char.escaped x.[iter]);;

let rec skip_all_spaces x iter = match x.[iter] with
	' ' -> skip_all_spaces x (iter + 1)
	| _ -> iter;; 

type something = Nothing | Lambda of lambda;;

let rec lambda_of_string_another x iter prev = match x.[iter] with
	'\\' -> 
		let (result, iter) = read_abs x (iter + 1) "" in
			let iter = skip_all_spaces x iter in
				let (toAdd, iter) = lambda_of_string_another x iter Nothing in
					(Abs (result, toAdd), iter)
	| '(' -> 
		let iter = skip_all_spaces x (iter + 1) in
			let (res, iter) = lambda_of_string_another x iter Nothing in
				rec_or_ret x (iter + 1) res prev
	| _ -> 
		let (result, iter) = read_var x iter "" in
			rec_or_ret x iter (Var result) prev
and rec_or_ret x iter result prev =
	let result = match prev with
		Nothing -> result
		| Lambda a -> App(a, result) in
			let iter = skip_all_spaces x iter in
				match x.[iter] with 
					')' | '#' -> (result, iter)
					| _ -> lambda_of_string_another x iter (Lambda result);;

let lambda_of_string x = 
	let (result, last) = lambda_of_string_another (x ^ "#") 0 Nothing in
		result;;

let rec string_of_lambda x = match x with 
	Var a -> a
	| App (a, b) -> "("  ^ (string_of_lambda a) ^ " " ^ (string_of_lambda b) ^ ")"
	| Abs (a, b) -> "\\" ^ a ^ "." ^ (string_of_lambda b);;

let rec string_of_lambda1 x = match x with 
	Var a -> a
	| App (a, b) -> "App(" ^ (string_of_lambda1 a) ^ ", " ^ (string_of_lambda1 b) ^ ")"
	| Abs (a, b) -> "Abs(" ^ a ^  ", " ^ (string_of_lambda1 b) ^ ")";;

let rec filter pred lst = match lst with
	[] -> []
	| a :: left -> match (pred a) with
			true -> a :: (filter pred left)
			| _ -> filter pred left;;

let rec concat a b = match a with
	[] -> b
	| q :: w -> q :: (concat w b);;

let rec free_vars x = match x with 
	Var a -> [a]
	| App (a, b) -> (concat (free_vars a) (free_vars b))
	| Abs (a, b) -> 
		let predicate character = not (a = character) in
				filter predicate (free_vars b);;

let rec list_of_strings_to_string b = match b with
	[] -> ""
	| a :: left -> a ^ " " ^ (list_of_strings_to_string left);;

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

type 'a optional = Empty | Element of 'a;;

let rec make_unique a prev equal = match prev with
	Empty -> (match a with 
		[] -> []
		| a :: last -> a :: (make_unique last (Element a) equal))
	| Element el -> match a with
		[] -> []
		| a :: last -> if (equal a el) then (make_unique last prev equal) else a :: (make_unique last (Element a) equal);;

let unique a = 
	let my_comp c d = (String.compare c d) < 0 in
		make_unique (merge_sort_with_comp a my_comp) Empty (=);;

let rec get_all_dependences_rec a v = match a with
		Var var -> 
			if var = v then ([], true) else ([], false)
		| App (a, b) -> 
			let lst1, val1 = (get_all_dependences_rec a v) and lst2, val2 = (get_all_dependences_rec b v) in
				((concat lst1 lst2), (val1 || val2))
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

module StringMap = Map.Make(String);;

let reduce_to_normal_form a = 
	let map = ref StringMap.empty in
		let rec func a = match a with
			Var x -> (Var x, false)
			| Abs (x, p) -> let res = func1 p in
				(Abs (x, (fst res)), (snd res))
			| App (p, q) -> match p with
				Abs (v, p) -> (substitute p q v, true)
				| _ -> match (func1 p) with
					(res, true) -> (App (res, q), true)
					| (res, false) -> 
						let first, second = func1 q in
							(App (res, first), second)
		and func1 a = 
			let lambda_name = string_of_lambda a in
				let rec get_parent prev =
					let a = string_of_lambda (fst prev) in 
						if (StringMap.mem a !map) = false || a == (string_of_lambda (fst (StringMap.find a !map))) then 
							prev
						else 
							let result = get_parent (StringMap.find a !map) in
								map := StringMap.add a result !map;
								result in
					if (StringMap.mem lambda_name !map) = true then map := StringMap.add lambda_name (get_parent (StringMap.find lambda_name !map)) !map;
					if (StringMap.mem lambda_name !map) = true then print_string "True\n" else print_string "False\n";
					if (StringMap.mem lambda_name !map) = true then 
						StringMap.find lambda_name !map
					else
						let result = func a in
							map := (StringMap.add lambda_name result !map);
							result
		and loop now = 
			let result, need_else = func1 now in
				if need_else = true then loop result else result in
		loop a;;