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

let rec read_abs_with_buffer x iter res = match x.[iter] with
	'.' -> (res, (iter + 1))
	 | _ -> read_abs_with_buffer x (iter + 1) (res ^ Char.escaped x.[iter]);;

let read_abs x iter = read_abs_with_buffer x iter "";;

let rec read_var_with_buffer x iter res = match x.[iter] with
	' ' | '(' | ')' | '#' -> (res, iter)
	| _ -> read_var_with_buffer x (iter + 1) (res ^ Char.escaped x.[iter]);;

let rec skip_spaces x iter = match x.[iter] with
	' ' -> skip_spaces x (iter + 1)
	| _ -> iter;;

let read_var x iter = read_var_with_buffer x iter "";;

let rec lambda_of_string_another x iter = 
	let iter = skip_spaces x iter in match x.[iter] with
		'\\' ->  
			let abs_name, iter = read_abs x (iter + 1) in
				let next_lambda, iter = lambda_of_string_another x iter in
					(Abs (abs_name, next_lambda), iter)
		| _ -> 
			let rec application prev now = match prev with
				None -> Some now
				| Some prev -> Some (App (prev, now))
			and loop iter prev = 
				let iter = skip_spaces x iter in
					match x.[iter] with
					')' | '#' -> (match prev with
						Some prev -> (prev, iter)
						| None -> failwith "Empty bracket")
					| '\\' -> let next_lambda, iter = lambda_of_string_another x iter in
						loop iter (application prev next_lambda)
					| '(' -> let next_lambda, iter = lambda_of_string_another x (iter + 1) in
						loop (iter + 1) (application prev next_lambda)
					| _ -> let var, iter = read_var x iter in
						loop iter (application prev (Var var)) in
			loop iter None;;

let lambda_of_string x = (fst (lambda_of_string_another (x ^ "#") 0));;

let rec string_of_lambda x = 
	let rec string_of_lambda_wrapped_with_brackets x = match x with 
		Var a -> a
		| App (a, b) -> "("  ^ (string_of_lambda_wrapped_with_brackets a) ^ " " ^ (string_of_lambda_wrapped_with_brackets b) ^ ")"
		| Abs (a, b) -> "(\\" ^ a ^ "." ^ (string_of_lambda b) ^ ")" in 
	match x with
		Var a -> a
		| App (a, b) -> (string_of_lambda_wrapped_with_brackets a) ^ " " ^ (string_of_lambda_wrapped_with_brackets b)
		| Abs (a, b) -> "\\" ^ a ^ "." ^ (string_of_lambda b);;

let rec string_of_lambda1 x = match x with 
	Var a -> a
	| App (a, b) -> "App(" ^ (string_of_lambda1 a) ^ ", " ^ (string_of_lambda1 b) ^ ")"
	| Abs (a, b) -> "Abs(" ^ a ^  ", " ^ (string_of_lambda1 b) ^ ")";;

let rec list_of_strings_to_string b = match b with
	[] -> ""
	| a :: left -> a ^ " " ^ (list_of_strings_to_string left);;