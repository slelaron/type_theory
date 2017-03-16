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
	Nil -> y
	| Cons (a, b) -> reverse b (Cons (a, y));;

let rec string_of_list x = match x with 
	Nil -> "\n"
	| Cons (a, b) -> string_of_int (int_of_peano a) ^ " " ^ (string_of_list b);;

let rec string_of_list_int x = match x with
	Nil -> "\n"
	| Cons (a, b) -> string_of_int (a) ^ " " ^ (string_of_list_int b);;

let rev x = reverse x Nil;;

let rec list_length x = match x with
	Nil -> 0
	| Cons (a, b) -> (list_length b) + 1;;

let rec divide_in_the_middle lst another cnt size = match lst with
	Cons (a, b) -> if cnt < size / 2 then divide_in_the_middle b (Cons (a, another)) (cnt + 1) size else ((rev another), lst)
	| Nil -> (Nil, Nil)

let get_two_parts lst = divide_in_the_middle lst Nil 0 (list_length lst);;

let rec merge a b result = match a, b with
	Nil, Nil -> (rev result)
	| Nil, Cons (c, d) -> merge Nil d (Cons (c, result))
	| Cons (c, d), Nil -> merge d Nil (Cons (c, result))
	| Cons (la, lb), Cons (ra, rb) -> if la < ra then merge lb b (Cons (la, result)) else merge a rb (Cons (ra, result));;

let rec merge_sort x = match x with 
	Nil -> Nil
	| Cons (a, Nil) -> x
	| _ -> 
		let (a, b) = get_two_parts x in
			merge (merge_sort a) (merge_sort b) Nil;;

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