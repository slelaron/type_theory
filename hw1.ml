type peano = Z | S of peano;;
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;
type 'a list = Nil | Cons of 'a * 'a list;;

let peano_of_int x = failwith "Not implemented";;

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
		let two_parts = get_two_parts x in
		let first = match two_parts with (a, b) -> a in
		let second = match two_parts with (a, b) -> b in
			merge (merge_sort first) (merge_sort second) Nil;;

                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;