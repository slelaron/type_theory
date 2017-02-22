type peano = Z | S of peano;; (* òèïû íåîáõîäèìî êîïèðîâàòü â ðåàëèçàöèþ *)
type lambda = Var of string | Abs of string * lambda | App of lambda * lambda;;

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
                     
let rev x = failwith "Not implemented";;
let merge_sort x = failwith "Not implemented";;
                     
let string_of_lambda x = failwith "Not implemented";;
let lambda_of_string x = failwith "Not implemented";;