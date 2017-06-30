type peano = Z | S of peano
type 'a list = Nil | Cons of 'a * 'a list

val peano_of_int: int -> peano
val int_of_peano: peano -> int
val string_of_list: peano list -> string
val string_of_list_int: int list -> string
val list_length: 'a list -> int

val inc: peano -> peano
val add: peano -> peano -> peano
val sub: peano -> peano -> peano
val mul: peano -> peano -> peano
val power: peano -> peano -> peano
val div: peano -> peano -> peano

val rev: 'a list -> 'a list
val merge_sort: 'a list -> 'a list

type lambda = Var of string | Abs of string * lambda | App of lambda * lambda

val string_of_lambda: lambda -> string
val string_of_lambda1: lambda -> string
val lambda_of_string: string -> lambda
val list_of_strings_to_string: string list -> string

val unique: string list -> string list

(* Вернуть список имён свободных переменных *)
val free_vars: lambda -> string list

(* Выполнить один шаг бета-редукции, используя нормальный порядок *)
val normal_beta_reduction: lambda -> lambda

(* Проверить свободу для подстановки. Параметры:
   что подставляем, где подставляем, вместо чего подставляем *)
val free_to_subst: lambda -> lambda -> string -> bool

(* Проверить, находится ли лямбда-выражение в нормальной форме *)
val is_normal_form: lambda -> bool

(* Проверить, альфа-эквивалентны ли лямбда-выражения *)
val is_alpha_equivalent: lambda -> lambda -> bool

(* Свести выражение к нормальной форме с использованием нормального
   порядка редукции; реализация должна быть эффективной: использовать 
   мемоизацию *)
val reduce_to_normal_form: lambda -> lambda