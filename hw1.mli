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
