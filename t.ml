open Hw1;;
(*
print_int (int_of_peano (S (S (Z))));;
*)
(*print_int (int_of_peano (div (peano_of_int 1000) (peano_of_int 3)));;*)
print_string (string_of_list (Cons ((peano_of_int 1), Cons ((peano_of_int 2), Cons ((peano_of_int 3), Nil)))));;
print_string (string_of_list (rev (Cons ((peano_of_int 1), Cons ((peano_of_int 2), Cons ((peano_of_int 3), Nil))))));;

print_string (string_of_list_int (merge_sort (Cons (1, Cons (8, Cons (7, Cons(6, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Nil))))))))))));

print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x x") ^ "\n");;

print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\f.(\\x.f(x x))(\\x.f (x x))") ^ "\n");;