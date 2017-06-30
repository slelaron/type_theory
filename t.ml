open Hw1;;
(*
print_int (int_of_peano (S (S (Z))));;
*)
(*print_int (int_of_peano (div (peano_of_int 1000) (peano_of_int 3)));;*)
print_string (string_of_list (Cons ((peano_of_int 1), Cons ((peano_of_int 2), Cons ((peano_of_int 3), Nil)))));;
print_string (string_of_list (rev (Cons ((peano_of_int 1), Cons ((peano_of_int 2), Cons ((peano_of_int 3), Nil))))));;

print_string (string_of_list_int (merge_sort (Cons (1, Cons (8, Cons (7, Cons(6, Cons(5, Cons(4, Cons(3, Cons(2, Cons(1, Nil))))))))))));;

print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\x.\\y.x x") ^ "\n");;

print_string (Hw1.string_of_lambda (Hw1.lambda_of_string "\\f.(\\x.f(x x))(\\x.f (x x))") ^ "\n");;

print_string ((list_of_strings_to_string (free_vars (lambda_of_string "\\x.f(x x y)"))) ^ "\n") ;;

print_string ((string_of_lambda (normal_beta_reduction(normal_beta_reduction(normal_beta_reduction(normal_beta_reduction (lambda_of_string "(\\x. x x q)(\\x. x x)")))))) ^ "\n");;

print_string ((list_of_strings_to_string (unique (Cons ("a", Cons ("b", Cons ("a", Cons ("c", Cons ("b", Nil)))))))) ^ "\n");;

print_string ((string_of_bool (free_to_subst (lambda_of_string "\\y.x") (lambda_of_string "\\t.y") "x")) ^ "\n");;

print_string ((string_of_bool (is_normal_form (lambda_of_string "\\y.\\w.\\t.(y ((\\w.w) (\\x.x x)))"))) ^ "\n");;

print_string ((string_of_bool (is_alpha_equivalent (lambda_of_string "\\y.\\w.\\t.(y ((\\w.w) (\\x.x x)))") (lambda_of_string "\\q.\\e.\\y.(q ((\\z.z) (\\a.a a)))"))) ^ "\n");;

module StringMap = Map.Make(String);;

(*print_string ((string_of_bool (StringMap.mem (lambda_of_string "\\y.y") (StringMap.add (lambda_of_string "\\y.y") (lambda_of_string "\\x.x") (StringMap.empty)))));;*)

(*print_string (string_of_lambda (reduce_to_normal_form (lambda_of_string "(\\s.\\k.\\i.(((s ((s (k s)) ((s ((s (k s)) ((s (k k)) i))) (k ((s (k (s ((s (k s)) ((s (k (s (k (s ((s ((s ((s i) (k (k (k i))))) (k ((s (k k)) i)))) (k ((s ((s (k s)) ((s (k k)) i))) (k i))))))))) ((s ((s (k s)) ((s (k k)) ((s (k s)) ((s (k (s (k ((s ((s (k s)) ((s (k k)) ((s (k s)) ((s (k k)) i))))) (k ((s ((s (k s)) ((s (k k)) i))) (k i)))))))) ((s ((s (k s)) ((s (k k)) i))) (k i))))))) (k ((s (k k)) i)))))))) ((s (k k)) ((s ((s (k s)) ((s (k k)) i))) (k i)))))))) (k (k ((s ((s (k s)) ((s (k k)) i))) ((s ((s (k s)) ((s (k k)) i))) ((s ((s (k s)) ((s (k k)) i))) (k i))))))) ((s ((s ((s (k s)) ((s (k k)) i))) (k ((s i) i)))) ((s ((s (k s)) ((s (k k)) i))) (k ((s i) i))))) ((s ((s (k s)) ((s (k (s (k s)))) ((s ((s (k s)) ((s (k (s (k s)))) ((s (k (s (k k)))) ((s ((s (k s)) ((s (k k)) i))) (k ((s (k (s (k (s i))))) ((s (k (s (k k)))) ((s (k (s i))) ((s (k k)) i)))))))))) (k (k ((s (k k)) i))))))) (k (k (k i))))) (\\x.\\y.\\z.x z (y z)) (\\x.\\y.x) (\\x.x)")));;*)

(*print_string (string_of_lambda (reduce_to_normal_form (lambda_of_string "(\\x.x x x x)((\\y.y)a)")));;*)

let reduce_check str = 
	print_string ((string_of_lambda (reduce_to_normal_form (lambda_of_string str))) ^ "\n");;

reduce_check "((\\f.(\\x.f (x x)) (\\x.f (x x))) (\\f.\\n.(\\n.n (\\x.\\x.\\y.y) \\x.\\y.x) n (\\f.\\x.f x) ((\\a.\\b.a ((\\a.\\b.\\f.\\x.a f (b f x)) b) \\f.\\x.x) n (f ((\\n.(\\p.p \\x.\\y.x) (n (\\p.\\f.f ((\\p.p \\x.\\y.y) p) ((\\n.\\f.\\x.f (n f x)) ((\\p.p \\x.\\y.y) p))) (\\f.f (\\f.\\x.x) (\\f.\\x.x)))) n))))) \\f.\\x.f (f (f (f (f (f x)))))";;

reduce_check "((\\l0.((\\l1.((\\l2.((\\l3.((\\l4.((\\l5.((\\l6.((\\l7.((\\l8.((\\l9.((\\l10.((\\l11.((\\l12.((\\f.((f (\\x.(\\l15.(x (x l15))))) (\\x.(\\l15.(x (x (x l15))))))) (\\f.(\\x.(((l0 (\\l15.(\\l16.(\\l17.(((l1 (l10 l16)) (l12 l17)) (((l1 (l10 l17)) ((l15 (l11 l16)) (\\l18.(\\l19.(l18 l19))))) ((l15 (l11 l16)) ((l15 l16) (l11 l17))))))))) f) x))))) (\\l12.(\\f.(\\x.((l12 f) (f x))))))) (\\l11.(\\l12.(\\f.(((l11 (\\x.(\\l15.(l15 (x l12))))) (\\x.f)) (\\x.x))))))) (\\l10.((l10 (\\l11.l3)) l2)))) (l0 (\\l9.(\\l10.(\\l11.((\\l12.((\\f.(((l1 l12) f) (((l1 f) l12) ((l9 (l4 l10)) (l4 l11))))) (l8 l11))) (l8 l10)))))))) (\\l8.((l8 (\\l9.l3)) l2)))) (\\l7.(\\l8.((l8 l4) l7))))) (\\l6.(\\l7.((l6 l5) l7))))) (\\l5.(\\l6.(\\l7.((l5 l6) (l6 l7))))))) (\\l4.(\\l5.(\\l6.(((l4 (\\l7.(\\l8.(l8 (l7 l5))))) (\\l7.l6)) (\\l7.l7))))))) (\\l3.(\\l4.l4)))) (\\l2.(\\l3.l2)))) (\\l1.(\\l2.(\\l3.((l1 l2) l3)))))) (\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1))))))";;

reduce_check "((\\l0.((\\l1.((\\l2.((\\l3.((\\l4.((\\l5.((\\l6.((\\l7.((\\l8.((\\l9.((\\l10.((\\l11.((\\l12.((\\l13.((\\l14.((\\l15.((\\l16.((\\l17.((\\l18.(l18 (\\l19.(\\l20.(l19 (l19 (l19 (l19 (l19 (l19 (l19 (l19 (l19 l20))))))))))))) (\\l18.(l4 (((l17 l18) (\\l19.(\\l20.l20))) l18))))) (l0 (\\l17.(\\l18.(\\l19.(\\l20.(((l1 ((l9 l19) l20)) l19) ((\\l21.(((l1 ((l16 (l14 l21)) l18)) (((l17 l18) ((l6 l21) (\\l22.(\\l23.(l22 l23))))) l20)) (((l17 l18) l19) l21))) (l15 ((l6 l19) l20))))))))))) (l0 (\\l16.(\\l17.(\\l18.((l10 (l8 l17)) (((l1 (l8 l18)) l3) ((l16 ((l7 l17) (\\l19.(\\l20.(l19 l20))))) ((l7 l18) (\\l19.(\\l20.(l19 l20))))))))))))) (l0 (\\l15.(\\l16.(((l1 (l8 (l4 l16))) (\\l17.(\\l18.l18))) ((l6 (\\l17.(\\l18.(l17 l18)))) (l15 (l4 (l4 l16)))))))))) (\\l14.(\\l15.(l14 (l14 l15)))))) (\\l13.((((l0 (\\l14.(\\l15.(\\l16.(\\l17.(((l1 (l8 l15)) l17) (((l14 (l4 l15)) l17) ((l6 l16) l17)))))))) l13) (\\l14.(\\l15.l15))) (\\l14.(\\l15.(l14 l15))))))) (\\l12.(\\l13.(\\l14.((l14 l12) l13)))))) (l0 (\\l11.(\\l12.(\\l13.(((l1 (l8 l12)) (\\l14.(\\l15.l15))) ((l6 l13) ((l11 (l4 l12)) l13))))))))) (\\l10.(\\l11.(((l1 l10) l2) l11))))) (l0 (\\l9.(\\l10.(\\l11.((\\l12.((\\l13.(((l1 l12) l13) (((l1 l13) l12) ((l9 (l4 l10)) (l4 l11))))) (l8 l11))) (l8 l10)))))))) (\\l8.((l8 (\\l9.l3)) l2)))) (\\l7.(\\l8.((l8 l4) l7))))) (\\l6.(\\l7.((l6 l5) l7))))) (\\l5.(\\l6.(\\l7.((l5 l6) (l6 l7))))))) (\\l4.(\\l5.(\\l6.(((l4 (\\l7.(\\l8.(l8 (l7 l5))))) (\\l7.l6)) (\\l7.l7))))))) (\\l3.(\\l4.l4)))) (\\l2.(\\l3.l2)))) (\\l1.(\\l2.(\\l3.((l1 l2) l3)))))) (\\l0.((\\l1.(l0 (l1 l1))) (\\l1.(l0 (l1 l1))))))";;