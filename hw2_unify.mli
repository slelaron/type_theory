type algebraic_term = Var of string | Fun of string * (algebraic_term list)

(* По списку уравнений вернуть одно уравнение *)
val system_to_equation: (algebraic_term * algebraic_term) list -> (algebraic_term * algebraic_term)

(* Применить подстановку к уравнению *)
val apply_substitution: (string * algebraic_term) list -> algebraic_term -> algebraic_term

(* Проверить решение *)
val check_solution: (string * algebraic_term) list -> (algebraic_term * algebraic_term) list -> bool

(* Решить систему; если решения нет -- вернуть None *)
val solve_system: (algebraic_term * algebraic_term) list -> (string * algebraic_term) list option

(* По строке сконструировать терм *)
val term_of_string: string -> algebraic_term

(* Вернуть строковое представление терма *)
val string_of_term: algebraic_term -> string

(* Вернуть строковое представление системы уравнений *)
val string_of_system: (algebraic_term * algebraic_term) list -> string

(* Применить подстановку к уравнению *)
val apply_substitution_with_map: algebraic_term Map.Make(String).t -> algebraic_term -> algebraic_term