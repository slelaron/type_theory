type algebraic_term = Var of string | Fun of string * (algebraic_term list)

let get_name index str = 
	let rec get_name_with_buffer index buffer =
		match str.[index] with
			',' | ')' | '(' | '#' -> (index, buffer)
			| symbol -> get_name_with_buffer (index + 1) (buffer ^ Char.escaped symbol) in
		get_name_with_buffer index "";;

let term_of_string str =
	let str = str ^ "#" in
		let rec make_step index =
			let index, name = get_name index str in
				match str.[index] with
					'(' -> 
						let rec loop index = 
							let index, result = make_step index in
								match str.[index] with
									',' -> 
										let index, tail = loop (index + 1) in
											(index, result :: tail)
									| _ -> (index + 1, result :: []) in
							let index, result = loop (index + 1) in
								(index, Fun (name, result))
					| _ -> (index, Var name) in
			snd (make_step 0);;

let rec string_of_term term = match term with
	Fun (name, last) -> name ^ "(" ^ (String.concat "," (List.map string_of_term last)) ^ ")"
	| Var var -> var;;

let string_of_system lst = 
	((String.concat "\n" (List.map (fun (a, b) -> (string_of_term a) ^ " = " ^ (string_of_term b)) lst)) ^ "\n");;

let system_to_equation x = failwith "Not implemented";;

(*let apply_substitution x y =
	let func a b = 
		let str, term = b in
			let rec internal_func a = 
				match a with
					Var v -> if v = str then term else Var v
					| Fun (name_of_func, term_list) -> 
						Fun (name_of_func, List.map internal_func term_list) in
				internal_func a in
	List.fold_left func y x;;*)

module StringMap = Map.Make(String);;

let rec apply_substitution_with_map map y = 
	match y with
		Var var -> if StringMap.mem var map then StringMap.find var map else Var var
		| Fun (name, last) -> Fun (name, List.map (fun x -> apply_substitution_with_map map x) last);;

let apply_substitution x y =
	let map = List.fold_left (fun prev (key, value) -> StringMap.add key value prev) StringMap.empty x in
	 	apply_substitution_with_map map y;;

let rec apply_to_system_one_substitution x v term = 
	List.map 
		(fun eq -> 
			match eq with
				Var var -> if var = v then term else Var var
				| Fun (name, last) -> Fun (name, apply_to_system_one_substitution last v term)
				)
		x;;

let substitute_into_system x y = 
	let map = 
		List.fold_left 
			(fun map (var, term) ->
				StringMap.add var term map)
			StringMap.empty
			x in
		let rec substitute terms = 
			List.map
				(fun term -> match term with
					Fun (name, last) -> Fun (name, substitute last)
					| Var a -> 
						if StringMap.mem a map then 
							StringMap.find a map 
						else
							Var a) 
				terms in
			let first, second = List.split y in
				let after_first = substitute first and after_second = substitute second in
					List.combine after_first after_second;;

let check_solution x y = 
	let first, second = List.split (substitute_into_system x y) in
		first = second;;

let contains_in_term term var = 
	let rec contains_in_term_internal term =
		match term with
			Var v -> v = var
			| Fun (name, last) ->
				List.exists (fun x -> x) (List.map contains_in_term_internal last) in
		contains_in_term_internal term;;

let rec count_variables_in_terms terms init_map = 
	List.fold_left 
		(fun map term -> match term with
			Fun (name, last) -> count_variables_in_terms last map
			| Var var -> StringMap.add var ((if (StringMap.mem var map) = false then 0 else (StringMap.find var map)) + 1) map)
		init_map
		terms;;

let count_variables_in_system system =
	let first, second = List.split system in
		let map = count_variables_in_terms first StringMap.empty in
			count_variables_in_terms second map;;

let solve_system x = 
	let concat prev now = 
		match prev with 
			None -> None 
			| Some prev ->
				 match now with 
				 	None -> None 
				 	| Some now -> Some (now @ prev) in
		let rec get_good_system x = 
			match x with
				Fun (fname, flast), Fun (sname, slast) -> 
					if fname = sname && (List.length flast) = (List.length slast) then
						List.fold_left 
							concat
							(Some [])
							(List.map get_good_system (List.combine flast slast))
					else
						None
				| Var var1, Var var2 -> 
					if var1 = var2 then
						Some []
					else
						Some ((Var var1, Var var2) :: [])
				| Fun (name, last), Var var | Var var, Fun (name, last) -> 
					if contains_in_term (Fun (name, last)) var = true then 
						None
					else
						Some ((Var var, Fun (name, last)) :: [])
		and solving system =
			let vars_in_terms = count_variables_in_system system in
				let fun_to_sub = 
					List.fold_left 
						(fun prev now -> 
							match prev with
								None -> 
									(match now with 
										Var var, another -> 
											if (StringMap.find var vars_in_terms) > 1 then
												Some (var, another)
											else
												None
										| _, _ ->	
											None)
								| Some el -> Some el)
						None
						system in
					match fun_to_sub with
						Some (var, term) -> 
							let first, second = List.split system in
								let after_first = apply_to_system_one_substitution first var term
								and after_second = apply_to_system_one_substitution second var term in
									let new_system = List.fold_left concat (Some []) (List.map get_good_system (List.combine after_first after_second)) in
										(match new_system with
											None -> None
											| Some system -> solving ((Var var, term) :: system))
						| None ->
							Some (List.map 
								(fun a -> match a with
									Var var, another -> (var, another)
									| _, _ -> failwith "Something went wrong")
								system) in
			let initial_system = List.fold_left concat (Some []) (List.map get_good_system x) in
				match initial_system with
					None -> None
					| Some initial_system -> solving initial_system;;

let solvable1 = [(term_of_string "f1(k,f2(l,f3(m,f4(n,f5(o)))))", term_of_string "f1(f2(f3(f4(f5(o),n),m),l),k)")];;
let solvable2 = [(term_of_string "f(x,g(y))", term_of_string "f(e(g(f(t,z))),t)")];;
let solvable3 = [(term_of_string "e20(h79(f80(h99(a77(z25,m70,k82,q28,u48),g83(w4),b23(s74,y50,j58),h72(m89,k67,y98),a24(m37)),c29(h20(t62,w19,j28,r11),b99(w36,k41,q66),d76(o89,k50),h48(i31,s24,s94,s26),b13(l83,o3,k18,u23)),e97(f92(i17,k98,v60,k89,l54)),h50(a30(r45,p38,y50,i36),a42(o96,q4,p26,v7,s15),w71,b91(x87),d92(k0,u11,w68)),y93),g14(g66(v5))),d35(a25(b85(e43(o98,l75,j79,z73),g20(z5,w27,p80,m66)),b71(d38(m7,t5,i12,o65),f39(s36,w76,w48),e10(n36,u42,q50,r90,y68),d53(q93,r65,r37),t15),d53(c36(s42,u9),f31(u84,p34,m94,l88,y54),f64(i51,j10,i27)),g47(e70(o96,l61,o1,v23))),a93(d38(g76(t14,s21,n5,w73,o74),c72(k90,n14,j91,j44,j17)),h58(d81(q49,t90,t13,m76,r47),f40(q94,y0,t83)),h10(c66(w26),g27(k96,w93),c16(j77,q54,w97,t54,o21),g31(z22,t97),f71(l30,m59,z77,x44))),e9(h38(a80(m11,z59,v15),f19(l21,j13,x42)),e79(g72(t7,m44,z83),b73(u54,m4,r18,q98,p39),g89(q35,y84)),h60(x9)),a92(f71(g8(n58,y37,z24,x53),b67(t51,v96,l91,s97),f73(p39,z76,l15,z33,y49)),f32(c94(y51)))),z16,d72(h88(a81(h97(z95,m96,p42,w27),a80(l77,l51,x26),a74(q44,k61,p1)),g38(c49(i96,m36,t45))),c48(b24(e91(y57,k7,w3),q98,c56(z61,y89,u67)),e36(a50(o28,p88,i48,u72,w71),e55(o60)),g75(g38(u85),e86(i41,n43,s6,r84),d25(m33),s76),b97(f99(u35,t76,l43,y73)),g98(f75(v0,k3),b11(u58,m34)))),e88(g38(l74,c14(q82,f1(x61,q13),e18(y55,x16,r29)),e60(f82(x10,v0,k98,v40,p91),h97(j54,o45,v76,r81),a74(z28,k60,j48,q88),b42(t56,o16,n98),e83(y10)),e25(c13(k11,u87,x15,y44),g65(j69,l66,i89,w42,o17),b43(t77,w29,n97,u25,r61),g52(v35)),e9(c2(z61,t39,u93,y43),g65(z58,i96),a76(v77,r28,p95,n86,m95),h43(n88,m94,j5,s57))),f33(h88(c45(l2,s18,j88)),c31(n94,h60(j52,u9),h28(n65,j77,r54,t59),h85(m44),e66(t73,o68,n17))),j25,e18(d10(h99(p86,i52,v95),c44(p71,t97,k24),e80(n97,n93,z46))),f82(c38(g62(j10,n72,v84,y80),f43(z96,o84,q8,s1,o77),a10(n81,z95)),g41(f5(w95,u47,k90),g12(u45),a81(j5)),e68(f67(p15,j60,q50,x95,i63),a13(u9)),e36(m68,b71(z54,z73)))))", term_of_string "e20(h79(f80(h99(a77(z25,m70,k82,q28,x57),g83(w4),b23(s74,y50,p12),u4,a24(m37)),c29(h20(k77,w19,w29,n1),b99(w36,k41,q66),d76(o89,k50),h48(i31,s24,p6,s26),b13(l83,o3,k18,u23)),e97(f92(v41,k98,x5,k89,l54)),h50(a30(r14,p38,y50,i36),a42(o96,q4,p26,v7,s15),g29(v88,z0,t93,r37),b91(x87),d92(k0,u11,j87)),d85(t91)),g14(g66(b31(y64,n76,r11,n5,r10)))),z32,e87(b72(c70(h99(n43),h73(k53,r71,v90),a12(i30,p89,p4),g78(k20),h36(q91,y66,q76,m49)),g20(b24(r90,k3,s56,x43),a19(k56,q44))),e25(g61(f40(t27,l9,o49,w16,y87),g91(r52,u65,t36,z15)))),d72(h88(a81(h97(z95,m96,p42,w27),a80(l77,l51,x26),a74(q44,s95,p1)),g38(c49(i96,m36,t45))),c48(b24(e91(y57,k7,w3),e91(w42),c56(z61,j36,i38)),e36(a50(o28,p88,i48,u72,w71),e55(o60)),g75(g38(u85),e86(i41,n43,s6,m91),d25(m33),g94(k99,l98)),b97(f99(u35,t76,l43,y73)),g98(f75(v0,k3),m57))),e88(g38(e71(e39(w53,i31,q73),f42(s33,z81)),c14(c16(v16,v57),f1(l83,q13),e18(y55,x16,r29)),e60(f82(x10,v0,k98,v40,p91),v59,a74(u75,k60,j48,q88),b42(t56,o16,n98),e83(y10)),m12,e9(c2(z61,t39,u93,y43),g65(z58,i96),a76(v77,r28,m23,n86,m95),h43(n88,m94,j5,s57))),f33(h88(c45(j46,s18,j88)),c31(b50(o21),h60(j52,u9),h28(y36,s21,r54,t59),h85(k52),e66(t73,o68,t88))),b69(z30,d93(c70(s78,y41,q53),d16(l56),h81(u61,o65,s15),g44(s53,v88,r93,j27)),c62(b91(w50,q92),d41(j52,z16,o67))),e18(d10(h99(p86,i52,v95),c44(p71,j4,k24),e80(n97,n93,j58))),f82(c38(g62(j10,n72,z6,y80),f43(k23,o84,q8,s1,o77),a10(n81,z95)),g41(f5(w95,u47,k90),g12(o66),a81(j5)),i60,e36(f72(k46),b71(z54,z73)))))")];;
let no_solution = (term_of_string "f(x)", term_of_string "y") :: (term_of_string "f(x)", term_of_string "f(y)") :: [];;

let find_solution lst =
	let solution = solve_system lst in
		match solution with
			None -> "No solution" 
			| Some solution ->
				(String.concat 
					"\n"
					(List.map 
						(fun (fst, snd) -> fst ^ " = " ^ (string_of_term snd))
						solution)) ^ "\n" ^ (if check_solution solution lst = true then "Correct\n" else "Incorrect\n");;

(*print_string ((string_of_system solvable3) ^ "\n");;
print_string ((find_solution solvable3) ^ "\n");; *)
