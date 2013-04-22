
open Parsetree

let _ = print_endline "hello world" ;;

(* main driver needs to use commandliner to get source file name and
	 args. Args are a range of locations. Locations should be in format:
	 <l>:<c> or <char> *)

let main = () ;;

(* given a source file, and a location, can we find the AST node that
	 occurs nearest to, but not after, this location? *)

let ast_at_loc () = () ;;

(* givne an ast node, calculate its free variables *)

let diff a b = [] ;;

let pat_vars = function
	| _ -> []
;;

(* let lift desc = { pexp_desc = desc; pexp_loc = Location.dummy } ;; *)

let rec fv = function
	(* fv(v) -> [v] *)
	| Pexp_ident _ as v
		-> [v]
	(* fv(e) -> fv(e1) @ fv(e2) ... *)
	(* fv(abs vs e) -> fv(e) \ vs *)
	| Pexp_function (_, _, pat_exps) ->
		List.(flatten
						(map
							 (fun (v,e) ->
								 diff (fv e.pexp_desc) (pat_vars v))
							 pat_exps))
	(* fv(_) -> [] *)
	| _ -> []
;;
