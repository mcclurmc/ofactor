
open Asttypes
open Parsetree

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply" ;;

(* main driver needs to use commandliner to get source file name and
	 args. Args are a range of locations. Locations should be in format:
	 <l>:<c> or <char> *)

let main = () ;;

(* given a source file, and a location, can we find the AST node that
	 occurs nearest to, but not after, this location? *)

let ast_at_loc () = () ;;

(* givne an ast node, calculate its free variables *)

let diff a b = List.(filter (fun a -> not (mem a b)) a)  ;;
let (//) a b = diff a b ;;

let flatmap f l = List.(flatten (map f l)) ;;

let mapsnd ls = List.map snd ls ;;

let mkvar ?(loc=Location.none) s =
	Pexp_ident (Location.mkloc (Longident.Lident s) loc)
		(* { txt = Longident.Lident s *)
		(* ; loc } *)

let rec pvars = function
	| Ppat_any -> []
	| Ppat_var v -> [ mkvar v.txt ]
	| _ ->
		print_endline "pvars..."; []

let pexps pes =
	List.map
		(fun pe -> (snd pe).pexp_desc)
		pes
;;

let rec fv = function
	| Pexp_constant _ -> []
	(* fv(v) -> [v] *)
	| Pexp_ident i as v ->
		print_endline (Longident.last i.txt) ;
		[v]
	(* fv(e) -> fv(e1) @ fv(e2) ... *)
	(* fv(abs vs e) -> fv(e) \ vs *)
	| Pexp_let (rflag, pe, e) ->
		((fv e.pexp_desc) //
				(match rflag with
						| Nonrecursive | Default -> []
						| Recursive -> flatmap
							(fun (p,e) ->
								(fv e.pexp_desc) // (pvars p.ppat_desc))
							pe))
		@ (flatmap fv (pexps pe))
	| Pexp_function (_, _, pe) ->
		flatmap
			(fun (p,e) ->
				(fv e.pexp_desc) // (pvars p.ppat_desc))
			pe
	| Pexp_apply _ ->
		print_endline "skip Pexp_apply..."; []
	| _ -> print_endline "skip any..."; []
;;

(* let dummy = Location.in_file "dummy" ;; *)

let unwrap_str_expr = function
	| { pstr_desc = Pstr_eval e
		; pstr_loc = _ } -> e.pexp_desc
	| _ ->
		failwith "Expected expression; found, um... dunno!"

let parse_str s = s
	|> Lexing.from_string
	|> Parse.implementation
	|> List.hd
	|> unwrap_str_expr

let test_fv () =
	print_endline "Running tests..." ;
	(* let v_42 = parse "42" in *)
	let f1 = parse_str "function | x -> x y" in
	let fv1 = fv f1 in
	Printf.printf "len: %d\n" (List.length fv1) ;
	let v1 = parse_str "x" in
	let fv2 = fv v1 in
	Printf.printf "len: %d\n" (List.length fv2) ;
;;

let _ = test_fv () |> ignore ;;
