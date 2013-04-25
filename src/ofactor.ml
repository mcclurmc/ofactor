
open Asttypes
open Parsetree

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(* main driver needs to use commandliner to get source file name and
	 args. Args are a range of locations. Locations should be in format:
	 <l>:<c> or <char> *)

let main = ()

(* given a source file, and a location, can we find the AST node that
	 occurs nearest to, but not after, this location? *)

let ast_at_loc () = ()

(* given an ast node, calculate its free variables *)

let mem x xs =
	let open Longident in
	let cmp = function
		| (Lident a, Lident b) -> a = b
		| _ -> false in
	let rec loop = function
		| [] -> false
		| y::ys -> if cmp (x,y)
			then true
			else loop ys in
	loop xs

let diff a b = List.filter (fun a -> not (mem a b)) a
let (//) a b = diff a b

let flatmap f l = List.(flatten (map f l))

let mapfst ls = List.map fst ls
let mapsnd ls = List.map snd ls

let mkvar s = Longident.Lident s

let optmap f = function
	| None -> []
	| Some a -> f a

let rec pvars p = match p.ppat_desc with
	| Ppat_any
	| Ppat_constant _
	| Ppat_type _ -> []
	| Ppat_var v -> [ mkvar v.txt ]

let pevars (p,_) = pvars p

let rec fv e = match e.pexp_desc with
	| Pexp_assertfalse
	| Pexp_object _
	| Pexp_pack _
	| Pexp_constant _ -> []

	| Pexp_new i
	| Pexp_ident i -> [i.txt]

	| Pexp_array es
	| Pexp_tuple es -> flatmap fv es

	| Pexp_assert e
	| Pexp_lazy e
	| Pexp_poly (e, _)
	| Pexp_constraint (e, _, _)
	| Pexp_field (e, _) -> fv e

	| Pexp_match (e, pe)
	| Pexp_try (e, pe) ->
		(fv e) @ (flatmap fv_pat pe)

	| Pexp_construct (_, e, _) -> optmap fv e

	| Pexp_apply (e, les) ->
		(fv e) @ (flatmap (fun le -> fv (snd le)) les)
	| Pexp_record (les, e) ->
		(optmap fv e) @ (flatmap (fun le -> fv (snd le)) les)

	| Pexp_sequence (e1, e2)
	| Pexp_while (e1, e2)
	| Pexp_when (e1, e2)
	| Pexp_setfield (e1, _, e2) -> (fv e1) @ (fv e2)

	| Pexp_ifthenelse (e1, e2, e3) -> (fv e1) @ (fv e2) @ (optmap fv e3)

	| Pexp_for (i, e1, e2, _, e3) ->
		(fv e1) @ (fv e2) @
			((fv e3) // [ mkvar i.txt ])

	(* Binding cases *)
	| Pexp_let ((Nonrecursive|Default), pe, e) ->
		(flatmap fv_pat pe) @
			((fv e) // (flatmap pevars pe))
	| Pexp_let (Recursive, pe, e) ->
		((flatmap fv_pat pe) // (flatmap pevars pe)) @
			((fv e) // (flatmap pevars pe))
	| Pexp_function (_, e, pe) ->
		(flatmap fv_pat pe) @ (optmap fv e)

and fv_pat (p,e) = (fv e) // (pvars p)

(* TODO: bound variables. Eventually, we'll need to know the variables
	 bound in the scope _between_: (1) the level to which we're
	 extracting the new function, and (2), the level at which the
	 expression we're extracting has been defined. *)
let rec bv s e acc = match s.pstr_desc with
	| Pstr_value ((Nonrecursive|Default), pes) ->
		acc
	| Pstr_value (Recursive, _) ->
		failwith "bv: Pstr_value Recursive unimplemented"
	| _ -> acc

and bv_exp es e_stop acc =
	match es with
	| [] -> []
	| e::es ->
		(* TODO: Only return acc if we've reached the expression we're
			 targetting. Otherwise, we might have run down a branch which we
			 don't care about. Consider looking for the variables bound at a
			 particular match case expression: we would only want the variables
			 bound up to that point, and not those bound on other branches. *)
		if e = e_stop then acc else
			match e.pexp_desc with
			(* Terminal cases *)
			| Pexp_ident _
			| Pexp_constant _
			| Pexp_new _
			| Pexp_assertfalse -> []

			(* Recursing cases *)
			(* |  *)

			(* Binding cases *)
			| Pexp_let _ -> bv_exp es e_stop acc
			| Pexp_function _ -> bv_exp es e_stop acc

(* Tests *)

let unwrap_str_eval = function
	| { pstr_desc = Pstr_eval e
		; pstr_loc = _ } -> e
	| _ -> failwith "unwrap_str_eval"

let parse_str s = s
	|> Lexing.from_string
	|> Parse.implementation
	|> List.hd
	|> unwrap_str_eval

let print_idents is =
	let is = List.map Longident.last is in
	print_endline (String.concat ", " is)

let do_test s =
	let f = parse_str s in
	let vs = fv f in
	Printf.printf "[%s] free vars: " s ;
	print_idents vs

let test_fv () =
	print_endline "Running tests..." ;

	do_test "function | x -> x";					(* None *)
	do_test "function x -> y";						(* y *)
	do_test "function x -> x z";					(* z *)
	(* TODO: How do we deal with identifiers in global scope? *)
	do_test "for i = 0 to 10 do print_endline \"foo\" done";
	do_test "let rec f a = b and g a = f a in ();;"

let _ = test_fv ()
