
open Asttypes
open Parsetree

open OUnit

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(* main driver needs to use commandliner to get source file name and
	 args. Args are a range of locations. Locations should be in format:
	 <l>:<c> or <char> *)

let main = ()

(* given a source file, and a location, can we find the AST node that
	 occurs nearest to, but not after, this location? *)

let string_of_loc (loc : Lexing.position) =
	Printf.sprintf "l%dc%d"
		loc.Lexing.pos_lnum
		loc.Lexing.pos_cnum

let print_loc loc =
	Printf.printf "%s to %s\n"
		(string_of_loc loc.Location.loc_start)
		(string_of_loc loc.Location.loc_start)

let cmp_loc l1 l2 =
	let open Lexing in
	let c1 = compare l1.pos_lnum l2.pos_lnum
	and c2 = compare l1.pos_cnum l2.pos_cnum in
	if c1 = 0 then c2 else c1

let find_loc_exp loc e =
	let open Lexing in
	()

let ast_at_loc loc =
	let pt = loc.Location.loc_start.Lexing.pos_fname
		|> open_in
		|> Lexing.from_channel
		|> Parse.implementation in
	pt

(* given an ast node, calculate its free variables *)

let cmp_lident a b = Longident.(compare (last a) (last b))

let mem x xs =
	let rec loop = function
		| [] -> false
		| y::ys -> if (cmp_lident x y) = 0
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

let uniq ls =
	let rec loop acc = function
		| [] -> acc
		| (h::t) ->
			if List.mem h acc
			then loop acc t
			else loop (h::acc) t
	in loop [] ls |> List.rev

let rec pvars p = match p.ppat_desc with
	| Ppat_any
	| Ppat_constant _
	| Ppat_type _ -> []
	| Ppat_var v -> [ mkvar v.txt ]

let pevars (p,_) = pvars p

let rec fv e =
	match e.pexp_desc with
	| Pexp_object _
	| Pexp_pack _
	| Pexp_constant _ ->
		[]

	| Pexp_new i
	| Pexp_ident i ->
		[i.txt]

	| Pexp_array es
	| Pexp_tuple es ->
		flatmap fv es

	| Pexp_constraint (e, _)  (* TODO: implement types as fv *)
	| Pexp_assert e
	| Pexp_lazy e
	| Pexp_poly (e, _)
	| Pexp_open (_, e)	(* TODO: implement module ident as fv*)
	| Pexp_send (e, _)
	| Pexp_setinstvar (_, e)
	| Pexp_newtype (_, e)
	| Pexp_field (e, _) ->
		fv e

	| Pexp_variant (_, e)
	| Pexp_construct (_, e) ->
		optmap fv e

	| Pexp_apply (e, les) ->
		(fv e) @ (flatmap fv (mapsnd les))
	| Pexp_record (les, e) ->
		(optmap fv e) @ (flatmap fv (mapsnd les))

	| Pexp_sequence (e1, e2)
	| Pexp_while (e1, e2)
	| Pexp_setfield (e1, _, e2) ->
		(fv e1) @ (fv e2)

	| Pexp_ifthenelse (e1, e2, e3) -> (fv e1) @ (fv e2) @ (optmap fv e3)

	| Pexp_override es -> flatmap fv (mapsnd es) (* ??? *)

	| Pexp_letmodule (_, m, e) -> (fv e) @ (fv_mod m)

	(* Binding cases *)
	| Pexp_match (e, cs)
	| Pexp_try (e, cs) ->
		(fv e) @ (flatmap fv_case cs)

	| Pexp_for (i, e1, e2, _, e3) ->
		(fv e1) @ (fv e2) @
			((fv e3) // [ mkvar i.txt ])

	| Pexp_let (Nonrecursive, pe, e) ->
		(flatmap fv_pat pe) @
			((fv e) // (flatmap pevars pe))
	| Pexp_let (Recursive, pe, e) ->
		((flatmap fv_pat pe) // (flatmap pevars pe)) @
			((fv e) // (flatmap pevars pe))
	| Pexp_function cs ->
		flatmap fv_case cs

	| Pexp_fun (_, e0, p, e1) -> (* TODO: labels *)
		(optmap fv e0) @ ((fv e1) // (pvars p))

	| Pexp_coerce (_, _, _)
	| Pexp_extension _ -> print_endline "unimplemented"; []

and fv_pat (p,e) = (fv e) // (pvars p)

and fv_case c = ((optmap fv c.pc_guard) @ (fv c.pc_rhs)) // (pvars c.pc_lhs)

and fv_mod s = failwith "fv_mod unimplemented"

let fv e = fv e |> uniq

(* TODO: bound variables. Eventually, we'll need to know the variables
	 bound in the scope _between_: (1) the level to which we're
	 extracting the new function, and (2), the level at which the
	 expression we're extracting has been defined. *)
let rec bv s e acc = match s.pstr_desc with
	| Pstr_value (Nonrecursive, pes, _) ->
		acc
	| Pstr_value (Recursive, _, _) ->
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

			(* Recursing cases *)
			(* |  *)

			(* Binding cases *)
			| Pexp_let _ -> bv_exp es e_stop acc
			| Pexp_function _ -> bv_exp es e_stop acc

(* Tests *)

let unwrap_str_eval = function
	| { pstr_desc = Pstr_eval (e,_)
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

module EVar = struct
	type t = Longident.t
	let compare = cmp_lident
	let pp_printer fmt v = Format.pp_print_string fmt (Longident.last v)
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module SetVar = OUnitDiff.SetMake(EVar)
module ListVar = OUnitDiff.ListSimpleMake(EVar)

let debug = ref false

let mktest s vs () =
	let p = parse_str s in
	let fvs = fv p in
	let vs = List.map mkvar vs in

	if !debug then
		(Printf.printf "\n[%s] len: %d, free vars: %s - Pass? " s
			 (List.length fvs)
			 (List.map Longident.last fvs |> String.concat ", "));

	ListVar.assert_equal vs fvs ;

	let fvs = SetVar.of_list fvs in
	let vs = SetVar.of_list vs in
	SetVar.assert_equal vs fvs

let mktest s vs = s >:: (mktest s vs)

let suite = "ofactor" >:::
	[ mktest "function | x -> x" []
	; mktest "function x -> y" ["y"]
  ; mktest "function | x -> x" []
	; mktest "function x -> y" ["y"]
	; mktest "function x -> x z" ["z"]
	; mktest "for i = 0 to 10 do print_endline \"foo\" i x done" ["print_endline"; "x"]
	; mktest "let rec f a = b and g a = f a in f g a " ["b"; "a"]
	; mktest "match z with | x -> y | x -> x" ["z"; "y"]
	; mktest "new o" ["o"]
	; mktest "Foo exp" ["exp"]
	; mktest "o # msg" ["o"]
	; mktest "v <- i" ["i"]
	; mktest "let open M in ()" []
	; mktest "{ a = x []; b = y }" ["x"; "y"]
	; mktest "if true then a else b" ["a"; "b"]
	; mktest "if true then a else match z with | x -> y | x -> x" ["a"; "z"; "y"]
	; mktest "match z with | x -> y | x -> if x then z else y" ["z"; "y"]
	; mktest "(x : int)" ["x"]
	]

let _ = run_test_tt_main suite
