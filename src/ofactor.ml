
open Asttypes
open Parsetree

open Ast_mapper

open Helpers

(* main driver should use commandliner to get source file name and
	 args. Args are a range of locations. Locations should be in format:
	 <l>:<c> or <char> *)

let main = ()

(** Free and bound variables *)

(* TODO: unit tests *)
let rec pvars p = match p.ppat_desc with
	| Ppat_any
	| Ppat_constant _
	| Ppat_interval (_, _)
	| Ppat_unpack _
	| Ppat_extension _  (* ??? *)
	| Ppat_type _ -> []

	| Ppat_lazy p
	| Ppat_constraint (p, _) ->
		pvars p (* TODO: will need to worry about newtype soon *)

	| Ppat_array ps
	| Ppat_tuple ps ->
		flatmap pvars ps

	| Ppat_or (p1, p2) -> pvars p1 @ pvars p2

	| Ppat_alias (p, a) ->
		pvars p @ [ mkvar a.txt ]

	| Ppat_var v -> [ mkvar v.txt ]

	| Ppat_construct (id, p) ->
		let pv = match p with
		| None -> []
		| Some p -> pvars p in
		id.txt :: pv

	| Ppat_variant (_, None) -> []
	| Ppat_variant (_, Some p) -> pvars p

	| Ppat_record (ps, _) -> flatmap pvars (mapsnd ps)

and pevars (p,_) = pvars p

let rec fv_exp e =
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
		flatmap fv_exp es

	| Pexp_constraint (e, _)  (* TODO: implement types as fv_exp *)
	| Pexp_assert e
	| Pexp_lazy e
	| Pexp_poly (e, _)
	| Pexp_open (_, e)	(* TODO: implement module ident as fv*)
	| Pexp_send (e, _)
	| Pexp_setinstvar (_, e)
	| Pexp_newtype (_, e)  (* TODO: something special for newtype? *)
	| Pexp_field (e, _) ->
		fv_exp e

	| Pexp_variant (_, e)
	| Pexp_construct (_, e) ->
		optmap fv_exp e

	| Pexp_apply (e, les) ->
		(fv_exp e) @ (flatmap fv_exp (mapsnd les))
	| Pexp_record (les, e) ->
		(optmap fv_exp e) @ (flatmap fv_exp (mapsnd les))

	| Pexp_sequence (e1, e2)
	| Pexp_while (e1, e2)
	| Pexp_setfield (e1, _, e2) ->
		(fv_exp e1) @ (fv_exp e2)

	| Pexp_ifthenelse (e1, e2, e3) -> (fv_exp e1) @ (fv_exp e2) @ (optmap fv_exp e3)

	| Pexp_override es -> flatmap fv_exp (mapsnd es) (* ??? *)

	| Pexp_letmodule (_, me, e) -> (fv_exp e) @ (fv_me me)

	(* Binding cases *)
	| Pexp_match (e, cs)
	| Pexp_try (e, cs) ->
		(fv_exp e) @ (flatmap fv_case cs)

	| Pexp_for (i, e1, e2, _, e3) ->
		(fv_exp e1) @ (fv_exp e2) @
			((fv_exp e3) // [ mkvar i.txt ])

	| Pexp_let (Nonrecursive, pe, e) ->
		(flatmap fv_pat pe) @
			((fv_exp e) // (flatmap pevars pe))
	| Pexp_let (Recursive, pe, e) ->
		((flatmap fv_pat pe) // (flatmap pevars pe)) @
			((fv_exp e) // (flatmap pevars pe))
	| Pexp_function cs ->
		flatmap fv_case cs

	| Pexp_fun (_, e0, p, e1) -> (* TODO: labels *)
		(optmap fv_exp e0) @ ((fv_exp e1) // (pvars p))

	| Pexp_coerce (_, _, _)
	| Pexp_extension _ -> print_endline "unimplemented"; []

and fv_pat (p,e) = (fv_exp e) // (pvars p)

and fv_case c = ((optmap fv_exp c.pc_guard) @ (fv_exp c.pc_rhs)) // (pvars c.pc_lhs)

and fv_me s = [] (* TODO: implement *)

let fv_exp e = fv_exp e |> uniq ~cmp:cmp_lident

let choose_one_case pos cs =
	let is_in_guard c = match c.pc_guard with
		| None -> false
		| Some e -> pos_in_loc pos e.pexp_loc in
	let c = List.filter
		(fun c ->
			is_in_guard c || pos_in_loc pos c.pc_rhs.pexp_loc)
		cs in
	match c with
		| [c] ->
			if is_in_guard c
			then `guard c
			else `rhs c
		| _ ->
			failwith "choose_one_case: range does not specify an expression"

and choose_one_exp pos es =
	let es = List.filter (pos_in_exp pos) es in
	match es with
		| [e] -> e
		| _ ->
			failwith "choose_one_exp: range does not specify an expression"

let rec bv_str str upto bound_vars = match str.pstr_desc with
	| Pstr_value (rec_flag, pes, _) ->
		let pos_upto = upto.pexp_loc.Location.loc_start in
		let e = choose_one_exp pos_upto (mapsnd pes) in
			let bvars = match rec_flag with
				| Recursive    -> flatmap pevars pes
				| Nonrecursive -> [] in
			bv_exp e upto (bvars @ bound_vars)
	| _ -> bound_vars

and bv_exp e upto bound_vars =
	if e = upto then bound_vars else
		let pos_upto = upto.pexp_loc.Location.loc_start in
		match e.pexp_desc with

		(* Terminal cases *)

		| Pexp_ident _
		| Pexp_constant _
		| Pexp_new _ ->
			bound_vars

		(* Binding cases *)

		| Pexp_fun (l, le, p, e) ->
			(* XXX Check the label case... *)
			let l = match le with
				| None -> if l = "" then [] else [ mkvar l ]
				| Some _ -> [mkvar (String.sub l 1 (String.length l - 1)) ] in
			bv_exp e upto (pvars p @ l @ bound_vars)

		| Pexp_let (rec_flag, pes, e) ->
			let bvars = match rec_flag with
				| Recursive -> flatmap pevars pes
				| Nonrecursive -> [] in
			let es = e :: mapsnd pes in
			let e = choose_one_exp pos_upto es in
				bv_exp e upto (bvars @ bound_vars)

		| Pexp_function cs ->
			(match choose_one_case pos_upto cs with
			| `guard c -> bv_exp (opt c.pc_guard) upto (bound_vars)
			| `rhs c -> bv_exp c.pc_rhs upto (bound_vars))

		(* Non-binding cases *)

		| Pexp_apply (e, les) ->
			let es = e :: mapsnd les in
			let e = choose_one_exp pos_upto es in
				bv_exp e upto bound_vars

		(* TODO: other cases *)

		| Pexp_match (_, _)
		| Pexp_try (_, _)
		| Pexp_tuple _
		| Pexp_construct (_, _)
		| Pexp_variant (_, _)
		| Pexp_record (_, _)
		| Pexp_field (_, _)
		| Pexp_setfield (_, _, _)
		| Pexp_array _
		| Pexp_ifthenelse (_, _, _)
		| Pexp_sequence (_, _)
		| Pexp_while (_, _)
		| Pexp_for (_, _, _, _, _)
		| Pexp_constraint (_, _)
		| Pexp_coerce (_, _, _)
		| Pexp_send (_, _)
		| Pexp_setinstvar (_, _)
		| Pexp_override _
		| Pexp_letmodule (_, _, _)
		| Pexp_assert _
		| Pexp_lazy _
		| Pexp_poly (_, _)
		| Pexp_object _
		| Pexp_newtype (_, _)
		| Pexp_pack _
		| Pexp_open (_, _)
		| Pexp_extension _ ->
			failwith "bv_exp: unimplemented case"

(** AST searching *)

let search_mapper(pos1, pos2) =
object(this)

	inherit Ast_mapper.mapper as super

	val mutable exp = None
	val mutable str = None
	val mutable bound_vars = []

	val fmt = Format.str_formatter
	val printer = Pprintast.default

	method get_exp = exp
	method get_str = str

	method get_free_vars =
		match str, exp with
		| None, _
		| _, None -> []
		| Some s, Some e ->
			let fvs = fv_exp e
			and bvs = bv_str s e [] in
			Printf.printf "\n  bound vars: %s\n"
				(string_of_idents bvs) ;
			intersect fvs bvs

	method! expr e =
		let e = super # expr e in
		if exp <> None
		then e
		else if range_in_loc pos1 pos2 e.pexp_loc
		then begin
			printer # expression fmt e ;
			Printf.printf "found an expression at %s:\n  [%s]\n"
				(string_of_loc e.pexp_loc) (Format.flush_str_formatter ()) ;
			exp <- Some e ;
			let fvs = fv_exp e in
			Printf.printf "  free vars: %s\n" (string_of_idents fvs) ;
			e
		end
		else e

	method! structure_item si =
		Printf.printf "found a structure item at %s\n"
			(string_of_loc si.pstr_loc);
		match si.pstr_desc with
		| Pstr_value (rec_flag, pes, _) ->
			str <- Some si ;
			let pe = List.filter
				(fun (p,e) -> pos_in_loc pos1 e.pexp_loc)
				pes in
			let pe = match pe with
			| [pe] -> pe
			| _ ->
				failwith "search_mapper#structure_item: range does not specify an expression" in
			let bvars = match rec_flag with
				| Recursive    -> flatmap pevars pes
				| Nonrecursive -> [] in
			this # expr (snd pe) |> ignore ;
			bound_vars <- bound_vars @ bvars ;
			si
		| _ ->
			failwith "range does not specify a let binding"

	method! structure sis =
		let si = List.filter
			(fun s -> range_in_loc pos1 pos2 s.pstr_loc)
			sis in
		match si with
		| [] ->
			print_endline "no matching locations" ;
			sis
		| [si] -> super # structure [si]
		| _ -> failwith "range bounds multiple structure items!"

end

(** Tests *)

let unwrap_str_eval = function
	| { pstr_desc = Pstr_eval (e,_)
		; pstr_loc = _ } -> e
	| _ -> failwith "unwrap_str_eval"

let parse_string s = s
	|> Lexing.from_string
	|> Parse.implementation
	|> List.hd
	|> unwrap_str_eval

let ast_of_file fn =
	open_in fn
	|> Lexing.from_channel
	|> Parse.implementation

let do_test fn (l1,c1) (l2,c2) =
	let p1 = mkpos l1 c1
	and p2 = mkpos l2 c2 in
	let m = search_mapper(p1, p2) in
	Printf.printf "\nrunning search_mapper on %s:%s\n"
		(string_of_pos p1) (string_of_pos p2) ;
	let ast = ast_of_file fn in
	m # implementation "" ast |> ignore ;
	match m # get_exp with
	| None -> print_endline "  FAILED to retrieve expression"
	| Some e ->
		Printf.printf "retrieved expression at: %s"
			(string_of_loc e.pexp_loc) ;
		let fvs = m # get_free_vars in
		Printf.printf "  free vars /\\ bound vars: %s\n" (string_of_idents fvs)

let _ =
	do_test "test/parsing.ml" (6,11) (6,16) ;
	do_test "test/parsing.ml" (9,2)  (9,11) ;
	do_test "test/parsing.ml" (13,1) (15,44) ;
	do_test "test/parsing.ml" (15,5) (15,44) ;
	do_test "test/parsing.ml" (15,5) (15,22) ;

open OUnit

module EVar = struct
	type t = Longident.t
	let compare = cmp_lident
	let pp_printer fmt v = Format.pp_print_string fmt (Longident.last v)
  let pp_print_sep = OUnitDiff.pp_comma_separator
end

module SetVar = OUnitDiff.SetMake(EVar)
module ListVar = OUnitDiff.ListSimpleMake(EVar)

let verbose = ref false

let mktest s vs () =
	let p = parse_string s in
	let fvs = fv_exp p in
	let vs = List.map mkvar vs in

	if !verbose then
		(Printf.printf "\n[%s] len: %d, free vars: %s - Pass? " s
			 (List.length fvs)
			 (string_of_idents fvs));

	let fvs = SetVar.of_list fvs in
	let vs = SetVar.of_list vs in
	SetVar.assert_equal vs fvs

let mktest s vs = s >:: (mktest s vs)

let fv_suite = "ofactor" >:::
	[ mktest "function x -> x" []
	; mktest "function x -> y" ["y"]
	; mktest "function x -> y | z -> z" ["y"]
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
	; mktest
		"Printf.sprintf \"l%dc%d\"\
       pos.Lexing.pos_lnum\
         (pos.Lexing.pos_cnum - pos.Lexing.pos_bol)"
		["pos"; "sprintf"; "-"] ]

let mktest fn (l1,c1) (l2,c2) expected () =
	let p1 = mkpos l1 c1
	and p2 = mkpos l2 c2 in
	let m = search_mapper(p1, p2) in
	let ast = ast_of_file fn in
	m # implementation "" ast |> ignore ;
	let fvs = m # get_free_vars in

	if !verbose then
		(Printf.printf "\n  free vars: %s - Pass? "
			 (string_of_idents fvs));

	let fvs = SetVar.of_list fvs
	and expected = SetVar.of_list expected in
	SetVar.assert_equal expected fvs

let mktest fn (l1,c1) (l2,c2) expected =
	(Printf.sprintf "%s - l%dc%d:l%dc%d" fn l1 c1 l2 c2)
	>:: (mktest fn (l1,c1) (l2,c2) expected)

let mapper_suite = "ofactor search_mapper" >:::
	[	mktest "test/parsing.ml" (6,11) (6,16) []
	; mktest "test/parsing.ml" (9,2)  (9,11) []
	; mktest "test/parsing.ml" (13,1) (15,44) []
	; mktest "test/parsing.ml" (15,5) (15,44) []
	; mktest "test/parsing.ml" (15,5) (15,22) [] ]

let _ =
	run_test_tt_main fv_suite |> ignore ;
	run_test_tt_main mapper_suite |> ignore
