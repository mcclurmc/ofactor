
open Asttypes
open Parsetree

open Ast_mapper

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

(* main driver needs to use commandliner to get source file name and
	 args. Args are a range of locations. Locations should be in format:
	 <l>:<c> or <char> *)

let main = ()

(* given a source file, and a location, can we find the AST node that
	 occurs nearest to, but not after, this location? *)

let string_of_pos (pos : Lexing.position) =
	Printf.sprintf "l%dc%d"
		pos.Lexing.pos_lnum
		(pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let string_of_loc loc =
	Printf.sprintf "%s:%s"
		(string_of_pos loc.Location.loc_start)
		(string_of_pos loc.Location.loc_end)

let print_loc loc =
	print_endline (string_of_loc loc)

let fix_pos_offset p =
	let open Lexing in
	if p.pos_bol = -1
	then p
	else { p with
		pos_cnum = p.pos_cnum - p.pos_bol;
		pos_bol = -1 }

let cmp_pos p1 p2 =
	let open Lexing in

	let p1 = fix_pos_offset p1
	and p2 = fix_pos_offset p2 in

	let c1 = compare p1.pos_lnum p2.pos_lnum
	and c2 = compare p1.pos_cnum p2.pos_cnum in

	if c1 = 0 then c2 else c1

(* let pos_in_loc p l = *)
(* 	let open Lexing in *)
(* 	let open Location in *)

(* 	let l1 = l.loc_start *)
(* 	and l2 = l.loc_end in *)

(* 	(p.pos_lnum >= l1.pos_lnum && p.pos_lnum <= l2.pos_lnum) *)
(* 	&& *)
(* 		(p.pos_cnum >= (l1.pos_cnum - l1.pos_bol) *)
(* 		 && p.pos_cnum <= (l2.pos_cnum - l2.pos_bol)) *)

let pos_in_loc p l =
	let open Lexing in
	let open Location in

	let l1 = l.loc_start
	and l2 = l.loc_end in

	(* A position is "within" a location if it is "greater than or
		 equal" the location's starting postion, and "less than or equal
		 to" the location's ending position. *)
	(cmp_pos p l1 >= 0) && (cmp_pos p l2 <= 0)

let range_in_loc p1 p2 l =
	(pos_in_loc p1 l) && (pos_in_loc p2 l)

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

let intersect l1 l2 = List.(filter (fun a -> mem a l2) l1)

let flatmap f l = List.(flatten (map f l))

let mapfst ls = List.map fst ls
let mapsnd ls = List.map snd ls

let optmap f = function
	| None -> []
	| Some a -> f a

let uniq ?(cmp=compare) ls =
	List.(fold_left
		begin
			fun acc item -> match acc with
			| [] -> [item]
			| first :: rest when first = item -> acc
			| _ -> item :: acc
		end
		[] (sort cmp ls))

let mkvar s = Longident.Lident s

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

let pevars (p,_) = pvars p

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

	| Pexp_letmodule (_, m, e) -> (fv_exp e) @ (fv_mod m)

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

and fv_mod s = failwith "fv_mod unimplemented" (* TODO: implement *)

let fv_exp e = fv_exp e |> uniq ~cmp:cmp_lident

let string_of_idents fvs =
	List.map Longident.last fvs |> String.concat ", "

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
		match exp with
		| None -> []
		| Some e ->
			let fvs = fv_exp e in
			Printf.printf "\n  bound vars: %s\n"
				(string_of_idents bound_vars) ;
			intersect fvs bound_vars

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
			| _ -> failwith "range does not specify an expression" in
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

	(* TODO: bound variables. Eventually, we'll need to know the variables
		 bound in the scope _between_: (1) the level to which we're
		 extracting the new function, and (2), the level at which the
		 expression we're extracting has been defined. *)
	(* let rec bv s e acc = match s.pstr_desc with *)
	(* method bv_str s e acc = match s.pstr_desc with *)
	(* 	| Pstr_value (Nonrecursive, pes, _) -> *)
	(* 		acc *)
	(* 	| Pstr_value (Recursive, _, _) -> *)
	(* 		failwith "bv: Pstr_value Recursive unimplemented" *)
	(* 	| _ -> acc *)

	(* method bv_exp es e_stop acc = *)
	(* 	match es with *)
	(* 	| [] -> [] *)
	(* 	| e::es -> *)
	(* 		(\* TODO: Only return acc if we've reached the expression we're *)
	(* 			 targetting. Otherwise, we might have run down a branch which we *)
	(* 			 don't care about. Consider looking for the variables bound at a *)
	(* 			 particular match case expression: we would only want the variables *)
	(* 			 bound up to that point, and not those bound on other branches. *\) *)
	(* 		if e = e_stop then acc else *)
	(* 			match e.pexp_desc with *)
	(* 			(\* Terminal cases *\) *)
	(* 			| Pexp_ident _ *)
	(* 			| Pexp_constant _ *)
	(* 			| Pexp_new _ *)

	(* 			(\* Recursing cases *\) *)
	(* 			(\* |  *\) *)

	(* 			(\* Binding cases *\) *)
	(* 			| Pexp_let _ -> this # bv_exp es e_stop acc *)
	(* 			| Pexp_function _ -> this # bv_exp es e_stop acc *)

	(* 			(\* TODO: other cases *\) *)
	(* 			| Pexp_fun (_, _, _, _) *)
	(* 			| Pexp_apply (_, _) *)
	(* 			| Pexp_match (_, _) *)
	(* 			| Pexp_try (_, _) *)
	(* 			| Pexp_tuple _ *)
	(* 			| Pexp_construct (_, _) *)
	(* 			| Pexp_variant (_, _) *)
	(* 			| Pexp_record (_, _) *)
	(* 			| Pexp_field (_, _) *)
	(* 			| Pexp_setfield (_, _, _) *)
	(* 			| Pexp_array _ *)
	(* 			| Pexp_ifthenelse (_, _, _) *)
	(* 			| Pexp_sequence (_, _) *)
	(* 			| Pexp_while (_, _) *)
	(* 			| Pexp_for (_, _, _, _, _) *)
	(* 			| Pexp_constraint (_, _) *)
	(* 			| Pexp_coerce (_, _, _) *)
	(* 			| Pexp_send (_, _) *)
	(* 			| Pexp_setinstvar (_, _) *)
	(* 			| Pexp_override _ *)
	(* 			| Pexp_letmodule (_, _, _) *)
	(* 			| Pexp_assert _ *)
	(* 			| Pexp_lazy _ *)
	(* 			| Pexp_poly (_, _) *)
	(* 			| Pexp_object _ *)
	(* 			| Pexp_newtype (_, _) *)
	(* 			| Pexp_pack _ *)
	(* 			| Pexp_open (_, _) *)
	(* 			| Pexp_extension _ -> *)
	(* 				failwith "bv_exp unfinished cases" *)

	(* let bv s e = bv s e [] |> uniq ~cmp:cmp_lident *)

end

let ast_of_file fn =
	open_in fn
	|> Lexing.from_channel
	|> Parse.implementation

let mkpos l ?(b=(-1)) c =
	let open Lexing in
	{ pos_fname = "dummy"
	; pos_lnum = l
	; pos_bol = b
	; pos_cnum = c }

let _ =
	let pos1 = mkpos 6 11 in
	let pos2 = mkpos 6 16 in
	let m = search_mapper(pos1, pos2) in
	Printf.printf "running search_mapper on %s:%s\n"
		(string_of_pos pos1) (string_of_pos pos2) ;
	let ast = ast_of_file "test/parsing.ml" in
	m # implementation "" ast |> ignore ;
	match m # get_exp with
	| None -> print_endline "  FAILED to retrieve expression"
	| Some e ->
		Printf.printf "retrieved expression at: %s"
			(string_of_loc e.pexp_loc) ;
		let fvs = m # get_free_vars in
		Printf.printf "  free vars U bound vars: %s\n" (string_of_idents fvs)

(* Tests *)

let unwrap_str_eval = function
	| { pstr_desc = Pstr_eval (e,_)
		; pstr_loc = _ } -> e
	| _ -> failwith "unwrap_str_eval"

let parse_string s = s
	|> Lexing.from_string
	|> Parse.implementation
	|> List.hd
	|> unwrap_str_eval

let print_idents is =
	let is = List.map Longident.last is in
	print_endline (String.concat ", " is)

open OUnit

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
	let p = parse_string s in
	let fvs = fv_exp p in
	let vs = List.map mkvar vs in

	if !debug then
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

let _ = run_test_tt_main fv_suite
