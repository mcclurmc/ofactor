
open Asttypes
open Parsetree
open Helpers
open OUnit
open Ofactor

(** Tests *)

let unwrap_str_eval = function
	| { pstr_desc = Pstr_eval (e,_)
		; pstr_loc = _ } -> e
	| _ -> failwith "unwrap_str_eval"

let parse_string_str s = s
	|> Lexing.from_string
	|> Parse.implementation
	|> List.hd

let parse_string s = parse_string_str s |> unwrap_str_eval

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
		Printf.printf "  free vars /\\ bound vars: %s\n" (string_of_idents fvs) ;

		(* Build a function out of fvs and m#get_exp *)
		let the_fun = make_str_value "gen_func" fvs e in

		let p = Pprintast.default
		and f = Format.str_formatter in

		p # structure_item f the_fun ;
		Printf.printf "generated function:\n  [%s]\n"
			(Format.flush_str_formatter ())

let run_parsing_tests () =
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
	and expected = SetVar.of_list (List.map mkvar expected) in
	SetVar.assert_equal expected fvs

let mktest fn (l1,c1) (l2,c2) expected =
	(Printf.sprintf "%s - l%dc%d:l%dc%d" fn l1 c1 l2 c2)
	>:: (mktest fn (l1,c1) (l2,c2) expected)

let mapper_suite = "ofactor search_mapper" >:::
	[ mktest "test/parsing.ml" (6,11) (6,16)  ["a"; "b"; "c"; "d"]
	; mktest "test/parsing.ml" (9,4)  (9,13)  ["e"; "f"; "b"]
	; mktest "test/parsing.ml" (13,2) (15,46) ["pos"]
	; mktest "test/parsing.ml" (15,5) (15,44) ["pos"]
	; mktest "test/parsing.ml" (15,5) (15,22) ["pos"] ]

let mktest fn (l1,c1) (l2,c2) expected () =
	let p1 = mkpos l1 c1 and p2 = mkpos l2 c2
	and ast = ast_of_file fn in
	let m = search_mapper (p1, p2) in

	m # implementation "" ast |> ignore ;

	match m # get_str, m # get_exp with
	| Some s, Some e ->
		let bound = bv_str s e [] |> SetVar.of_list
		and expected = SetVar.of_list  (List.map mkvar expected) in
		SetVar.assert_equal expected bound
	| _ -> ()

let mktest fn (l1,c1) (l2,c2) expected =
	(Printf.sprintf "%s - l%dc%d:l%dc%d" fn l1 c1 l2 c2)
	>:: (mktest fn (l1,c1) (l2,c2) expected)

let bound_suite =
	let ptest = mktest "test/parsing.ml" in
	"ofactor bound vars" >:::
	[ ptest (10,2) (10,16) ["a"; "b"; "g"; "h"]
	; ptest (9,4) (9,12) ["a"; "b"; "g"; "h"; "e"; "f"]
	; ptest (5,4) (7,19) ["a"; "b"; "c"; "d"] ]

let _ =
	if !verbose then run_parsing_tests () ;
	run_test_tt_main fv_suite |> ignore ;
	run_test_tt_main mapper_suite |> ignore ;
	run_test_tt_main bound_suite |> ignore

