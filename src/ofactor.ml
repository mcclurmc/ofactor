
open Asttypes
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
let (//) a b = diff a b ;;

let flatmap f l = List.(flatten (map f l)) ;;

let mapsnd ls = List.map snd ls ;;

let pvars _ = [] ;;

let pexps pes =
	List.map
		(fun pe -> (snd pe).pexp_desc)
		pes
;;

let rec fv = function
	(* fv(v) -> [v] *)
	| Pexp_ident _ as v
		-> [v]
	(* fv(e) -> fv(e1) @ fv(e2) ... *)
	(* fv(abs vs e) -> fv(e) \ vs *)
	(* | Pexp_let ((Nonrecursive|Default), pe, e) -> *)
	(* 	(fv e.pexp_desc) @ (flatmap fv (mapsnd pe)) *)
	| Pexp_let (rflag, pe, e) ->
		((fv e.pexp_desc) //
				(match rflag with
						| Nonrecursive | Default -> []
						| Recursive -> flatmap pvars pe))
		@ (flatmap fv (pexps pe))
	| Pexp_function (_, _, pe) ->
		flatmap
			(fun (v,e) -> (fv e.pexp_desc) // (pvars v))
			pe
	(* fv(_) -> [] *)
	| _ -> []
;;

(* let dummy = Location.in_file "dummy" ;; *)

let parse s = Parse.implementation (Lexing.from_string s) ;;

let test_fv () =
	(* let v_42 = parse "42" in *)
	let f1 = parse "function | x -> x" in
	Printf.printf "f1.length = %d\n" (List.length f1)
;;

let _ = test_fv () ;;
