
(* open Ofactor *)
open Cmdliner

let (|>) = Helpers.(|>)

(*
	Discussion of command line interface:

	Extract function from expresion between line 5, char 6 to line 7,
	char 40, to a new function at toplevel, called new_fun. The output
	of this is the new function, and the location of the beginning of
	the structure item from which this funciton is being extracted, so
	that the editor can put the new function there. The editor can worry
	about deleting the original text.

	> # ofactor extract -r l5c6:l7c40 -n new_fun program.ml
	> l3c0
	> let new_fun a b c = ...

	Same as above, but to an inline function. This case will be
	trickier, because we'll need to decide where to put the new function
	based on what the surounding structure item looks like (let foo =
	function | Case..., for instance). Here I suppose we'll have to
	output the whole toplevel function, with the range that marks the
	start and end of the original structure item, so the editor can
	replace that text.

	> # ofactor extract -r l5c6:l7c40 -n new_fun -i program.ml
	> l2c0:20:40
	> let outer_fun x y =
	>   let new_fun a b c = ...

	So a point tells the editor to insert the rest of the output at that
	point, and a range tells the editor to replace that range of text
	with the rest of the output.

	The general form of the command seems to be taking the shape of:

	ofactor <command> <options> <target filename>

*)

(** ofactor commnads  *)

let rec extract r n f = match range r with
	| None -> failwith "TODO: extract requires range"
	| Some r' ->
		let pos, fun_string = Ofactor.extract r' n f in
		print_endline pos ;
		print_endline fun_string

and point s =
	let open Re_str in
	let r = regexp "l\\([0-9]+\\)c\\([0-9]+\\)" in
	if string_match r s 0
	then begin
		let l = matched_group 1 s |> int_of_string
		and c = matched_group 2 s |> int_of_string in
		(l,c)
	end
	else failwith ("Couldn't find point in " ^ s)

and range = function
	| None -> None
	| Some s ->
(* and range s = *)
		let r = Re_str.regexp ":" in
		match Re_str.split r s with
		| [s1; s2] -> Some (point s1, point s2)
		| _ -> None

let range_arg =
	let doc = "Range of the form l<num>c<num>:l<num>c<num>." in
	Arg.(value & opt (some string) None & info ["r"] ~docv:"range" ~doc)

let fun_name_arg =
	let doc = "Name of the extracted function." in
	Arg.(value & opt string "func" & info ["n"] ~docv:"name" ~doc)

let file_arg =
	let doc = "Filename of module to operate on." in
	Arg.(required & pos ~rev:true 0 (some file) None & info [] ~doc)

let extract_cmd =

	let doc = "Extract a function from the expression at a given location." in
	let man = [
		`S "DESCRIPTION" ;
		`P "lorum ipsum lorum ipsum lorum ipsum lorum ipsum lorum ipsum lorum ipsum" ;
	] in

	Term.(pure extract $ range_arg $ fun_name_arg $ file_arg),
	Term.info "extract" ~doc ~man

let default_cmd =
  let doc = "A refactoring tool for OCaml. Currently supports function extraction." in
  Term.(ret (pure (`Ok ()))),
  Term.info "ofactor" ~version:"0.1" ~doc

let () = match Term.eval_choice default_cmd [extract_cmd] with
	| `Error _ -> exit 1
	| _ -> ()
