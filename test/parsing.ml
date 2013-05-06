(* Test cases to be parsed by ofactor *)

let f a b =
	let g c d =
		match c with
		| true -> a + d
		| false -> b + d in
	let rec h e f =
		e + f in
	g true (h a b)
