
(** Helper functions *)

external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let string_of_pos (pos : Lexing.position) =
	Printf.sprintf "l%dc%d"
		pos.Lexing.pos_lnum
		(pos.Lexing.pos_cnum - pos.Lexing.pos_bol)

let string_of_loc loc =
	Printf.sprintf "%s:%s"
		(string_of_pos loc.Location.loc_start)
		(string_of_pos loc.Location.loc_end)

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

let pos_in_loc p l =
	let open Lexing in
	let open Location in

	let l1 = l.loc_start
	and l2 = l.loc_end in

	(cmp_pos p l1 >= 0) && (cmp_pos p l2 <= 0)

let range_in_loc p1 p2 l =
	(pos_in_loc p1 l) && (pos_in_loc p2 l)

let cmp_lident a b = Longident.(compare (last a) (last b))

let string_of_idents fvs =
	List.map Longident.last fvs |> String.concat ", "

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

let mkpos l ?(b=(-1)) c =
	let open Lexing in
	{ pos_fname = "dummy"
	; pos_lnum = l
	; pos_bol = b
	; pos_cnum = c }

let mkvar s = Longident.Lident s
