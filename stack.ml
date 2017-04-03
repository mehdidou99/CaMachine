type 'a stack == 'a list ref;;

let make_stack() = ref [];;
let is_empty s = !s = [];;
let push s x = s := x::!s;;
let pop s =
	match !s with
		[] -> invalid_arg "Empty stack."
		| t::q -> s := q; t
;;
let top s =
	match !s with
		[] -> invalid_arg "Empty stack."
		| t::q -> t
;;