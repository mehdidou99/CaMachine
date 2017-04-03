type 'a dict == (string * 'a) list ref;;

let make_dict() = ref [];;
let add dict key val = dict := (key, val)::!dict;;
let rec get_aux l key =
	match l with
		[] -> invalid_arg "Key not found."
		| (k, v)::q -> if k = key then v else get_aux q key
;;
let get dict key = get_aux !dict key;;
let rec remove_aux l key =
	match l with
		[] -> []
		| (k, v)::q -> if k = key then remove_aux q key else (k,v)::(remove_aux q key)
;;

let remove dict key = dict := remove_aux !dict key;;