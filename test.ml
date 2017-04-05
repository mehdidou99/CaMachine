
load "machine.ml";;

#open "machine";;

let context = get_new_default_context();;
let code = [PUSH(INT 1); PUSH(INT 3); GT];;
process_code context code;;
!(context.s);;
!(context.env);;

#close "machine";;