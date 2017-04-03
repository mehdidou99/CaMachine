load "machine.ml";;

#open "machine";;

let context = get_new_default_context();;
let code = [PUSH(INT 5); PUSH(INT 3); ADD; STORE "foo"; PUSH(INT 4); LOAD "foo"; PRINT; POP; PUSH(STR "Hello world !"); PRINT; POP];;
process_code context code;;
!(context.s);;
!(context.env);;

#close "machine";;
