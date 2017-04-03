load "stack.ml";;
load "dict.ml";;
load "utils.ml";;

#open "io";;
#open "stack";;
#open "dict";;
#open "utils";;

type types = INT of int | FLOAT of float | STR of string | BOOL of bool;;

type instructions =
	NOP | POP | PUSH of types | STORE of string | LOAD of string
	| ADD | SUB | MUL | DIV | MOD | UNARY_MINUS
	| AND | OR | NOT
	| EQ | NE | LT | LE | GT | GE
	| PRINT
	| BRANCH of code * code

and code == instructions list
;;

type context = {s : types stack; env : types dict};;

let process_arithmetic_binop context int_op float_op =
	match pop context.s, pop context.s with
		INT(i1), INT(i2) -> push context.s (INT(int_op i1 i2))
		| FLOAT(f1), FLOAT(f2) -> push context.s (FLOAT(float_op f1 f2))
		| _ -> invalid_arg "Invalid operand type."
;;

let process_boolean_binop context op =
	match pop context.s, pop context.s with
		BOOL(b1), BOOL(b2) -> push context.s (BOOL(op b1 b2))
		| _ -> invalid_arg "Invalid operand type."
;;

let have_same_type a b =
	match a, b with
		INT(_), INT(_) | FLOAT(_), FLOAT(_) | STR(_), STR(_) -> true
		| _ -> false
;;


let rec process_instruction context instruction =
	match instruction with
		NOP -> ()
		
		| POP -> utils__ignore(pop context.s)
		| PUSH(val) -> push context.s val
		| STORE(key) -> add context.env key (pop context.s)
		| LOAD(key) -> push context.s (get context.env key)
		
		| ADD -> let int_add a b = a + b and float_add a b = a +. b in process_arithmetic_binop context int_add float_add
		| SUB -> let int_sub a b = a - b and float_sub a b = a -. b in process_arithmetic_binop context int_sub float_sub
		| MUL -> let int_mul a b = a * b and float_mul a b = a *. b in process_arithmetic_binop context int_mul float_mul
		| DIV -> let int_div a b = a / b and float_div a b = a /. b in process_arithmetic_binop context int_div float_div
		| MOD -> (
				match pop context.s, pop context.s with
					| INT(i1), INT(i2) -> push context.s (INT(i1 mod i2))
					| _ -> invalid_arg "Invalid operand type."
			)
		| UNARY_MINUS -> (
					match pop context.s with
						INT(i) -> push context.s (INT(-i))
						| FLOAT(f) -> push context.s (FLOAT(-.f))
						| _ -> invalid_arg "Invalid operand type."
				)
		
		| AND -> process_boolean_binop context (fun b1 b2 -> b1 & b2)
		| OR -> process_boolean_binop context (fun b1 b2 -> b1 or b2)
		| NOT -> (
				match pop context.s with
					BOOL(b) -> push context.s (BOOL(not b))
					| _ -> invalid_arg "Invalid operand type."
			)

		| EQ -> let a = pop context.s in
			let b = pop context.s in
			if have_same_type a b then
				push context.s (BOOL(a = b))
			else
				invalid_arg "Invalid operand type."
		| NE -> let a = pop context.s in
			let b = pop context.s in
			if have_same_type a b then
				push context.s (BOOL(a <> b))
			else
				invalid_arg "Invalid operand type."
		| LT -> let a = pop context.s in
			let b = pop context.s in
			if have_same_type a b then
				push context.s (BOOL(a < b))
			else
				invalid_arg "Invalid operand type."
		| LE -> let a = pop context.s in
			let b = pop context.s in
			if have_same_type a b then
				push context.s (BOOL(a <= b))
			else
				invalid_arg "Invalid operand type."
		| GT -> let a = pop context.s in
			let b = pop context.s in
			if have_same_type a b then
				push context.s (BOOL(a > b))
			else
				invalid_arg "Invalid operand type."
		| GE -> let a = pop context.s in
			let b = pop context.s in
			if have_same_type a b then
				push context.s (BOOL(a >= b))
			else
				invalid_arg "Invalid operand type."

		| PRINT -> (
				print_string ">>> ";
				match top context.s with
					INT(i) -> print_int i
					| FLOAT(f) -> print_float f
					| STR(s) -> print_string s
					| BOOL(b) -> print_string (string_of_bool b)
				;
				print_newline()
			)

		| BRANCH(code_true, code_false) -> (
							match pop context.s with
								BOOL(true) -> process_code context code_true
								| BOOL(false) -> process_code context code_false
								| _ -> invalid_arg "Invalid operand type."
						)


and process_code context code =
	match code with
		[] -> ()
		| instruction::q -> process_instruction context instruction; process_code context q
;;

let get_new_default_context() = {s = make_stack(); env = make_dict()};;

#close "io";;
#close "stack";;
#close "dict";;
#close "utils";;
