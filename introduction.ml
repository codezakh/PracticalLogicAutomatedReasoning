type expression =
  Var of string
| Const of int
| Add of expression * expression
| Mul of expression * expression
| Power of expression * expression
;;
  
  
let simplify1 expr =
  match expr with
    Add(Const(m),Const(n)) -> Const(m + n)
| Mul(Const(m),Const(n)) -> Const(m * n)
| Add(Const(0),x) -> x
| Add(x,Const(0)) -> x
| Mul(Const(0),x) -> Const(0)
| Mul(x,Const(0)) -> Const(0)
| Mul(Const(1),x) -> x
| Mul(x,Const(1)) -> x
| _ -> expr;;


let rec simplify expr =
  match expr with
    Add(e1,e2) -> simplify1(Add(simplify e1,simplify e2))
  | Mul(e1,e2) -> simplify1(Mul(simplify e1,simplify e2))
  | _ -> simplify1 expr;;


let explode s =
  let rec expl i l =
    if i<0 then l else
      expl (i-1) (Bytes.make 1 s.[i] :: l) in
  expl (String.length s - 1) [];;
  

let matches s = let chars = explode s in fun c -> List.mem c chars;;


let space = matches " \t\n\r"
and punctuation = matches "()[]{},"
and symbolic = matches "~‘!@#$%^&*-+=|\\:;<>.?/"
and numeric = matches "0123456789"
and alphanumeric = matches
		     "abcdefghijklmnopqrstuvwxyz_’ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";;


let rec lexwhile prop inp =
  match inp with
    c::cs when prop c -> let tok,rest  = lexwhile prop cs in c^tok, rest
  | _ -> "", inp;;


let rec lex inp =
  match snd(lexwhile space inp) with
    [] -> []
  | c::cs -> let prop = if alphanumeric(c) then alphanumeric
			else if symbolic(c) then symbolic
			else fun c -> false in
	     let toktl, rest = lexwhile prop cs in
	     (c^toktl)::lex rest;;
  
		 
let rec parse_expression i =
  match parse_product i with
    e1, "+"::i1 -> let e2, i2 = parse_expression i1 in Add(e1,e2),i2
  | e1, i1 -> e1, i1
and parse_product i =
  match parse_atom i with
    e1, "*"::i1 -> let e2, i2 = parse_product i1 in Mul(e1,e2),i2
  | e1, "^"::i1 -> let e2, i2 = parse_product i1 in Power(e1, e2), i2
  | e1,i1 -> e1, i1
and parse_atom i =
  match i with
    [] -> failwith "Expected an expression at the end of input"
  | "("::i1 -> (match parse_expression i with
		  e2, ")"::i2 -> e2,i2
		| _ -> failwith "Expected closing bracket")
  | tok::i1 -> if List.for_all numeric (explode tok)
	       then Const(int_of_string tok),i1
	       else Var(tok), i1
;;
  

let make_parser pfn s =
  let expr, rest = pfn (lex(explode s)) in
  if rest = [] then expr else failwith "Unparsed input"
;;
  
let default_parser = make_parser parse_expression;;
