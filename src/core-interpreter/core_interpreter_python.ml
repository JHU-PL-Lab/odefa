open Batteries;;
open Jhupllib;;

open Core_ast;;
(* open Core_ast_pp;; *)

let lazy_logger = Logger_utils.make_lazy_logger "Interpreter";;

module Environment = Var_hashtbl;;

exception Evaluation_failure of string;;

let rv body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(Var(Ident(x), _),_) = List.last body in x
;;

let rv_var body =
  match body with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x, _) = List.last body in x
;;

let rec print_tabs num_tabs =
  if num_tabs = 0 then () else
    (print_string "\t"; print_tabs (num_tabs - 1))
;;

let rec print_pattern x p =
  match p with
  | Fun_pattern ->
    print_string ("if type(" ^ x ^ ") == types.FunctionType:" )
  | UInt_pattern
  | Int_pattern ->
    print_string ("if type(" ^ x ^ ") == int:" )
  | Bool_pattern pattern_boolean ->
    print_string ("if " ^ x ^ " == " );
    begin
      match pattern_boolean with
      | true -> print_string ("True")
      | false -> print_string ("False")
    end;
    print_string (":")
  | _ -> raise @@ Utils.Invariant_failure "Unhandled pattern match"
;;

let rec convert_to_python cls num_tabs =
  print_newline ();
  match cls with
  | [] -> ()
  | Clause(Var(Ident(x), _), cl) :: tl ->
    print_tabs num_tabs;
    begin
      match cl with
      | Value_body(Value_uint(v))
      | Value_body(Value_int(v)) ->
        print_string (x ^ " = ");
        print_int v
      | Value_body(Value_bool(v)) ->
        print_string (x ^ " = ");
        begin
          match v with
          | true -> print_string ("True")
          | false -> print_string ("False")
        end
      | Value_body(Value_function(Function_value(Var(Ident(x'), _), Expr(cls)))) ->
        print_string ("def " ^ x ^ "(" ^ x' ^ "):");
        convert_to_python cls (num_tabs + 1);

        print_tabs (num_tabs + 1);
        print_string ("return " ^ (rv cls))
      | Var_body(Var(Ident(x'),_)) ->
        print_string (x ^ " = " ^ x')
      | Appl_body(Var(Ident(xf),_), Var(Ident(xv),_)) ->
        print_string (x ^ " = " ^ xf ^ "(" ^ xv ^ ")")
      | Binary_operation_body(Var(Ident(x1),_),op,Var(Ident(x2),_)) ->
        begin
          match op with
          | Binary_operator_uint_plus
          | Binary_operator_plus -> print_string (x ^ " = (" ^ x1 ^ " + " ^ x2 ^ ")")
          | Binary_operator_uint_minus
          | Binary_operator_int_minus -> print_string (x ^ " = (" ^ x1 ^ " - " ^ x2 ^ ")")
          | Binary_operator_uint_equal_to
          | Binary_operator_equal_to -> print_string (x ^ " = (" ^ x1 ^ " == " ^ x2 ^ ")")
          | Binary_operator_uint_less_than
          | Binary_operator_int_less_than -> print_string (x ^ " = (" ^ x1 ^ " < " ^ x2 ^ ")")
          | Binary_operator_uint_less_than_or_equal_to
          | Binary_operator_int_less_than_or_equal_to -> print_string (x ^ " = (" ^ x1 ^ " <= " ^ x2 ^ ")")
          | Binary_operator_bool_and -> print_string (x ^ " = (" ^ x1 ^ " and " ^ x2 ^ ")")
          | Binary_operator_bool_or -> print_string (x ^ " = (" ^ x1 ^ " or " ^ x2 ^ ")")
          | _ ->
           raise @@ Evaluation_failure "Incorrect binary operation"
        end
      | Unary_operation_body(op,Var(Ident(x1),_)) ->
        begin
          match op with
          | Unary_operator_bool_not -> print_string (x ^ " = not " ^ x1)
          | _ ->
           raise @@ Evaluation_failure "Incorrect unary operation"
        end
      | Conditional_body(Var(Ident(xc),_),p,(Function_value(Var(Ident(x1),_), Expr(e1))),(Function_value(Var(Ident(x2),_), Expr(e2)))) ->
        print_pattern xc p;
        print_newline ();

        print_tabs (num_tabs + 1);
        print_string ("def " ^ x ^ "_f1(" ^ x1 ^ "):");

        convert_to_python e1 (num_tabs + 2);
        print_tabs (num_tabs + 2);
        print_string ("return " ^ (rv e1));
        print_newline ();

        print_tabs (num_tabs + 1);
        print_string (x ^ " = " ^ x ^ "_f1(" ^ xc ^ ")");
        print_newline ();

        print_tabs num_tabs;
        print_endline "else:";
        print_newline ();

        print_tabs (num_tabs + 1);
        print_string ("def " ^ x ^ "_f2(" ^ x2 ^ "):");

        convert_to_python e2 (num_tabs + 2);
        print_tabs (num_tabs + 2);
        print_string ("return " ^ (rv e2));
        print_newline ();

        print_tabs (num_tabs + 1);
        print_string (x ^ " = " ^ x ^ "_f2(" ^ xc ^ ")");
        print_newline ()

      | _ -> raise @@ Utils.Invariant_failure "Unhandled clause"
    end;
    convert_to_python tl num_tabs
;;

let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t =
  print_newline ();
  print_string "import types\n";
  convert_to_python cls 0;
  print_newline ();
  print_string ("print " ^ (rv cls) ^ "\n");
  let v = Value_int(3) in
  let env = Core_interpreter.Environment.create 10 in
  Core_interpreter.Environment.add env (rv_var cls) v;
  (rv_var cls), env
;;
