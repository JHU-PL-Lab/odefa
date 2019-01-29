open Batteries;;
open Jhupllib;;

open Core_ast;;
open Core_interpreter_utils;;

(* open Sys;; *)
open Unix;;
open Yojson.Basic.Util;;


(* returns the last program point of the program *)
let rv (cls: clause list) : var =
  match cls with
  | [] -> raise @@ Utils.Invariant_failure "empty function body provided to rv"
  | _ -> let Clause(x,_) = List.last cls in x
;;

(*
  lookup function
*)
let rec lookup lookup_stack (node:annotated_clause) context_stack graph iota: (Core_ast.value * formula * input_mapping) =
  failwith "TODO"
;;

let script formula =
  execv "/usr/bin/python" [| "python";"/home/theodore/research/odefa/src/core-interpreter/test.py";formula|]
;;

let eval (Expr(cls)) : Core_ast.var * value Core_interpreter.Environment.t * formula * input_mapping =

  (* let temp_formula = "x + y > 5" in
     let _ = handle_unix_error script temp_formula in *)


  let json = Yojson.Basic.from_file "ddpa_graphs.json" in
  (* let open Yojson.Basic.Util in *)
  let test = json |> member "element_type" |> to_string in
  print_endline ("TESTING: " ^ test);


  failwith "TODO"


;;
