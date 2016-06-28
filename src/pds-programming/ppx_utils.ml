open Ast_mapper;;
open Batteries;;

(* Much of this code was captured from v4.02.3 of the OCaml compiler:
    https://github.com/ocaml/ocaml/blob/4.02/parsing/ast_mapper.ml
*)

let run_mappers (mappers : (string list -> mapper) list) =
  try
    let a = Sys.argv in
    let n = Array.length a in
    if n > 2 then
      let args = (Array.to_list (Array.sub a 1 (n - 3))) in
      let source = a.(n-2) in
      let target = a.(n-1) in
      let intermediates =
        1--(List.length mappers - 1)
        |> Enum.map (fun i -> source ^ "__" ^ string_of_int i)
        |> List.of_enum
      in
      let names =
        List.concat
          [ [source]
          ; intermediates
          ; [target]
          ]
      in
      let names' = List.tl names in
      let name_pairs =
        List.of_enum (Enum.combine (List.enum names, List.enum names'))
      in
      let tasks = List.combine mappers name_pairs in
      tasks
      |> List.iter
        (fun (mapper_fn,(source_name,target_name)) ->
           let mapper = mapper_fn args in
           apply ~source:source_name ~target:target_name mapper
        )
    else begin
      Printf.eprintf "Usage: %s [extra_args] <infile> <outfile>\n%!"
        Sys.executable_name;
      exit 2
    end
  with exn ->
    prerr_endline (Printexc.to_string exn);
    exit 2
;;
