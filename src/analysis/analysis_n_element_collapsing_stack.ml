(*
This module models a context stack as a restricted regular expression.  The form
of the regular expression is limited to a string of basic terms, where each term
is either a symbol or a Kleene plus of a set of symbols.  The regular expression
is further limited to include each symbol at most once -- that is, the string
"aba" is illegal and must instead be represented as "(ab)*" -- and to have no
more than a fixed number of symbols.
*)

open Batteries;;

open Analysis_context_stack;;
open Ddpa_graph;;
open Logger_utils;;

let lazy_logger =
  Logger_utils.make_lazy_logger "Analysis_n_element_collapsing_stack"
;;

module type Spec =
sig
  val size : int
end;;

module Make(S : Spec) : Context_stack =
struct
  type term =
    | Symbol of abstract_clause
    | KleenePlus of Abs_clause_set.t
    [@@deriving ord]
  ;;
  let pp_term term =
    let pp c = Printf.sprintf "(%s)" (pp_abstract_clause c) in
    match term with
    | Symbol c -> pp c
    | KleenePlus s ->
      String_utils.concat_sep_delim "{" "}" ", " @@ Enum.map pp @@
      Abs_clause_set.enum s
  ;;
  let ppa_term term =
    match term with
    | Symbol c -> ppa_abstract_clause c
    | KleenePlus s ->
      String_utils.concat_sep_delim "(" ")+" "|" @@
      Enum.map ppa_abstract_clause @@ Abs_clause_set.enum s

  let symbols_of_term term =
    match term with
    | Symbol s -> Abs_clause_set.singleton s
    | KleenePlus ss -> ss
  ;;

  (** The type of the context.  This is a pair between the regex and a
      quick-reference set of all symbols in that regex.  As an invariant, the
      the second component is always equal to the set containing everything
      from the first component. *)
  type t = term Deque.dq * Abs_clause_set.t;;

  let pp x =
    String_utils.concat_sep_delim "" "|?" "|" @@
    Enum.map pp_term @@ Deque.enum @@ fst x
  ;;
  let ppa x =
    String_utils.concat_sep_delim "" "|?" "|" @@
    Enum.map ppa_term @@ Deque.enum @@ fst x
  ;;
  let compare x y =
    Enum.compare compare_term (Deque.enum @@ fst x) (Deque.enum @@ fst y)
  ;;
  let empty = (Deque.empty, Abs_clause_set.empty);;
  let push c ((regex,members) as ctx) =
    (* Some logging stuff... *)
    lazy_bracket_log (lazy_logger `trace)
      (fun () -> Printf.sprintf "Pushing %s onto %s"
          (ppa_abstract_clause c) (ppa ctx))
      (fun result ->
         let result' =
           result
           |> Enum.clone
           |> Enum.map ppa
           |> String_utils.concat_sep_delim "{" "}" ", "
         in
         Printf.sprintf "Results: %s" result'
      )
    @@ fun () ->
    (* The actual push logic. *)
    if Abs_clause_set.mem c members
    then
      (* This symbol is familiar.  Time to collapse a bunch of stuff! *)
      let rec digest regex' symbols_so_far =
        match Deque.front regex' with
        | None ->
          (* It shouldn't have been possible for us to get through the entire
             thing if the symbol set said that we'd seen the symbol before. *)
          raise @@ Utils.Invariant_failure
            "Regex claimed a familiar symbol it did not contain"
        | Some (term, regex'') ->
          let symbols = symbols_of_term term in
          let symbols_so_far' = Abs_clause_set.union symbols symbols_so_far in
          if Abs_clause_set.mem c symbols
          then Deque.cons (KleenePlus symbols_so_far') regex''
          else digest regex'' symbols_so_far'
      in
      let result_regex = digest regex Abs_clause_set.empty in
      Enum.singleton (result_regex, members)
    else
      (* We've never seen this symbol before.  We'll add it as a singleton. *)
      let regex' = Deque.cons (Symbol c) regex in
      let members' = Abs_clause_set.add c members in
      let (regex'', members'') =
        if Abs_clause_set.cardinal members' <= S.size
        then (regex',members')
        else
          let (regex_to_use, lost_piece) = Option.get @@ Deque.rear regex' in
          let lost_members = symbols_of_term lost_piece in
          (regex_to_use, Abs_clause_set.diff members' lost_members)
      in
      Enum.singleton (regex'', members'')
  ;;
  let rec pop ((regex,members) as ctx) =
    (* Some logging stuff... *)
    lazy_bracket_log (lazy_logger `trace)
      (fun () -> Printf.sprintf "Popping from %s" (ppa ctx))
      (fun result ->
         let result' =
           result
           |> Enum.clone
           |> Enum.map ppa
           |> String_utils.concat_sep_delim "{" "}" ", "
         in
         Printf.sprintf "Results: %s" result'
      )
    @@ fun () ->
    (* The actual pop logic. *)
    match Deque.front regex with
    | None -> Enum.singleton empty
    | Some(term,regex') ->
      match term with
      | Symbol c ->
        Enum.singleton (regex', Abs_clause_set.remove c members)
      | KleenePlus s ->
        (* There are two cases to consider.
           1. The Kleene closure represents two or more elements.  In this case,
              a duplicate is popped and the representation remains unchanged.
           2. The Kleene closure represents a single element.  In this case,
              it is discarded and the result is the answer.
           We'll calculate results for each of these cases. *)
        let members' = Abs_clause_set.diff members s in
        let ctx' = (regex',members') in
        List.enum [ctx;ctx']
  ;;
  let is_top c (regex_in,_) =
    let rec loop regex =
      match Deque.front regex with
      | None -> true
      | Some(term,regex') ->
        match term with
        | Symbol c' -> equal_abstract_clause c c'
        | KleenePlus s ->
          if Abs_clause_set.mem c s
          then true
          else loop regex'
    in
    loop regex_in
  ;;
  let name = string_of_int S.size ^ "ddpa";;
end;;
