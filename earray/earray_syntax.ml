(* Copyright (c) 2013 Radek Micek *)

(** Preprocessor for Earray syntax. *)

open Asttypes
open Parsetree
open Ast_helper
module Convenience = Ast_convenience

let n_pat_vars = ref 0

(** Creates a new variable for pattern matching. *)
let fresh_pat_var () : string =
  incr n_pat_vars;
  "earr_pat_var_" ^ string_of_int (!n_pat_vars)

(** Replaces [Ppat_array] subpatterns of the given pattern by fresh variables.
   Returns a new pattern and a list of introduced fresh variables where
   each variable is paired with elements from the corresponding array
   subpattern.

   Not all patterns are currently supported.
*)
let rec replace_array_pats_by_fresh_vars
    (pat : pattern)
    : pattern * (string * pattern list) list =
  match pat.ppat_desc with
    | Ppat_any -> (pat, [])
    | Ppat_var _ -> (pat, [])
    | Ppat_alias (p, lstr) ->
        let p2, xs = replace_array_pats_by_fresh_vars p in
        ({ pat with ppat_desc = Ppat_alias (p2, lstr) }, xs)
    | Ppat_constant _ -> (pat, [])
    | Ppat_interval _ -> (pat, [])
    | Ppat_tuple ps ->
        let ps2, xs =
          List.map (replace_array_pats_by_fresh_vars) ps
          |> List.split
          |> (fun (ps2, xss) -> (ps2, xss |> List.concat)) in
        ({ pat with ppat_desc = Ppat_tuple ps2 }, xs)
    | Ppat_construct (_, None) -> (pat, [])
    | Ppat_construct (lab, Some p) ->
        let p2, xs = replace_array_pats_by_fresh_vars p in
        ({ pat with ppat_desc = Ppat_construct (lab, Some p2) }, xs)
    | Ppat_variant _ -> failwith "Ppat_variant"
    | Ppat_record _ -> failwith "Ppat_record"
    | Ppat_array ps ->
        let var = fresh_pat_var () in
        let lstr = { txt = var; loc = pat.ppat_loc } in
        ({ pat with ppat_desc = Ppat_var lstr }, [var, ps])
    | Ppat_or _ -> failwith "Ppat_or"
    | Ppat_constraint _ -> failwith "Ppat_constraint"
    | Ppat_type _ -> failwith "Ppat_type"
    | Ppat_lazy _ -> failwith "Ppat_lazy"
    | Ppat_unpack _ -> failwith "Ppat_unpack"
    | Ppat_exception _ -> failwith "Ppat_exception"
    | Ppat_extension _ -> failwith "Ppat_extension"

let n_result_vars = ref 0

(** Creates a new variable for storing result of pattern match. *)
let fresh_result_var () =
  incr n_result_vars;
  "earr_result_var_" ^ string_of_int (!n_result_vars)

let lid = Convenience.lid

let elid = Convenience.evar

(** Expression [false]. *)
let false_ = Convenience.constr "false" []

(** Expression [true]. *)
let true_ = Convenience.constr "true" []

let ptuple = function
  | [ pat ] -> pat
  | pats -> Pat.tuple pats

let etuple = function
  | [ exp ] -> exp
  | exps -> Exp.tuple exps

let case lhs guard rhs =
  {
    pc_lhs = lhs;
    pc_guard = guard;
    pc_rhs = rhs;
  }

(** Let [xs] be a nonempty list of pairs [(xi, [p1; ..; pni])] where [xi] is
   a variable of type [Earray.t] and [p1; ..; pni] are patterns for
   its elements. The call [transform_pm_into_guard guard rhs result_var xs]
   generates boolean expression which tests the following conditions:

   - each array [xi] has length [ni],
   - elements of the [i]-th array [xi] match patterns [p1; ..; pni]
   - and [guard] is true.

   If all conditions are satisfied then [rhs] is evaluated, its
   value is stored into [result_var] and whole expression evaluates to [true].
   Otherwise the the whole expression evaluates to [false] and [rhs]
   isn't evaluated.

   [Ppat_array] subpatterns in patterns are transformed to match
   values of the type [Earray.t].
*)
let rec transform_pm_into_guard
    (guard : expression option)
    (rhs : expression)
    (result_var : string)
    (xs : (string * pattern list) list)
    : expression =
  let and_app x y = Exp.apply (elid "&&") ["", x; "", y] in
  let test_lengths_expr =
    let tests =
      List.map
        (fun (arr_var, ps) ->
          let len = Exp.apply (elid "Earray.length") ["", elid arr_var] in
          let const = Convenience.int (List.length ps) in
          Exp.apply (elid "=") ["", len; "", const])
        xs in
    List.fold_left and_app (List.hd tests) (List.tl tests) in
  let elems_tuple =
    xs
    |> List.map
         (fun (var, ps) ->
           let get_nth_elem i =
             Exp.apply
               (elid "Earray.get")
               ["", elid var; "", Convenience.int i] in
           List.mapi (fun i _ -> get_nth_elem i) ps)
    |> List.concat
    |> etuple in
  let pats_tuple = ptuple (List.concat (List.map snd xs)) in
  let pats_tuple2, xs2 = replace_array_pats_by_fresh_vars pats_tuple in
  let case_matches =
      match xs2 with
        (* Patterns don't contain any [Ppat_array] subpatterns. *)
        | [] ->
            let save_rhs =
              let opt_rhs = Convenience.constr "Some" [rhs] in
              Exp.apply
                (elid ":=")
                ["", elid result_var; "", opt_rhs] in
            case
              pats_tuple
              (* Guard is always set. Otherwise if the patterns are
                 irrefutable the [case_not_matches] would result in warning.
              *)
              (match guard with
                | None -> Some true_
                | Some _ -> guard)
              (Exp.sequence save_rhs true_)
        (* Patterns contain some [Ppat_array] subpatterns. *)
        | _ ->
            case
              pats_tuple2
              (Some (transform_pm_into_guard guard rhs result_var xs2))
              true_ in
  let case_not_matches = Exp.case (Pat.any ()) false_ in
  let match_expr = Exp.match_ elems_tuple [case_matches; case_not_matches] in

  and_app test_lengths_expr match_expr

(** Repeatedly removes [Ppat_or] patterns at the top of the case pattern
   by duplicating the case.
*)
let rec remove_top_or_pats (case : case) : case list =
  match case.pc_lhs.ppat_desc with
    | Ppat_or (a, b) ->
        let case_a = { case with pc_lhs = a } in
        let case_b = { case with pc_lhs = b } in
        List.append (remove_top_or_pats case_a) (remove_top_or_pats case_b)
    | _ -> [case]

(** Transforms array patterns in the given case to match values
   of the type [Earray.t].
*)
let transform_case
    (mapper : Ast_mapper.mapper)
    (result_var : string)
    (case : case)
    : case =
  let open Ast_mapper in
  (* Transform array patterns in matches marked with [%earr] in the guard
     and in the right-hand side to match values of the type [Earray.t].
  *)
  let tguard = map_opt (mapper.expr mapper) case.pc_guard in
  let trhs = mapper.expr mapper case.pc_rhs in
  let pat, xs = replace_array_pats_by_fresh_vars case.pc_lhs in
  match xs with
    (* Pattern doesn't contain array subpatterns. *)
    | [] -> { case with pc_guard = tguard; pc_rhs = trhs }
    | _ ->
        let read_result =
          Exp.match_
            (Exp.apply (elid "!") ["", elid result_var])
            [
              Exp.case
                (Convenience.pconstr "None" [])
                (Exp.apply
                   (elid "failwith")
                   ["", Convenience.str "earr syntax: no result"]);
              Exp.case
                (Convenience.pconstr "Some" [Convenience.pvar "res"])
                (elid "res");
            ] in
        with_default_loc case.pc_lhs.ppat_loc
          (fun () ->
            {
              pc_lhs = pat;
              pc_guard =
                Some (transform_pm_into_guard tguard trhs result_var xs);
              (* Guard will store the value of the original right-hand side
                 into a variable from [result_var] and we have to read it.
              *)
              pc_rhs = read_result;
            })

(** Transforms array patterns in matches marked with [%earr] to match values
   of the type [Earray.t].
*)
let mapper _ =
  let open Ast_mapper in
  let super = default_mapper in
  { super with
    expr =
      (fun this e ->
        match e.pexp_desc with
          | Pexp_extension (
              { txt = "earr" },
              PStr [{ pstr_desc = Pstr_eval ({
                pexp_loc = loc; pexp_desc = Pexp_match (e, cases) }, _) }])
            ->
              let result_var = fresh_result_var () in
              let result_vb =
                Vb.mk
                  (Convenience.pvar result_var)
                  (Exp.apply
                     (elid "ref")
                     ["", Convenience.constr "None" []]) in
              let e2 = this.expr this e in
              let cases2 =
                cases
                |> List.map remove_top_or_pats
                |> List.concat
                |> List.map (transform_case this result_var) in
              let match_expr = Exp.match_ ~loc e2 cases2 in
              Convenience.let_in [result_vb] match_expr

          | _ -> super.expr this e);
  }

let () = Ast_mapper.run_main mapper
