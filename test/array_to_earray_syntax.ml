(* Copyright (c) 2014 Radek Micek *)

open Asttypes
open Parsetree
open Ast_helper
module Convenience = Ast_convenience

(** Wraps all array expressions with [Earray.of_array]. *)
let mapper _ =
  let open Ast_mapper in
  let super = default_mapper in
  { super with
    expr =
      (fun this e ->
        match e.pexp_desc with
          | Pexp_array xs ->
              let xs2 = List.map (this.expr this) xs in
              with_default_loc e.pexp_loc
                (fun () ->
                  Exp.apply
                    (Convenience.evar "Earray.of_array")
                    ["", { e with pexp_desc = Pexp_array xs2 }])
          | _ -> super.expr this e);
  }

let () = Ast_mapper.run_main mapper
