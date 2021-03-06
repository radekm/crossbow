(* Copyright (c) 2013 Radek Micek *)

module T = Term
module L = Lit
module C = Clause

let detect_commutativity symdb clauses =
  BatDynArray.filter_map
    (fun cl ->
      match C.unflatten symdb cl with
        | None -> None
        | Some cl2 ->
            let cl2, _ = C.normalize_vars cl2 in
            match%earr cl2 with
              | [ L.Lit (Sh.Pos, eq,
                         [|
                           T.Func (f, [| T.Var 0; T.Var 1 |]);
                           T.Func (f', [| T.Var 1; T.Var 0 |]);
                         |])
                ] when eq = Symb.sym_eq && f = f' ->
                  Symb.set_commutative symdb f true;
                  None
              | _ -> Some cl)
    clauses

let iter_axioms f symdb clauses =
  BatDynArray.iter
    (fun cl ->
      match C.unflatten symdb cl with
        | None -> ()
        | Some cl ->
            (* Put symbol with higher arity to the left. *)
            let cl =
              match%earr cl with
                | [ L.Lit (Sh.Pos, eq,
                           [|
                             T.Func (s, _) as l;
                             T.Func (s', _) as r;
                           |])
                  ] when eq = Symb.sym_eq && Symb.arity s < Symb.arity s' ->
                    [ L.lit (Sh.Pos, eq, Earray.of_array [| r; l |]) ]
                | _ -> cl in
            let cl, _ = C.normalize_vars cl in
            match%earr cl with
              | [ L.Lit (Sh.Pos, eq, [| l; r |]) ] when eq = Symb.sym_eq ->
                  f l r
              | _ -> ())
    clauses

let detect_hints_for_groups symdb clauses =
  let comm f = Symb.commutative symdb f in

  (* Axioms. *)
  let g0_0 = ref BatSet.empty in
  let x_ggx = ref BatSet.empty in
  let x_f0x = ref BatSet.empty in
  let x_fx0 = ref BatSet.empty in
  let fxgx_0 = ref BatSet.empty in
  let fgxx_0 = ref BatSet.empty in
  let fxfyz_ffxyz = ref BatSet.empty in

  let record_axiom l r =
    match%earr l, r with
      (* g(0) = 0 *)
      | T.Func (g, [| T.Func (e, [| |]) |]), T.Func (e', [| |])
        when e = e' ->
          g0_0 := BatSet.add (g, e) !g0_0
      (* x = g(g(x)) *)
      | T.Var 0, T.Func (g, [| T.Func (g', [| T.Var 0 |]) |])
        when g = g' ->
          x_ggx := BatSet.add g !x_ggx
      (* x = f(0, x) *)
      | T.Var 0, T.Func (f, [| T.Func (e, [| |]); T.Var 0 |]) ->
          x_f0x := BatSet.add (f, e) !x_f0x
      (* x = f(x, 0) *)
      | T.Var 0, T.Func (f, [| T.Var 0; T.Func (e, [| |]) |]) ->
          x_fx0 := BatSet.add (f, e) !x_fx0;
          if comm f then
            (* x = f(0, x) holds for commutative symbol. *)
            x_f0x := BatSet.add (f, e) !x_f0x
      (* f(x, g(x)) = 0 *)
      | T.Func (f, [| T.Var 0; T.Func (g, [| T.Var 0 |]) |]),
        T.Func (e, [| |]) ->
          fxgx_0 := BatSet.add (f, g, e) !fxgx_0;
          if comm f then
            (* f(g(x), x) = 0 holds for commutative symbol. *)
            fgxx_0 := BatSet.add (f, g, e) !fgxx_0
      (* f(g(x), x) = 0 *)
      | T.Func (f, [| T.Func (g, [| T.Var 0 |]); T.Var 0 |]),
        T.Func (e, [| |]) ->
          fgxx_0 := BatSet.add (f, g, e) !fgxx_0
      (* f(x, f(y, z)) = f(f(x, y), z) *)
      | T.Func (f1, [| T.Var 0; T.Func (f2, [| T.Var 1; T.Var 2 |]) |]),
        T.Func (f3, [| T.Func (f4, [| T.Var 0; T.Var 1 |]); T.Var 2 |])
        when List.for_all ((=) f1) [f2; f3; f4] ->
          fxfyz_ffxyz := BatSet.add f1 !fxfyz_ffxyz
      (* f(x, f(y, z)) = f(z, f(x, y))
         associativity for commutative symbol.
      *)
      | T.Func (f1, [| T.Var 0; T.Func (f2, [| T.Var 1; T.Var 2 |]) |]),
        T.Func (f3, [| T.Var 2; T.Func (f4, [| T.Var 0; T.Var 1 |]) |])
        when List.for_all ((=) f1) [f2; f3; f4] && comm f1 ->
          fxfyz_ffxyz := BatSet.add f1 !fxfyz_ffxyz
      | _ -> () in

  iter_axioms record_axiom symdb clauses;

  BatSet.iter
    (fun (f, g, e) ->
      (* Triple (f, g, e) satisfies group axioms. *)
      if
        BatSet.mem (g, e) !g0_0 &&
        BatSet.mem g !x_ggx &&
        BatSet.mem (f, e) !x_f0x &&
        BatSet.mem (f, e) !x_fx0 &&
        (* Skip fxgx_0. *)
        BatSet.mem (f, g, e) !fgxx_0 &&
        BatSet.mem f !fxfyz_ffxyz
      then begin
        Symb.add_hint symdb f Symb.Latin_square;
        Symb.add_hint symdb g Symb.Permutation
      end)
    !fxgx_0

(* Note: commutativity is not supported. *)
let detect_hints_for_quasigroups symdb clauses =
  let x_mylyx = ref BatSet.empty in
  let x_lymyx = ref BatSet.empty in
  let x_mrxyy = ref BatSet.empty in
  let x_rmxyy = ref BatSet.empty in

  let record_axiom l r =
    match l with
      | T.Var 0 ->
          begin match%earr r with
            (* x = mult(y, ld(y, x)) or
               x = ld(y, mult(y, x))
            *)
            | T.Func (s,
                      [|
                        T.Var 1;
                        T.Func (s', [| T.Var 1; T.Var 0 |]);
                      |]) ->
                x_mylyx := BatSet.add (s, s') !x_mylyx;
                x_lymyx := BatSet.add (s', s) !x_lymyx
            (* x = mult(rd(x, y), y) or
               x = rd(mult(x, y), y)
            *)
            | T.Func (s,
                      [|
                        T.Func (s', [| T.Var 0; T.Var 1 |]);
                        T.Var 1;
                      |]) ->
                x_mrxyy := BatSet.add (s, s') !x_mrxyy;
                x_rmxyy := BatSet.add (s', s) !x_rmxyy
            | _ -> ()
          end
      | _ -> () in

  iter_axioms record_axiom symdb clauses;

  BatSet.iter
    (fun (mult, ld) ->
      if BatSet.mem (mult, ld) !x_lymyx then
        BatSet.iter
          (fun (mult', rd) ->
            if
              mult = mult' &&
              BatSet.mem (mult, rd) !x_rmxyy
            then begin
              Symb.add_hint symdb mult Symb.Latin_square
            end)
          !x_mrxyy)
    !x_mylyx

let detect_hints_for_involutive_funcs symdb clauses =
  let proc_axiom l r =
    match%earr l, r with
      (* x = f(f(x)) *)
      | T.Var 0, T.Func (f, [| T.Func (f', [| T.Var 0 |]) |])
        when f = f' ->
          Symb.add_hint symdb f Symb.Permutation
      | _ -> () in

  iter_axioms proc_axiom symdb clauses
