(* Copyright (c) 2013 Radek Micek *)

type 's id = int

type arity = int

type role =
  | Func
  | Pred

type 's symbol = {
  s_id : 's id;
  s_arity : arity;
  s_commutative : bool;
  s_auxiliary : bool;
  s_role : role;
}

type 's db = {
  by_id : 's symbol BatDynArray.t;
}

type wdb =
  | Wr : 's db -> wdb

let max_arity = 255

let create_db () =
  let by_id = BatDynArray.make 20 in

  BatDynArray.add by_id {
    s_id = 0;
    s_arity = 2;
    s_commutative = true;
    s_auxiliary = false;
    s_role = Pred;
  };

  (* FIXME: Dummy symbol - to preserve order in hash tables. *)
  BatDynArray.add by_id {
    s_id = 1;
    s_arity = 1;
    s_commutative = false;
    s_auxiliary = false;
    s_role = Pred;
  };

  Wr { by_id }

let add_func db arity =
  if arity < 0 || arity > max_arity then invalid_arg "arity";
  let id = BatDynArray.length db.by_id in
  BatDynArray.add db.by_id {
    s_id = id;
    s_arity = arity;
    s_commutative = false;
    s_auxiliary = false;
    s_role = Func;
  };
  id

let add_pred db arity =
  if arity < 0 || arity > max_arity then invalid_arg "arity";
  let id = BatDynArray.length db.by_id in
  BatDynArray.add db.by_id {
    s_id = id;
    s_arity = arity;
    s_commutative = false;
    s_auxiliary = false;
    s_role = Pred;
  };
  id

let iter f db =
  f 0;
  (* FIXME: Skip dummy symbol. *)
  for i = 2 to BatDynArray.length db.by_id - 1 do
    f i
  done

let id_to_int sym = sym

let sym_eq = 0

(* ************************************************************************* *)
(* Properties of symbols *)

let get db sym =
  if BatDynArray.length db.by_id <= sym then raise Not_found;
  BatDynArray.get db.by_id sym

let arity db sym = (get db sym).s_arity

let commutative db sym = (get db sym).s_commutative

let set_commutative db sym comm =
  if sym = sym_eq then
    failwith "predefined symbol";
  let symb = get db sym in
  if symb.s_arity <> 2 then
    failwith "non-binary symbol";
  BatDynArray.set db.by_id sym { symb with s_commutative = comm }

let auxiliary db sym = (get db sym).s_auxiliary

let set_auxiliary db sym aux =
  if sym = sym_eq then
    failwith "predefined symbol";
  let symb = get db sym in
  BatDynArray.set db.by_id sym { symb with s_auxiliary = aux }
