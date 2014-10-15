(* Copyright (c) 2013-14 Radek Micek *)

type id = int

type arity = int

type kind =
  | Func
  | Pred

type hint =
  | Permutation
  | Latin_square

type symbol = {
  s_id : id;
  s_commutative : bool;
  s_auxiliary : bool;
  s_hints : hint list;
  s_distinct_constant: bool;
}

type 'a db = {
  by_id : symbol BatDynArray.t;
}
constraint 'a = [< `R|`W]

let max_arity = 255

module Id : sig
  val make : int -> arity -> kind -> id
  val to_idx : id -> int
  val to_arity : id -> arity
  val to_kind : id -> kind
end = struct
  let bits_arity = 8

  let bits_kind = 1

  let bits_total = bits_arity + bits_kind

  let make n arity kind =
    let r =
      match kind with
        | Func -> 0
        | Pred -> 1 in
    (n lsl bits_total) lor (arity lsl bits_kind) lor r

  let to_idx id = id lsr bits_total

  let to_arity id = (id lsr bits_kind) land max_arity

  let to_kind id =
    match id land 1 with
      | 0 -> Func
      | _ -> Pred
end

let sym_eq = Id.make 0 2 Pred

let create_db () =
  let by_id = BatDynArray.make 20 in

  BatDynArray.add by_id {
    s_id = sym_eq;
    s_commutative = true;
    s_auxiliary = false;
    s_hints = [];
    s_distinct_constant = false;
  };

  { by_id }

external read_only : [> `R] db -> [`R] db = "%identity"

external write_only : [> `W] db -> [`W] db = "%identity"

let add_func db arity =
  if arity < 0 || arity > max_arity then invalid_arg "arity";
  let id = Id.make (BatDynArray.length db.by_id) arity Func in
  BatDynArray.add db.by_id {
    s_id = id;
    s_commutative = false;
    s_auxiliary = false;
    s_hints = [];
    s_distinct_constant = false;
  };
  id

let add_pred db arity =
  if arity < 0 || arity > max_arity then invalid_arg "arity";
  let id = Id.make (BatDynArray.length db.by_id) arity Pred in
  BatDynArray.add db.by_id {
    s_id = id;
    s_commutative = false;
    s_auxiliary = false;
    s_hints = [];
    s_distinct_constant = false;
  };
  id

let iter f db = BatDynArray.iter (fun s -> f s.s_id) db.by_id

let id_to_int sym = Id.to_idx sym

(* ************************************************************************* *)
(* Properties of symbols *)

let get db sym =
  let n = Id.to_idx sym in
  if BatDynArray.length db.by_id <= n then raise Not_found;
  BatDynArray.get db.by_id n

let arity = Id.to_arity

let kind = Id.to_kind

let commutative db sym = (get db sym).s_commutative

let set_commutative db sym comm =
  if sym = sym_eq then
    failwith "predefined symbol";
  let symb = get db sym in
  if Id.to_arity sym <> 2 then
    failwith "non-binary symbol";
  BatDynArray.set db.by_id (Id.to_idx sym) { symb with s_commutative = comm }

let auxiliary db sym = (get db sym).s_auxiliary

let set_auxiliary db sym aux =
  if sym = sym_eq then
    failwith "predefined symbol";
  let symb = get db sym in
  BatDynArray.set db.by_id (Id.to_idx sym) { symb with s_auxiliary = aux }

let distinct_constant db sym = (get db sym).s_distinct_constant

let set_distinct_constant db sym distinct_constant =
  if sym = sym_eq then
    failwith "predefined symbol";
  if kind sym <> Func || arity sym <> 0 then
    failwith "not constant";
  let symb = get db sym in
  BatDynArray.set db.by_id (Id.to_idx sym)
    { symb with s_distinct_constant = distinct_constant }

let hints db sym = (get db sym).s_hints

let add_hint db sym hint =
  if sym = sym_eq then
    failwith "predefined symbol";
  if kind sym <> Func then
    failwith "kind";
  begin match hint with
    | Permutation ->
        if arity sym <> 1 then failwith "permutation"
    | Latin_square ->
        if arity sym <> 2 then failwith "latin square"
  end;
  let symb = get db sym in
  let s_hints = BatList.sort_unique compare (hint :: symb.s_hints) in
  BatDynArray.set db.by_id (Id.to_idx sym) { symb with s_hints }

module IntMap = Sh.IntMap

module Map = struct
  type 'a t = 'a IntMap.t

  let empty = IntMap.empty
  let add = IntMap.add
  let find = IntMap.find
  let mem = IntMap.mem
  let iter = IntMap.iter
  let mapi = IntMap.mapi
  let compare = IntMap.compare
  let equal = IntMap.equal
  let enum = IntMap.enum
  let of_enum = IntMap.of_enum
end

module IntSet = Sh.IntSet

module Set = struct
  type t = IntSet.t

  let empty = IntSet.empty
  let is_empty = IntSet.is_empty
  let cardinal = IntSet.cardinal
  let add = IntSet.add
  let compare = IntSet.compare
  let equal = IntSet.equal
  let enum = IntSet.enum
  let of_enum = IntSet.of_enum
end

let distinct_consts db =
  let xs = ref Set.empty in
  iter
    (fun s ->
      if distinct_constant db s then xs := Set.add s !xs)
    db;
  !xs
