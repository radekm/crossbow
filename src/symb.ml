(* Copyright (c) 2013 Radek Micek *)

type id = int

type name = string

type arity = int

type symbol = {
  s_id : id;
  s_name : name;
  s_arity : arity;
  s_commutative : bool;
  s_auxiliary : bool;
}

type db = {
  by_id : symbol BatDynArray.t;
  by_name : (string * int, id) Hashtbl.t;
}

let max_arity = 255

let create_db () =
  let by_id = BatDynArray.make 20 in
  let by_name = Hashtbl.create 20 in

  BatDynArray.add by_id {
    s_id = 0;
    s_name = "=";
    s_arity = 2;
    s_commutative = true;
    s_auxiliary = false;
  };
  Hashtbl.add by_name ("=", 2) 0;

  BatDynArray.add by_id {
    s_id = 1;
    s_name = "~";
    s_arity = 1;
    s_commutative = false;
    s_auxiliary = false;
  };
  Hashtbl.add by_name ("~", 1) 1;

  { by_id; by_name }

let add_symb db name arity =
  if String.length name <= 0 then invalid_arg "name";
  if arity < 0 || arity > max_arity then invalid_arg "arity";
  if Hashtbl.mem db.by_name (name, arity) then failwith "duplicate symbol";
  let id = BatDynArray.length db.by_id in
  BatDynArray.add db.by_id {
    s_id = id;
    s_name = name;
    s_arity = arity;
    s_commutative = false;
    s_auxiliary = false;
  };
  Hashtbl.add db.by_name (name, arity) id;
  id

let mem db name arity = Hashtbl.mem db.by_name (name, arity)

let find db name arity = Hashtbl.find db.by_name (name, arity)

let add_anon_symb db arity =
  if arity < 0 || arity > max_arity then invalid_arg "arity";
  let id = BatDynArray.length db.by_id in
  BatDynArray.add db.by_id {
    s_id = id;
    s_name = "";
    s_arity = arity;
    s_commutative = false;
    s_auxiliary = false;
  };
  id

let iter f db =
  for i = 0 to BatDynArray.length db.by_id - 1 do
    f i
  done

let id_to_int sym = sym

let sym_eq = 0

let sym_not = 1

(* ************************************************************************* *)
(* Properties of symbols *)

let get db sym =
  if BatDynArray.length db.by_id <= sym then raise Not_found;
  BatDynArray.get db.by_id sym

let name db sym = (get db sym).s_name

let arity db sym = (get db sym).s_arity

let commutative db sym = (get db sym).s_commutative

let set_commutative db sym comm =
  if sym = sym_eq || sym = sym_not then
    failwith "predefined symbol";
  let symb = get db sym in
  if symb.s_arity <> 2 then
    failwith "non-binary symbol";
  BatDynArray.set db.by_id sym { symb with s_commutative = comm }

let auxiliary db sym = (get db sym).s_auxiliary

let set_auxiliary db sym aux =
  if sym = sym_eq || sym = sym_not then
    failwith "predefined symbol";
  let symb = get db sym in
  BatDynArray.set db.by_id sym { symb with s_auxiliary = aux }

let anon db sym = name db sym = ""
