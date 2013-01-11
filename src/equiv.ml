(* Copyright (c) 2013 Radek Micek *)

type block_id = int

type node = {
  id : block_id;
  mutable size : int;
  mutable parent : node option;
}

type t = {
  items : node BatDynArray.t;
}

type item = int

let create () = { items = BatDynArray.create () }

let add_item e =
  let id = BatDynArray.length e.items in
  BatDynArray.add e.items { id; size = 1; parent = None };
  id

let find_root e i =
  let rec find_root = function
    | { parent = None } as n -> n
    | { parent = Some p } -> find_root p in
  let rec compress_path root = function
    | { parent = None } -> ()
    | { parent = Some p } as n ->
        n.parent <- Some root;
        compress_path root p in
  let node = BatDynArray.get e.items i in
  let root = find_root node in
  compress_path root node;
  root

let union e i1 i2 =
  if i1 >= BatDynArray.length e.items then invalid_arg "i1";
  if i2 >= BatDynArray.length e.items then invalid_arg "i2";

  let n1 = find_root e i1 in
  let n2 = find_root e i2 in

  if n1.id <> n2.id then
    if n1.size <= n2.size then begin
      n2.size <- n1.size + n2.size;
      n1.parent <- Some n2
    end else begin
      n1.size <- n1.size + n2.size;
      n2.parent <- Some n1
    end

let find e i =
  if i >= BatDynArray.length e.items then invalid_arg "i";
  (find_root e i).id
