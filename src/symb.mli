(* Copyright (c) 2013 Radek Micek *)

(** Symbol databases.

   A symbol database is a collection of symbols
   and their properties. Every symbol in a database has a unique [id] which
   was assigned when the symbol was added to the database.
*)

(** Symbol database. *)
type 's db

type wdb =
  | Wr : 's db -> wdb

(** An identifier of a symbol in a database. *)
type 's id

(** Symbol arity. *)
type arity = int

val max_arity : int

type kind =
  | Func
  | Pred

(** Creates a new symbol database containing only the predefined symbols. *)
val create_db : unit -> wdb

(** Adds a new function symbol into the database.
   The symbol is not commutative and not auxiliary.

   Raises [Invalid_argument] if the arity is out of the range.
*)
val add_func : 's db -> arity -> 's id

(** Adds a new predicate symbol into the database.
   The symbol is not commutative and not auxiliary.

   Raises [Invalid_argument] if the arity is out of the range.
*)
val add_pred : 's db -> arity -> 's id

(** [iter f db] applies [f] to each symbol in the database [db]. *)
val iter : ('s id -> unit) -> 's db -> unit

(** Converts the identifier to integer. *)
val id_to_int : 's id -> int

(** {6 Properties of symbols}

   These functions raise [Not_found] if the symbol is not in the database.
*)

(** Returns the arity. *)
val arity : 's id -> arity

val kind : 's id -> kind

(** Returns whether the symbol is commutative. *)
val commutative : 's db -> 's id -> bool

(** Sets commutativity of the binary symbol.
   Must not be used for predefined symbols.

   Raises [Failure] if the symbol is predefined or non-binary.
*)
val set_commutative : 's db -> 's id -> bool -> unit

(** Returns whether the symbol is auxiliary. *)
val auxiliary : 's db -> 's id -> bool

(** Sets whether the symbol is auxiliary.

   Raises [Failure] if the symbol is predefined.
*)
val set_auxiliary : 's db -> 's id -> bool -> unit

(** {6 Predefined symbols}

   Identifiers of these symbols are same across all databases.
*)

(** The id of the equality symbol [=/2]. *)
val sym_eq : 's id

module Map : sig
  type ('s, 'a) t

  val empty : ('s, 'a) t
  val add : 's id -> 'a -> ('s, 'a) t -> ('s, 'a) t
  val find : 's id -> ('s, 'a) t -> 'a
  val mem : 's id -> ('s, 'a) t -> bool
  val iter : ('s id -> 'a -> unit) -> ('s, 'a) t -> unit
  val mapi : ('s id -> 'a -> 'b) -> ('s, 'a) t -> ('s, 'b) t
  val compare : ('a -> 'a -> int) -> ('s, 'a) t -> ('s, 'a) t -> int
  val equal : ('a -> 'a -> bool) -> ('s, 'a) t -> ('s, 'a) t -> bool
  val enum : ('s, 'a) t -> ('s id * 'a) BatEnum.t
  val of_enum : ('s id * 'a) BatEnum.t -> ('s, 'a) t
end
