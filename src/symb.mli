(* Copyright (c) 2013 Radek Micek *)

(** Symbol databases.

   A symbol database is a collection of symbols
   and their properties. Every symbol in a database has a unique [id] which
   was assigned when the symbol was added to the database.
*)

(** Symbol database. *)
type db

(** An identifier of a symbol in a database. *)
type id

(** Symbol arity. *)
type arity = int

val max_arity : int

type kind =
  | Func
  | Pred

(** Note: Solver may ignore these without affecting correctness. *)
type hint =
  | Permutation
  (** For unary functions. *)
  | Latin_square
  (** For binary functions. *)

(** Creates a new symbol database containing only the predefined symbols. *)
val create_db : unit -> db

(** Adds a new function symbol into the database.
   The symbol is not commutative and not auxiliary.

   Raises [Invalid_argument] if the arity is out of the range.
*)
val add_func : db -> arity -> id

(** Adds a new predicate symbol into the database.
   The symbol is not commutative and not auxiliary.

   Raises [Invalid_argument] if the arity is out of the range.
*)
val add_pred : db -> arity -> id

(** [iter f db] applies [f] to each symbol in the database [db]. *)
val iter : (id -> unit) -> db -> unit

(** Converts the identifier to integer. *)
val id_to_int : id -> int

(** {6 Properties of symbols}

   These functions raise [Not_found] if the symbol is not in the database.
*)

(** Returns the arity. *)
val arity : id -> arity

val kind : id -> kind

(** Returns whether the symbol is commutative. *)
val commutative : db -> id -> bool

(** Sets commutativity of the binary symbol.
   Must not be used for predefined symbols.

   Raises [Failure] if the symbol is predefined or non-binary.
*)
val set_commutative : db -> id -> bool -> unit

(** Returns whether the symbol is auxiliary. *)
val auxiliary : db -> id -> bool

(** Sets whether the symbol is auxiliary.

   Raises [Failure] if the symbol is predefined.
*)
val set_auxiliary : db -> id -> bool -> unit

(** Returns the hints of the given symbol. *)
val hints : db -> id -> hint list

(** Records hint for the given symbol.

   Raises [Failure] if the symbol is predefined
   or if the hint is not compatible with the kind
   or the arity of the symbol.
*)
val add_hint : db -> id -> hint -> unit

(** {6 Predefined symbols}

   Identifiers of these symbols are same across all databases.
*)

(** The id of the equality symbol [=/2]. *)
val sym_eq : id

module Map : sig
  type 'a t

  val empty : 'a t
  val add : id -> 'a -> 'a t -> 'a t
  val find : id -> 'a t -> 'a
  val mem : id -> 'a t -> bool
  val iter : (id -> 'a -> unit) -> 'a t -> unit
  val mapi : (id -> 'a -> 'b) -> 'a t -> 'b t
  val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
  val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
  val enum : 'a t -> (id * 'a) BatEnum.t
  val of_enum : (id * 'a) BatEnum.t -> 'a t
end
