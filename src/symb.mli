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

(** An identifier of a symbol in a database.
   Identifiers are not portable across databases.
*)
type 's id

(** Symbol arity. *)
type arity = int

val max_arity : int

(** Creates a new symbol database containing only the predefined symbols. *)
val create_db : unit -> wdb

(** Adds a new symbol into the database.
   The symbol is not commutative and not auxiliary.

   Raises [Invalid_argument] if the arity is out of the range.
*)
val add : 's db -> arity -> 's id

(** [iter f db] applies [f] to each symbol in the database [db]. *)
val iter : ('s id -> unit) -> 's db -> unit

(** Converts the identifier to integer. *)
val id_to_int : 's id -> int

(** {6 Properties of symbols}

   These functions raise [Not_found] if the symbol is not in the database.
*)

(** Returns the arity. *)
val arity : 's db -> 's id -> arity

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

(** The id of the negation symbol [~/1]. *)
val sym_not : 's id
