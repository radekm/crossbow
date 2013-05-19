(* Copyright (c) 2013 Radek Micek *)

(** Symbol databases.

   A symbol is a pair [name/arity]. A symbol database is a collection of symbols
   and their properties. Every symbol in a database has a unique [id] which
   was assigned when the symbol was added to the database.
*)

(** Symbol database. *)
type db

(** An identifier of a symbol in a database.
   Identifiers are not portable across databases.
*)
type id

(** Symbol name. *)
type name = string

(** Symbol arity. *)
type arity = int

val max_arity : int

(** Creates a new symbol database containing only the predefined symbols. *)
val create_db : unit -> db

(** Adds the symbol into the database.
   The symbol is not commutative and not auxiliary.

   Raises [Failure] if the symbol already exists in the database.
   Raises [Invalid_argument] if the name is empty or the arity is out of the range.
*)
val add_symb : db -> name -> arity -> id

(** Tests if the symbol is in the database. *)
val mem : db -> name -> arity -> bool

(** Finds the symbol by the name and the arity and returns its identifier.

   Raises [Not_found] if the symbol is not in the database.
*)
val find : db -> name -> arity -> id

(** Adds a new anonymous symbol into the database.
   Anonymous symbols are ignored by {!mem} and {!find}.
   The symbol is not commutative and not auxiliary.

   Raises [Invalid_argument] if the arity is out of the range.
*)
val add_anon_symb : db -> arity -> id

(** [iter f db] applies [f] to each symbol in the database [db]. *)
val iter : (id -> unit) -> db -> unit

(** Converts the identifier to integer. *)
val id_to_int : id -> int

(** {6 Properties of symbols}

   These functions raise [Not_found] if the symbol is not in the database.
*)

(** Returns the name. Empty string for anonymous symbols. *)
val name : db -> id -> name

(** Returns the arity. *)
val arity : db -> id -> arity

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

(** Returns whether the symbol is anonymous. *)
val anon : db -> id -> bool

(** {6 Predefined symbols}

   Identifiers of these symbols are same across all databases.
*)

(** The id of the equality symbol [=/2]. *)
val sym_eq : id

(** The id of the negation symbol [~/1]. *)
val sym_not : id
