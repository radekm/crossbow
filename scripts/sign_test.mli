(* Copyright (c) 2015 Radek Micek *)

module Utils : sig

  val comb : int -> int -> Z.t

  val binom : int -> int -> Z.t

end

(** Alternative hypothesis. *)
type hypothesis =
  | Lower
  | Not_equal
  | Greater

(** [test ~nneg ~nzero ~npos ~ha] performs a sign test with the alternative
   hypothesis [ha]. If [nzero] is zero then the standard sign test is
   performed. Otherwise the Fong's modified sign test is performed.

   Details about the Fong's test can be found in:
   - Fong, 2003, Use of the Sign Test for the Median in the Presence of Ties
   - Helsel, 2012, Statistics for Censored Environmental Data
     Using Minitab and R
*)
val test : nneg:int -> nzero:int -> npos:int -> ha:hypothesis -> float
