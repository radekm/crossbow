(* Copyright (c) 2015 Radek Micek *)

module Utils = struct

  (** [comb n k] computes a binomial coefficient

     / n \         n!
     |   |  =  -----------
     \ k /     k! * (n-k)!

   *)
  let comb n k =
    let x = ref Z.one in
    for i = max k (n - k) + 1 to n do
      x := Z.(!x * Z.of_int i)
    done;
    for i = min k (n - k) downto 2 do
      x := Z.(!x / Z.of_int i)
    done;
    !x

  (** [binom n x] computes a probability [P(X >= x)] for binomial distribution
     with parameters [n] and [0.5]. The computed probability is divided
     by [0.5^n].
  *)
  let binom n x =
    BatList.range x `To n
    |> BatList.map (fun x -> comb n x)
    |> BatList.fold_left Z.(+) Z.zero

end

open Utils

type hypothesis =
  | Lower
  | Not_equal
  | Greater

let test ~nneg ~nzero ~npos ~ha =
  let f =
    match ha with
      (* Alternative hypothesis has wrong direction. *)
      | Lower when nneg < npos -> min
      (* Alternative hypothesis has wrong direction. *)
      | Greater when nneg > npos -> min
      (* Alternative hypothesis has correct direction. *)
      | _ -> max in
  let n = nneg + nzero + npos in
  let x = binom n (f nneg npos) in
  let pval =
    if nzero = 0 then
      (* No ties - use standard sign test:

         2 * binom n (f nneg nplus) * 0.5^n
      *)
      Q.div_2exp (Q.of_bigint x) (n - 1)
    else
      (* Use Fong's sign test. *)
      let numerator = x in
      let denominator = binom n ((n - nzero + 1) / 2) in
      Q.make numerator denominator in
  let pval =
    if ha <> Not_equal then
      (* One-tailed test - divide p-value by 2. *)
      Q.div_2exp pval 1
    else
      pval in
  (* Convert p-value to float. *)
  (pval |> Q.num |> Z.to_float) /. (pval |> Q.den |> Z.to_float)
