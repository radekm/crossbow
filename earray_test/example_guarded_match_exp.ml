(* Copyright (c) 2013 Radek Micek *)

let arr : (int, [`R]) Earray.t = Earray.of_array [| 2 |]

let _ =
  let earr_result_var_1 = ref None in
  match arr with
    | earr_pat_var_1
      when
        Earray.length earr_pat_var_1 = 1 &&
        (match Earray.get earr_pat_var_1 0 with
          | i when i >= 0 -> earr_result_var_1 := Some "nonnegative"; true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | earr_pat_var_2
      when
        Earray.length earr_pat_var_2 = 1 &&
        (match Earray.get earr_pat_var_2 0 with
          | i when true -> earr_result_var_1 := Some "negative"; true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | _ -> "other"
