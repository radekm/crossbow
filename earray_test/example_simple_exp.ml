(* Copyright (c) 2013 Radek Micek *)

let a : (int, [`R]) Earray.t = Earray.of_array [| 1; 2; 3 |]

let _ =
  let earr_result_var_1 = ref None in
  match a with
    | earr_pat_var_1
      when
        Earray.length earr_pat_var_1 = 0 &&
        (match () with
          | () when true -> earr_result_var_1 := Some "empty"; true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)

    | earr_pat_var_2
      when
        Earray.length earr_pat_var_2 = 3 &&
        (match
          (Earray.get earr_pat_var_2 0,
           Earray.get earr_pat_var_2 1,
           Earray.get earr_pat_var_2 2)
         with
           | (_, x, 3) when true -> earr_result_var_1 := Some "matches"; true
           | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)

    | _ -> "fallback"
