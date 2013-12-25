(* Copyright (c) 2013 Radek Micek *)

let arr : (int, [`R]) Earray.t = Earray.of_array [| |]

let _ =
  let earr_result_var_1 = ref None in
  match arr with
    | earr_pat_var_1
      when
        Earray.length earr_pat_var_1 = 1 &&
        (match Earray.get earr_pat_var_1 0 with
          | 1 when true -> earr_result_var_1 := Some 1; true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | earr_pat_var_2
      when
        Earray.length earr_pat_var_2 = 1 &&
        (match Earray.get earr_pat_var_2 0 with
          | 2 when true -> earr_result_var_1 := Some 2; true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | earr_pat_var_3
      when
        Earray.length earr_pat_var_3 = 2 &&
        (match
          (Earray.get earr_pat_var_3 0,
           Earray.get earr_pat_var_3 1)
         with
           | (1, 1) when true -> earr_result_var_1 := Some 2; true
           | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | earr_pat_var_4
      when
        Earray.length earr_pat_var_4 = 1 &&
        (match Earray.get earr_pat_var_4 0 with
          | 3 when true -> earr_result_var_1 := Some 3; true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | earr_pat_var_5
      when
        Earray.length earr_pat_var_5 = 2 &&
        (match
          (Earray.get earr_pat_var_5 0,
           Earray.get earr_pat_var_5 1)
         with
           | (1, 2) when true -> earr_result_var_1 := Some 3; true
           | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | earr_pat_var_6
      when
        Earray.length earr_pat_var_6 = 3 &&
        (match
          (Earray.get earr_pat_var_6 0,
           Earray.get earr_pat_var_6 1,
           Earray.get earr_pat_var_6 2)
         with
           | (1, 1, 1) when true -> earr_result_var_1 := Some 3; true
           | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | _ -> ~-1
