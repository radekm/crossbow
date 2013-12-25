(* Copyright (c) 2013 Radek Micek *)

let a : (int, [`R]) Earray.t = Earray.of_array [| 8 |]
let b : (int, [`R]) Earray.t option = Some (Earray.of_array [| 9 |])

let _ =
  let earr_result_var_1 = ref None in
  match a with
    | earr_pat_var_2
      when
        Earray.length earr_pat_var_2 = 1 &&
        (match Earray.get earr_pat_var_2 0 with
          | i
            when
              let earr_result_var_2 = ref None in
              (match b with
                | Some earr_pat_var_1
                  when
                    Earray.length earr_pat_var_1 = 1 &&
                    (match Earray.get earr_pat_var_1 0 with
                      | j when true -> earr_result_var_2 := Some (i < j); true
                      | _ -> false)
                  ->
                    (match !earr_result_var_2 with
                      | None -> failwith "earr syntax: no result"
                      | Some res -> res)
                | _ -> false)
            ->
              earr_result_var_1 := Some 1; true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | _ -> 2
