(* Copyright (c) 2013 Radek Micek *)

let arr = [| Some 4 |]
let earr : (int option, [`R]) Earray.t = Earray.of_array [| Some 5 |]

let _ =
  let earr_result_var_1 = ref None in
  match earr with
    | earr_pat_var_2
      when
        Earray.length earr_pat_var_2 = 1 &&
        (match Earray.get earr_pat_var_2 0 with
          | Some x when true ->
              earr_result_var_1 := Some
                (match arr with
                  | [| Some y |] ->
                      let earr_result_var_2 = ref None in
                      (match earr with
                        | earr_pat_var_1
                          when
                            Earray.length earr_pat_var_1 = 1 &&
                            (match Earray.get earr_pat_var_1 0 with
                              | Some 5 when true ->
                                  earr_result_var_2 := Some 1;
                                  true
                              | _ -> false)
                          ->
                            (match !earr_result_var_2 with
                              | None -> failwith "earr syntax: no result"
                              | Some res -> res)
                        | _ -> 2)
                  | _ -> 3);
              true
          | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | _ -> 4
