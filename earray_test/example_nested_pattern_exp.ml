(* Copyright (c) 2013 Radek Micek *)

let arr : ((int, [`R]) Earray.t, [`R]) Earray.t option = None

let _ =
  let earr_result_var_1 = ref None in
  match arr with
    | None -> 1
    | Some earr_pat_var_1
      when
        Earray.length earr_pat_var_1 = 3 &&
        (match
          (Earray.get earr_pat_var_1 0,
           Earray.get earr_pat_var_1 1,
           Earray.get earr_pat_var_1 2)
         with
           | (earr_pat_var_2, earr_pat_var_3, ys)
             when
               (Earray.length earr_pat_var_2 = 1 &&
                Earray.length earr_pat_var_3 = 2) &&
               (match
                 (Earray.get earr_pat_var_2 0,
                  Earray.get earr_pat_var_3 0,
                  Earray.get earr_pat_var_3 1)
                with
                  | (_, x, 17) when true -> earr_result_var_1 := Some 2; true
                  | _ -> false)
             -> true
           | _ -> false)
      ->
        (match !earr_result_var_1 with
          | None -> failwith "earr syntax: no result"
          | Some res -> res)
    | Some _ -> 3
