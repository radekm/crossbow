(* Copyright (c) 2013 Radek Micek *)

module R = Report

let milisecs_to_secs ms =
  if ms mod 1000 = 0
  then ms / 1000
  else ms / 1000 + 1

(* Assumes that model was not found when the exit code <> 0. *)
let model_found res =
  res.R.exit_status = R.Exit_code 0 &&
  res.R.model_size <> None
