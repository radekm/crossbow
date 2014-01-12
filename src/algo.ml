(* Copyright (c) 2013 Radek Micek *)

module Array = Earray.Array

let vertex_cover edges k =
  if k < 0 then invalid_arg "k";

  let vertex_to_int = Hashtbl.create 20 in
  let int_to_vertex = BatDynArray.make 20 in
  let vert_to_int v =
    if Hashtbl.mem vertex_to_int v then
      Hashtbl.find vertex_to_int v
    else begin
      let id = BatDynArray.length int_to_vertex in
      Hashtbl.add vertex_to_int v id;
      BatDynArray.add int_to_vertex v;
      id
    end in

  (* Map vertices to integers 0, .., n-1 to speed up the
     detection of an uncovered edge.
  *)
  let edges =
    Earray.map (fun (u, v) -> (vert_to_int u, vert_to_int v)) edges in
  (* Which vertices are covered. *)
  let cov = Earray.make (BatDynArray.length int_to_vertex) false in

  let rec vertex_cover k =
    (* Find an uncovered edge. *)
    let uncov_edge =
      Earray.Exceptionless.find
        (fun (u, v) -> not cov.(u) && not cov.(v)) edges in
    match uncov_edge with
      | None -> true
      | _ when k <= 0 -> false
      | Some (u, v) ->
          (cov.(u) <- true; vertex_cover (k-1)) ||
          (cov.(u) <- false; cov.(v) <- true; vertex_cover (k-1)) ||
          (cov.(v) <- false; false) in

  if vertex_cover k then begin
    let cover = ref [] in
    Earray.iteri (fun i c ->
      if c then cover := BatDynArray.get int_to_vertex i :: !cover) cov;
    Some !cover
  end else
    None

let min_vertex_cover edges =
  let rec loop k =
    match vertex_cover edges k with
      | None -> loop (k+1)
      | Some cover -> cover in
  loop 0
