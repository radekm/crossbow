(* Copyright (c) 2013 Radek Micek *)

open OUnit

let assert_vertex_cover edges cover =
  (* No duplicate vertices. *)
  assert_equal
    (List.length cover)
    (List.length (BatList.sort_unique compare cover));
  (* All edges are covered. *)
  let edge_is_covered (u, v) =
    assert_bool "" (Elist.contains u cover || Elist.contains v cover) in
  Earray.iter edge_is_covered edges

(* Minimum vertex cover is 2, 6.

    3
   /
  2--4
 /|\
1 | 5
 \|
  6
*)
let mk_graph1 () = [| 1,2; 2,3; 2,4; 2,5; 2,6; 6,1 |]

(* Minimum vertex cover is 2, 3, 6.

  2---3---4
 /|   |
1 |   |
 \|   |
  6---5
*)

let mk_graph2 () = [| 1,2; 2,3; 3,4; 3,5; 5,6; 6,2; 6,1 |]

let test_min_vertex_cover1 () =
  let edges = mk_graph1 () in
  let cover = Algo.min_vertex_cover edges in
  assert_vertex_cover edges cover;
  assert_equal 2 (List.length cover)

let test_min_vertex_cover2 () =
  let edges = mk_graph2 () in
  let cover = Algo.min_vertex_cover edges in
  assert_vertex_cover edges cover;
  assert_equal 3 (List.length cover)

let test_vertex_cover_empty_graph () =
  assert_equal
    (Some [])
    (Algo.vertex_cover [| |] 4)

let suite =
  "Algo suite" >:::
    [
      "min_vertex_cover 1" >:: test_min_vertex_cover1;
      "min_vertex_cover 2" >:: test_min_vertex_cover2;
      "vertex_cover - empty graph" >:: test_vertex_cover_empty_graph;
    ]
