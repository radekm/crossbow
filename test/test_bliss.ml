(* Copyright (c) 2013 Radek Micek *)

open OUnit

let (|>) = BatPervasives.(|>)
let (%>) = BatPervasives.(%>)

let test_canonical_form () =
  let green = 0 in
  let blue = 1 in

  let edges = [0,1; 0,2; 0,3; 1,2; 1,3] in
  let lab =
    let g = Bliss.create_graph () in
    assert_equal 0 (Bliss.add_vertex g green);
    assert_equal 1 (Bliss.add_vertex g blue);
    assert_equal 2 (Bliss.add_vertex g blue);
    assert_equal 3 (Bliss.add_vertex g blue);
    List.iter (fun (a, b) -> Bliss.add_edge g a b) edges;
    Bliss.canonical_form g 4 in

  let edges2 = [0,1; 0,3; 1,2; 1,3; 2,3] in
  let lab2 =
    let g = Bliss.create_graph () in
    assert_equal 0 (Bliss.add_vertex g blue);
    assert_equal 1 (Bliss.add_vertex g green);
    assert_equal 2 (Bliss.add_vertex g blue);
    assert_equal 3 (Bliss.add_vertex g blue);
    List.iter (fun (a, b) -> Bliss.add_edge g a b) edges2;
    Bliss.canonical_form g 4 in

  (* Labeling is permutation. *)
  let assert_perm l =
    Array.iter (fun i -> assert_bool "range" (0 <= i && i <= 3)) l;
    assert_equal 4 (Array.to_list l |> BatList.unique |> List.length) in
  assert_perm lab;
  assert_perm lab2;

  (* Labelings are canonical. *)
  assert_bool "color" (lab.(0) = lab2.(1));
  let map_edges l =
    BatList.map
      (fun (a, b) ->
        if l.(a) <= l.(b) then
          l.(a), l.(b)
        else
          l.(b), l.(a))
    %> BatList.sort compare in
  assert_bool "edges" (map_edges lab edges = map_edges lab2 edges2)

let test_canonical_form_zero_len () =
    let g = Bliss.create_graph () in
    assert_equal 0 (Bliss.add_vertex g 0);
    assert_equal 1 (Bliss.add_vertex g 1);
    Bliss.add_edge g 1 0;
    assert_equal (Earray.to_array [| |]) (Bliss.canonical_form g 0)

let suite =
  "Bliss suite" >:::
    [
      "canonical_form" >:: test_canonical_form;
      "canonical_form - zero length" >:: test_canonical_form_zero_len;
    ]
