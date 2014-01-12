(* Copyright (c) 2013 Radek Micek *)

open OUnit

module Array = Earray.Array

let assert_assig start len adeq_sizes max_size a =
  for i = start to start + len - 1 do
    let dsize =
      if adeq_sizes.(i) = 0 || adeq_sizes.(i) >= max_size then
        max_size
      else
        adeq_sizes.(i) in
    assert_bool "" (a.(i) >= 0);
    assert_bool "" (a.(i) < max_size);
    assert_bool "" (a.(i) < dsize)
  done

let assert_assig_me start len adeq_sizes max_size a =
  assert_assig start len adeq_sizes max_size a;
  assert_bool
    ""
    (Earray.rindex_of (fun _ x -> x = max_size - 1) a start len <> None)

let assert_assigs start len adeq_sizes max_size assigs =
  List.iter (assert_assig start len adeq_sizes max_size) assigs;
  let unique = BatList.sort_unique compare assigs in
  assert_equal (List.length assigs) (List.length unique)

let assert_assigs_me start len adeq_sizes max_size assigs =
  List.iter (assert_assig_me start len adeq_sizes max_size) assigs;
  let unique = BatList.sort_unique compare assigs in
  assert_equal (List.length assigs) (List.length unique)

let assert_assigs_comm_me start len adeq_sizes max_size assigs =
  let norm_comm a =
    if a.(start) > a.(start+1) then begin
      let tmp = a.(start) in
      a.(start) <- a.(start+1);
      a.(start+1) <- tmp
    end in
  List.iter (assert_assig_me start len adeq_sizes max_size) assigs;
  List.iter norm_comm assigs;
  let unique = BatList.sort_unique compare assigs in
  assert_equal (List.length assigs) (List.length unique)


(* ************************************************************************* *)


let test_c_zero_max_size () =
  assert_equal 0 (Assignment.count 0 2 [| 2; 0 |] 0)

let test_ecr_me_max_size_one () =
  let i = ref 0 in
  let start = 1 in
  let len = 3 in
  let adeq_sizes  = [| 2; 1; 4; 0 |] in
  let max_size = 1 in
  let cnt = Assignment.count_me start len adeq_sizes max_size in
  Assignment.each_me [| 5; 4; 3; 1 |] start len adeq_sizes max_size
    (fun a ->
      assert_equal [| 5; 0; 0; 0 |] a;
      assert_equal (0, 1) (Assignment.rank_me a start len adeq_sizes);
      incr i);
  assert_equal 1 cnt;
  assert_equal 1 !i

let test_ecr_comm_me_max_size_one () =
  let i = ref 0 in
  let start = 0 in
  let len = 4 in
  let adeq_sizes  = [| 2; 2; 4; 0 |] in
  let max_size = 1 in
  let cnt = Assignment.count_comm_me start len adeq_sizes max_size in
  Assignment.each_comm_me [| 5; 4; 3; 1 |] start len adeq_sizes max_size
    (fun a ->
      assert_equal [| 0; 0; 0; 0 |] a;
      assert_equal (0, 0) (Assignment.rank_comm_me a start len adeq_sizes);
      incr i);
  assert_equal 1 cnt;
  assert_equal 1 !i

let test_ec_zero_len () =
  let assigs = ref [] in
  let i = ref 0 in
  let start = 1 in
  let len = 0 in
  let adeq_sizes  = [| 1; 1 |] in
  let max_size = 1 in
  let cnt = Assignment.count start len adeq_sizes max_size in
  Assignment.each [| 1; 4 |] start len adeq_sizes max_size
    (fun a -> assigs := Earray.copy a :: !assigs; incr i);
  assert_equal 1 cnt;
  assert_equal 1 !i;
  assert_equal [[| 1; 4 |]] !assigs

let test_ec_me_zero_len () =
  let assigs = ref [] in
  let i = ref 0 in
  let start = 1 in
  let len = 0 in
  let adeq_sizes  = [| 1; 1 |] in
  let max_size = 1 in
  let cnt = Assignment.count_me start len adeq_sizes max_size in
  Assignment.each_me [| 1; 4 |] start len adeq_sizes max_size
    (fun a -> assigs := Earray.copy a :: !assigs; incr i);
  assert_equal 0 cnt;
  assert_equal 0 !i;
  assert_equal [] !assigs

let test_r_me () =
  let a = [| 3; 2; 4; 5; 1; 1 |] in
  let r, max_el_idx =
    Assignment.rank_me a 0 6 [| 6; 3; 6; 6; 4; 4 |] in
  let exp_r =
    (* The first occurence of max_el is at a.(0) *)
    3 * 6 * 6 * 4 * 4 +
    (* The first occurence of max_el is at a.(2) *)
    5 * 3 * 6 * 4 * 4 +
    (* The first occurence of max_el is at a.(3) *)
    3 * 3 * 5 * 4 * 4 +
    2 * 5 * 4 * 4 +
    4 * 4 * 4 +
    4 +
    1 in
  assert_equal exp_r r;
  assert_equal 3 max_el_idx

let test_ec () =
  let assigs = ref [] in
  let i = ref 0 in
  let start = 2 in
  let len = 4 in
  let adeq_sizes  = [| 8; 1; 5; 3; 6; 0 |] in
  let max_size = 6 in
  let cnt = Assignment.count start len adeq_sizes max_size in
  Assignment.each [| 1; 4; 0; 3; 12; 8 |] start len adeq_sizes max_size
    (fun a -> assigs := Earray.copy a :: !assigs; incr i);
  assert_equal cnt !i;
  assert_assigs start len adeq_sizes max_size !assigs

let test_ecr_me () =
  let assigs = ref [] in
  let i = ref 0 in
  let start = 1 in
  let len = 6 in
  let adeq_sizes  = [| 8; 1; 5; 3; 6; 0; 0 |] in
  let max_size = 6 in
  let cnt = Assignment.count_me start len adeq_sizes max_size in
  Assignment.each_me (Earray.make 7 ~-1)  start len adeq_sizes max_size
    (fun a ->
      assigs := Earray.copy a :: !assigs;
      let r, _ = Assignment.rank_me a start len adeq_sizes in
      assert_equal !i r;
      incr i);
  assert_equal cnt !i;
  assert_assigs_me start len adeq_sizes max_size !assigs

(* Commutative elements can be assigned max_el. *)
let test_ecr_comm_me1 () =
  let assigs = ref [] in
  let i = ref 0 in
  let start = 1 in
  let len = 6 in
  let adeq_sizes  = [| 5; 7; 7; 0; 6; 1; 4 |] in
  let max_size = 7 in
  let cnt = Assignment.count_comm_me start len adeq_sizes max_size in
  Assignment.each_comm_me (Earray.make 20 ~-1)  start len adeq_sizes max_size
    (fun a ->
      assigs := Earray.copy a :: !assigs;
      let r, _ = Assignment.rank_comm_me a start len adeq_sizes in
      assert_equal !i r;
      incr i);
  assert_equal cnt !i;
  assert_assigs_comm_me start len adeq_sizes max_size !assigs

(* Similar to test_ecr_comm_me1 but commutative elements have no adequate
   domain size.
*)
let test_ecr_comm_me1' () =
  let assigs = ref [] in
  let i = ref 0 in
  let start = 1 in
  let len = 6 in
  let adeq_sizes  = [| 5; 0; 0; 0; 6; 1; 4 |] in
  let max_size = 7 in
  let cnt = Assignment.count_comm_me start len adeq_sizes max_size in
  Assignment.each_comm_me (Earray.make 20 ~-1)  start len adeq_sizes max_size
    (fun a ->
      assigs := Earray.copy a :: !assigs;
      let r, _ = Assignment.rank_comm_me a start len adeq_sizes in
      assert_equal !i r;
      incr i);
  assert_equal cnt !i;
  assert_assigs_comm_me start len adeq_sizes max_size !assigs

(* Commutative elements cannot be assigned max_el. *)
let test_ecr_comm_me2 () =
  let assigs = ref [] in
  let i = ref 0 in
  let start = 1 in
  let len = 6 in
  let adeq_sizes  = [| 5; 6; 6; 0; 3; 8; 7 |] in
  let max_size = 8 in
  let cnt = Assignment.count_comm_me start len adeq_sizes max_size in
  Assignment.each_comm_me (Earray.make 20 ~-1)  start len adeq_sizes max_size
    (fun a ->
      assigs := Earray.copy a :: !assigs;
      let r, _ = Assignment.rank_comm_me a start len adeq_sizes in
      assert_equal !i r;
      incr i);
  assert_equal cnt !i;
  assert_assigs_comm_me start len adeq_sizes max_size !assigs

let suite =
  "Assignment suite" >:::
    [
      "count - zero max_size" >:: test_c_zero_max_size;
      "each_me, count_me, rank_me - max_size = 1" >:: test_ecr_me_max_size_one;
      "each_comm_me, count_comm_me, rank_comm_me - max_size = 1" >::
        test_ecr_comm_me_max_size_one;
      "each, count - zero len" >:: test_ec_zero_len;
      "each_me, count_me - zero len" >:: test_ec_me_zero_len;
      "rank_me" >:: test_r_me;
      "each, count" >:: test_ec;
      "each_me, count_me, rank_me" >:: test_ecr_me;
      "each_comm_me, count_comm_me, rank_comm_me 1" >:: test_ecr_comm_me1;
      "each_comm_me, count_comm_me, rank_comm_me 1'" >:: test_ecr_comm_me1';
      "each_comm_me, count_comm_me, rank_comm_me 2" >:: test_ecr_comm_me2;
    ]
