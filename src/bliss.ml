(* Copyright (c) 2013 Radek Micek *)

type graph

type color = int

type vertex = int

external create_graph : unit -> graph = "bls_create_graph"

external add_vertex : graph -> color -> vertex = "bls_add_vertex"

external add_edge : graph -> vertex -> vertex -> unit = "bls_add_edge"

external canonical_form : graph -> int -> vertex array = "bls_canonical_form"
