type t

val memp : t -> Path.t -> bool

val graph_of_known_cipher : string -> string -> Graph.t

val cycles : Graph.t -> t

val iter_fusion : (Path.t -> unit) -> t -> unit

val iter_multi : (Symbol.sym -> (int list) -> unit) -> Graph.t -> t -> unit