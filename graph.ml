module Positions = Set.Make(Int)

type t = (Positions.t array) array

let create () = Array.make_matrix Symbol.nb_syms Symbol.nb_syms (Positions.empty)

let add_edge g a b i =
  let ia = Symbol.to_int a in
  let ib = Symbol.to_int b in
  g.(ia).(ib) <- Positions.add i g.(ia).(ib);
  g.(ib).(ia) <- Positions.add i g.(ib).(ia)

let get_edge g a b = 
  let ia = Symbol.to_int a in
  let ib = Symbol.to_int b in
  g.(ia).(ib)

let filter_mapi f list =
  let f' (i, x) = f i x in
  List.filter_map f' (List.mapi (fun i e -> (i, e)) list)

let fold_over_connected g f x s =
  let is = Symbol.to_int s in
  let connected =
    filter_mapi
      (fun i pos -> if Positions.is_empty pos then None else Some i)
      (Array.to_list g.(is))
  in
  List.fold_left (fun acc i -> f acc (Symbol.of_int i)) x connected

let print_path g out list =
  let rec aux = function
    | h1 :: h2 :: t -> begin
        Printf.fprintf out " - [";
        Positions.iter (fun e -> Printf.fprintf out "%d " e) (get_edge g h1 h2);
        Printf.fprintf out "] -> %c" (Symbol.to_char h2); (* TODO *)
        aux (h2 :: t)
      end
    | _ -> ()
  in
  if list <> []
  then Printf.fprintf out "%c" (Symbol.to_char (List.hd list)); 
  aux list
