module Paths = Set.Make(Path)

type t = Paths.t

let memp ps p = Paths.mem p ps

let graph_of_known_cipher clear cipher =
  let graph = Graph.create () in
  String.iteri
    (fun i c -> Graph.add_edge graph (Symbol.of_char c) (Symbol.of_char (String.get cipher i)) i)
    clear;
  graph

let cycles g =
  let can_extend p = match Path.rev_path p with
    | h :: _ :: _ -> h <> (Path.source p)
    | _ -> true
  in
  let new_path_is_elem p s =
    (not (Path.mem p s)) || (Path.source p == s)
  in
  let new_path_is_ord p =
    let src = Path.source p in
    List.fold_left (fun a s -> a && (src <= s)) true (Path.rev_path p)
  in
  let init_set =
    Symbol.fold (fun acc s -> Paths.add (Path.singleton s) acc) Paths.empty
  in
  let extend_path p = match Path.rev_path p with
    | start :: _ ->
      Graph.fold_over_connected g
        (fun acc s -> 
          if (can_extend p && new_path_is_elem p s)
          then
            let new_path = Path.snoc p s in
            if new_path_is_ord new_path
            then Paths.add new_path acc
            else acc
          else acc)
        (Paths.singleton p)
        start
    | _ -> failwith ""
  in
  let all_paths =
    Symbol.fold
      (fun current_set _ ->
        Paths.fold
          (fun p acc -> Paths.union (extend_path p) acc)
          current_set
          Paths.empty
      )
      init_set
  in
  Paths.fold
    (fun p acc ->
      if can_extend p
      then acc
      else Paths.add p acc)
    all_paths
    Paths.empty 

let iter_fusion f cycles =
  Paths.iter f cycles

let iter_multi f cycles = 
  Paths.iter f cycles (*TODO*)


(** Behaviour of the "enigma" executable. *)
let () =
  if Filename.basename Sys.argv.(0) = "cycles" then begin

    let rec run args =
      let clear,cipher,args = match args with
        | arg::cip::args ->
            Printf.printf "Translating command-line argument...\n" ;
            arg,cip,args
        | _ ->
            Printf.printf "Type your text, or Ctrl-D to exit: %!" ;
            input_line stdin,input_line stdin, args
      in
      let clear = String.uppercase clear in
      let cipher = String.uppercase cipher in
      let g = graph_of_known_cipher clear cipher in
      let cs = cycles g in
        Printf.printf "\n %s %s \n" clear cipher;
        iter_fusion (fun p -> Graph.print_path g Pervasives.stdout (Path.rev_path p)) cs;
        Printf.printf "\n%!" ;
        run args
    in
      try run (List.tl (Array.to_list Sys.argv)) with End_of_file -> Printf.printf "\n"

  end