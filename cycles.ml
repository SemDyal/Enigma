
let graph_of_known_cipher clear cipher =
  let graph = Graph.create () in
  String.iteri
    (fun i c -> Graph.add_edge graph (Symbol.of_char c) (Symbol.of_char (String.get cipher i)) i)
    clear;
  graph


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
        Printf.printf "\n" ;
        Graph.print_path g Pervasives.stdout [Symbol.a];
        Printf.printf "\n%!" ;
        run args
    in
      try run (List.tl (Array.to_list Sys.argv)) with End_of_file -> Printf.printf "\n"

  end