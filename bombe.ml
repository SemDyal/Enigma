
let infer_plug_constraint graph cycles machine =
  let rec machine_i n m =
    if n = 0 then m
    else machine_i (n-1) (Enigma.step m)
  in
  let board = Board.top () in
  let apply_cycle s pos =
    match pos with
    | [p1; p2] -> begin
        let m1 = machine_i p1 machine in
        let m2 = machine_i p2 machine in
        let test x =
          let res = Enigma.image m1 (Enigma.image m2 x) in
          if res <> s then Board.remove_assoc board res x
        in
        Symbol.iter test
      end
    | _ -> ()
  in
  Cycles.iter_multi apply_cycle graph cycles;
  board


(** Behaviour of the "bombe" executable. *)
let () =
  if Filename.basename Sys.argv.(0) = "bombe" then begin

    let rec run m args =
      Printf.printf "Using machine %s:\n%a\n" (Enigma.to_string m) Enigma.print m;
      let clear,cipher,args = match args with
        | arg::cip::args ->
            Printf.printf "Translating command-line argument...\n" ;
            arg,cip,args
        | _ ->
            Printf.printf "Type your text, or Ctrl-D to exit: %!" ;
            input_line stdin,input_line stdin, args
      in
      let clear = String.uppercase_ascii clear in
      let cipher = String.uppercase_ascii cipher in
      let g = Cycles.graph_of_known_cipher clear cipher in
      let cs = Cycles.cycles g in
      Cycles.iter_fusion (fun p -> Graph.print_path g Stdlib.stdout (Path.rev_path p)) cs;
      let b = infer_plug_constraint g cs m in
      Board.print ~long:false Stdlib.stdout b;
      Printf.printf "\n%!" ;
      run m args
    in
      try run Enigma.test (List.tl (Array.to_list Sys.argv)) with End_of_file -> Printf.printf "\n"

  end