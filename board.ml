open Symbol

type t = Set.t Map.t

let top () = Map.init (fun s -> Set.empty)

let possible board s1 s2 =
  not (Set.member s2 (Map.get board s1))

let possibles board s =
  let l = ref [] in
  Symbol.iter
    (fun s2 ->
      if (possible board s s2)
      then l := s2 :: !l
    );
  !l

let print ~long out board =
  let print_one_symbol s =
    Printf.fprintf out "p(%c) = {" (to_char s);
    Symbol.iter
      (fun s2 ->
        if (possible board s s2)
        then Printf.fprintf out "%c " (to_char s2)
      );
    Printf.fprintf out "}\n" 
  in
  Symbol.iter print_one_symbol

exception Impossible

let rec remove_assoc board x y =
  let assoc = Set.add y (Map.get board x) in
  let assoc' = Set.add x (Map.get board y) in
  Map.set board x assoc;
  Map.set board y assoc';
  let update_for a except =
    let ps_l = possibles board a in
    match ps_l with
    | []  -> raise Impossible
    | [x] -> Symbol.iter
      (fun s ->
        if (s <> a) && possible board x s
        then remove_assoc board x s)
    | _ -> ()
  in
  update_for x y;
  update_for y x

type state = bool

let save board = true

let restore state = ()

