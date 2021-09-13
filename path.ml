type t = (Symbol.sym list) * Symbol.sym

let compare (p1,_) (p2,_) =
  let rec eq = function
    | ([], []) -> 0
    | (h1 :: t1, h2 :: t2) ->
      if h1 == h2
      then eq (t1, t2)
      else (Symbol.to_int h1) - (Symbol.to_int h2)
    | ([], _) -> -1
    | (_, []) -> 1
  in
  let o = eq (p1, p2) in
  if eq (p1, List.rev p2) == 0 then 0 else o

let source (_, s) = s

let rev_path (p, _) = p

let mem (p, _) s = List.mem s p

let singleton s = ([s],s)

let snoc (p, src) s = (s :: p, src)