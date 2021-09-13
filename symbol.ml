type sym = int

let a = 0

let nb_syms = 26

let of_char c = (Char.code c) - (Char.code 'A')

let to_char s = Char.chr (s + Char.code 'A')

let of_int s = s

let to_int s = s

let next s = (s + 1) mod nb_syms

let (++) s1 s2 = (s1 + s2) mod nb_syms

let (--) s1 s2 = (nb_syms + s1 - s2) mod nb_syms

let iter f = for i = 0 to (nb_syms - 1) do f i done

let fold f d = 
  let rec aux acc s =
    if s < nb_syms
    then aux (f acc s) (s + 1)
    else acc
  in aux d a

module Set = struct
  type t = int

  let empty = 0

  let singleton s = Int.shift_left 1 s

  let member s set = (singleton s) = (Int.logand set (singleton s))

  let add s set = Int.logor set (singleton s)
end

module Map = struct
  type 'a t = 'a array

  let get map s = map.(s)

  let set map s v = map.(s) <- v

  let make v = Array.make nb_syms v

  let init f = Array.init nb_syms f

  let copy = Array.copy

  let map = Array.map

  let inverse map =
    let m = make a in
    Array.iteri (fun s1 s2 -> set m s2 s1) map;
    m

  let print_tmap out map =
    Array.iter (fun s -> Printf.fprintf out "%c " (to_char s)) map

end
