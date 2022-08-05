(* heterogeneous lists *)

module Simple = struct
  (* type-level access indices *)
  (* [hd], [tl], and [get] can all be defined without lenses *)
  type ('a, 'xs) idx =
    | Zero : ('a, 'a * 'tl) idx
    | Succ : ('a, 'aa) idx -> ('a, 'hd * 'aa) idx

  (* simple hlist *)
  type _ t =
    | ( :: ) : 'hd * 'tl t -> ('hd * 'tl) t
    | [] : 'a t

  let hd : type hd tl. (hd * tl) t -> hd = function
    | [] -> failwith "hd of empty simple hlist"
    | x :: _ -> x

  let tl : type hd tl. (hd * tl) t -> tl t = function
    | [] -> failwith "tl of empty simple hlist"
    | _ :: tl -> tl

  let rec get : type a xs. (a, xs) idx -> xs t -> a =
   fun ln xs ->
    match ln with
    | Zero -> hd xs
    | Succ ln' -> get ln' (tl xs)
end

module Complex = struct
  (* lenses are required to define [put] *)
  type ('a, 'b, 'xs, 'ys) idx =
    | Zero : ('a, 'b, 'a * 'tl, 'b * 'tl) idx
    | Succ : ('a, 'b, 'aa, 'bb) idx -> ('a, 'b, 'hd * 'aa, 'hd * 'bb) idx

  (* complex hlist *)
  type _ t =
    | ( :: ) : 'hd * 'tl t -> ('hd * 'tl) t
    | [] : 'a t

  let hd : type hd tl. (hd * tl) t -> hd = function
    | [] -> failwith "hd of empty complex hlist"
    | x :: _ -> x

  let tl : type hd tl. (hd * tl) t -> tl t = function
    | [] -> failwith "tl of empty complex hlist"
    | _ :: tl -> tl

  let rec get : type a b xs ys. (a, b, xs, ys) idx -> xs t -> a =
   fun ln xs ->
    match ln with
    | Zero -> hd xs
    | Succ ln' -> get ln' (tl xs)

  let rec put : type a b xs ys. (a, b, xs, ys) idx -> xs t -> b -> ys t =
   fun ln xs b ->
    match ln with
    | Zero -> b :: tl xs
    | Succ ln' -> hd xs :: put ln' (tl xs) b
end
