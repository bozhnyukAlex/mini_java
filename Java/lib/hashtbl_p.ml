(* Апдейтнутая хеш-таблица для работы с deriving*)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let create = Hashtbl.create

let pp pp_key pp_value ppf values =
  Hashtbl.iter
    (fun key data ->
      Format.fprintf ppf "@[<1>%a@ ->@ %a@]@\n@." pp_key key pp_value data)
    values

let filter : ('a, 'b) t -> ('a -> 'b -> bool) -> ('a, 'b) t =
 fun ht f ->
  let new_table = Hashtbl.create 100 in
  Hashtbl.iter (fun k v -> if f k v then Hashtbl.add new_table k v) ht;
  new_table

let get_elem_if_present key = Hashtbl.find_opt key
