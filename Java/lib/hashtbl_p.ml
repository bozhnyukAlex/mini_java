(* Апдейтнутая хеш-таблица для работы с deriving*)

type ('a, 'b) t = ('a, 'b) Hashtbl.t

let create = Hashtbl.create

let pp pp_key pp_value ppf values =
  Hashtbl.iter
    (fun key data ->
      Format.fprintf ppf "@[<1>%a@ ->@ %a@]@\n@." pp_key key pp_value data)
    values

let get_elem_if_present key = Hashtbl.find_opt key
