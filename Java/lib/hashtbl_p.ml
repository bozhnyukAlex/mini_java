(* Апдейтнутая хеш-таблица для работы с deriving*)
open Base.Hashtbl

type ('a, 'b) t = ('a, 'b) Base.Hashtbl.t

let pp pp_key pp_value ppf values =
  iteri values ~f:(fun ~key ~data ->
      Format.fprintf ppf "@[<1>%a@ ->@ %a@]@\n@." pp_key key pp_value data)

let get_elem_if_present key = Base.Hashtbl.find key
