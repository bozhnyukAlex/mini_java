open Ast
open Parser
open Pretty_printer
open Format

(*------------------------------ RENAMING VARIABLES ------------------------------ *)
let pp_dif old_pp new_pp ppf = function
  | el -> fprintf ppf "-- %a\n++ %a\n\n" old_pp el new_pp el

let rec look_up_expr old_name = function
  | Add (l, r)
  | Sub (l, r)
  | Div (l, r)
  | Mod (l, r)
  | Mult (l, r)
  | Or (l, r)
  | And (l, r)
  | Equal (l, r)
  | NotEqual (l, r)
  | Less (l, r)
  | More (l, r)
  | LessOrEqual (l, r)
  | MoreOrEqual (l, r)
  | Assign (l, r)
  | FieldAccess (l, r)
  | ArrayAccess (l, r) ->
      look_up_expr old_name l || look_up_expr old_name r
  | Identifier name -> old_name = name
  | This | Super | Null | Const _ -> false
  | PrefInc e | PrefDec e | PostInc e | PostDec e | Not e ->
      look_up_expr old_name e
  | CallMethod (_, args_list)
  | ClassCreate (_, args_list)
  | ArrayCreateElements (_, args_list) ->
      List.exists (fun e -> look_up_expr old_name e) args_list
  | ArrayCreateSized (_, szexpr) -> look_up_expr old_name szexpr

let rec pp_exp_nn old_n new_n ppf = function
  | Add (left, right) ->
      fprintf ppf "%a + %a" (pp_exp_nn old_n new_n) left (pp_exp_nn old_n new_n)
        right
  | Sub (left, right) ->
      fprintf ppf "%a - %a" (pp_exp_nn old_n new_n) left (pp_exp_nn old_n new_n)
        right
  | Mult (left, right) ->
      if is_add_or_sub left then (
        fprintf ppf "%a * " (pp_exp_par_nn old_n new_n) left;
        if is_add_or_sub right then
          fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
      else (
        fprintf ppf "%a * " (pp_exp_nn old_n new_n) left;
        if is_add_or_sub right then
          fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
  | Div (left, right) ->
      if is_add_or_sub left then (
        fprintf ppf "%a / " (pp_exp_par_nn old_n new_n) left;
        if is_add_or_sub right then
          fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
      else (
        fprintf ppf "%a / " (pp_exp_nn old_n new_n) left;
        if is_add_or_sub right then
          fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
  | Mod (left, right) ->
      if is_add_or_sub left then (
        fprintf ppf "%a %% " (pp_exp_par_nn old_n new_n) left;
        if is_add_or_sub right then
          fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
      else (
        fprintf ppf "%a %% " (pp_exp_nn old_n new_n) left;
        if is_add_or_sub right then
          fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
  | And (left, right) ->
      if is_or left then (
        fprintf ppf "%a && " (pp_exp_par_nn old_n new_n) left;
        if is_or right then fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
      else (
        fprintf ppf "%a && " (pp_exp_nn old_n new_n) left;
        if is_or right then fprintf ppf "%a" (pp_exp_par_nn old_n new_n) right
        else fprintf ppf "%a" (pp_exp_nn old_n new_n) right )
  | Or (left, right) ->
      fprintf ppf "%a || %a" (pp_exp_nn old_n new_n) left
        (pp_exp_nn old_n new_n) right
  | Not be ->
      if is_const_or_id be then fprintf ppf "!%a" (pp_exp_nn old_n new_n) be
      else fprintf ppf "!%a" (pp_exp_par_nn old_n new_n) be
  | Less (left, right) ->
      fprintf ppf "%a < %a" (pp_exp_nn old_n new_n) left (pp_exp_nn old_n new_n)
        right
  | More (left, right) ->
      fprintf ppf "%a > %a" (pp_exp_nn old_n new_n) left (pp_exp_nn old_n new_n)
        right
  | LessOrEqual (left, right) ->
      fprintf ppf "%a <= %a" (pp_exp_nn old_n new_n) left
        (pp_exp_nn old_n new_n) right
  | MoreOrEqual (left, right) ->
      fprintf ppf "%a >= %a" (pp_exp_nn old_n new_n) left
        (pp_exp_nn old_n new_n) right
  | Equal (left, right) ->
      fprintf ppf "%a == %a" (pp_exp_nn old_n new_n) left
        (pp_exp_nn old_n new_n) right
  | NotEqual (left, right) ->
      fprintf ppf "%a != %a" (pp_exp_nn old_n new_n) left
        (pp_exp_nn old_n new_n) right
  | PostInc e -> fprintf ppf "%a++" (pp_exp_nn old_n new_n) e
  | PrefInc e -> fprintf ppf "++%a" (pp_exp_nn old_n new_n) e
  | PostDec e -> fprintf ppf "%a--" (pp_exp_nn old_n new_n) e
  | PrefDec e -> fprintf ppf "--%a" (pp_exp_nn old_n new_n) e
  | Null -> fprintf ppf "null"
  | This -> fprintf ppf "this"
  | Super -> fprintf ppf "super"
  | Const v -> fprintf ppf "%a" pp_val v
  | Identifier s ->
      if s = old_n then fprintf ppf "%s" new_n else fprintf ppf "%s" s
  | FieldAccess (left, right) ->
      if is_create left then
        fprintf ppf "%a.%a"
          (pp_exp_par_nn old_n new_n)
          left (pp_exp_nn old_n new_n) right
      else
        fprintf ppf "%a.%a" (pp_exp_nn old_n new_n) left (pp_exp_nn old_n new_n)
          right
  | ArrayAccess (arre, index) ->
      fprintf ppf "%a[%a]" (pp_exp_nn old_n new_n) arre (pp_exp_nn old_n new_n)
        index
  | ArrayCreateSized (arr_type, size) ->
      fprintf ppf "new %a[%a]" pp_type arr_type (pp_exp_nn old_n new_n) size
  | ArrayCreateElements (arr_type, expr_list) ->
      fprintf ppf "new %a[] {%a}" pp_type arr_type
        (pp_exp_list_nn old_n new_n)
        expr_list
  | ClassCreate (name, expr_list) ->
      fprintf ppf "new %a(%a)" pp_name name
        (pp_exp_list_nn old_n new_n)
        expr_list
  | CallMethod (m_expr, expr_list) ->
      fprintf ppf "%a(%a)" (pp_exp_nn old_n new_n) m_expr
        (pp_exp_list_nn old_n new_n)
        expr_list
  | Assign (left, right) ->
      fprintf ppf "%a = %a" (pp_exp_nn old_n new_n) left (pp_exp_nn old_n new_n)
        right

and pp_exp_par_nn old_n new_n ppf = fprintf ppf "(%a)" (pp_exp_nn old_n new_n)

and pp_exp_list_nn old_n new_n ppf =
  pp_print_list ~pp_sep:pp_sep_comma (pp_exp_nn old_n new_n) ppf

let look_up_nepairs old_n =
  List.exists (function Name name, expr_o ->
      ( match expr_o with
      | Some expr -> name = old_n || look_up_expr old_n expr
      | None -> name = old_n ))

let pp_new_nepair old_n new_n ppf = function
  | Name n, expr_o -> (
      match expr_o with
      | Some expr ->
          if n = old_n then
            fprintf ppf "%s = %a" new_n (pp_exp_nn old_n new_n) expr
          else fprintf ppf "%s = %a" n (pp_exp_nn old_n new_n) expr
      | None -> if n = old_n then fprintf ppf "%s" new_n else fprintf ppf "%s" n
      )

let pp_new_nepairs old_n new_n ppf =
  pp_print_list ~pp_sep:pp_sep_comma (pp_new_nepair old_n new_n) ppf

let pp_varfield ppf = function
  | ml, VarField (t, ps) ->
      fprintf ppf "%a %a %a;" pp_modif_list ml pp_type t pp_pairs_dec_l ps
  | _ -> ()

let pp_new_varfield old_n new_n ppf = function
  | ml, VarField (t, ps) ->
      fprintf ppf "%a %a %a;" pp_modif_list ml pp_type t
        (pp_new_nepairs old_n new_n)
        ps
  | _ -> ()

let rec pp_st_nn old_n new_n ppf = function
  | If (bexpr, thenSt, elseSt_o) -> (
      let pp_head pp_e ppf be = fprintf ppf "if (%a)" pp_e be in
      if look_up_expr old_n bexpr then
        pp_dif (pp_head pp_exp) (pp_head (pp_exp_nn old_n new_n)) ppf bexpr
      else ();
      pp_st_nn old_n new_n ppf thenSt;
      match elseSt_o with
      | Some elseSt -> pp_st_nn old_n new_n ppf elseSt
      | None -> () )
  | While (bexpr, body) ->
      let pp_head pp_e ppf be = fprintf ppf "while (%a)" pp_e be in
      if look_up_expr old_n bexpr then
        pp_dif (pp_head pp_exp) (pp_head (pp_exp_nn old_n new_n)) ppf bexpr
      else ();
      pp_st_nn old_n new_n ppf body
  | Return expr_o -> (
      match expr_o with
      | None -> ()
      | Some expr ->
          let pp_head pp_e ppf e = fprintf ppf "return %a;" pp_e e in
          if look_up_expr old_n expr then
            pp_dif (pp_head pp_exp) (pp_head (pp_exp_nn old_n new_n)) ppf expr
          else () )
  | For (decs_o, bexpr_o, after_elist, body) ->
      let pp_init_nn old_n new_n ppf = function
        | VarDec (mod_o, v_type, pairs_list) -> (
            match mod_o with
            | Some modif ->
                fprintf ppf "%a %a %a;" pp_modif modif pp_type v_type
                  (pp_new_nepairs old_n new_n)
                  pairs_list
            | None ->
                fprintf ppf "%a %a;" pp_type v_type
                  (pp_new_nepairs old_n new_n)
                  pairs_list )
        | _ -> ()
      in
      ( match (decs_o, bexpr_o) with
      | Some decs, Some bexpr ->
          let pp_head pp_s pp_e pp_aft ppf = function
            | de, be, aft ->
                fprintf ppf "for (%a %a; %a)" pp_s de pp_e be pp_aft aft
          in
          (match decs with VarDec (_, _, plist) -> plist | _ -> [])
          |> fun pair_list ->
          if
            look_up_expr old_n bexpr
            || look_up_nepairs old_n pair_list
            || List.exists (look_up_expr old_n) after_elist
          then
            pp_dif
              (pp_head pp_st pp_exp pp_exp_list)
              (pp_head (pp_init_nn old_n new_n) (pp_exp_nn old_n new_n)
                 (pp_exp_list_nn old_n new_n))
              ppf (decs, bexpr, after_elist)
      | None, Some bexpr ->
          let pp_head pp_e pp_aft ppf = function
            | be, aft -> fprintf ppf "for (; %a; %a)" pp_e be pp_aft aft
          in
          if
            look_up_expr old_n bexpr
            || List.exists (look_up_expr old_n) after_elist
          then
            pp_dif
              (pp_head pp_exp pp_exp_list)
              (pp_head (pp_exp_nn old_n new_n) (pp_exp_list_nn old_n new_n))
              ppf (bexpr, after_elist)
      | Some decs, None ->
          let pp_head pp_st pp_aft ppf = function
            | de, aft -> fprintf ppf "for (%a; %a)" pp_st de pp_aft aft
          in
          (match decs with VarDec (_, _, plist) -> plist | _ -> [])
          |> fun pair_list ->
          if
            look_up_nepairs old_n pair_list
            || List.exists (look_up_expr old_n) after_elist
          then
            pp_dif
              (pp_head pp_st pp_exp_list)
              (pp_head (pp_init_nn old_n new_n) (pp_exp_list_nn old_n new_n))
              ppf (decs, after_elist)
      | None, None ->
          let pp_head pp_aft ppf = function
            | aft -> fprintf ppf "for (;; %a)" pp_aft aft
          in
          if List.exists (look_up_expr old_n) after_elist then
            pp_dif (pp_head pp_exp_list)
              (pp_head (pp_exp_list_nn old_n new_n))
              ppf after_elist );
      pp_st_nn old_n new_n ppf body
  | VarDec (modif_o, var_t, ne_pairs_list) -> (
      if look_up_nepairs old_n ne_pairs_list then
        match modif_o with
        | Some modif ->
            let pp_vardec pp_e ppf = function
              | md, vt, pl ->
                  fprintf ppf "%a %a %a;" pp_modif md pp_type vt pp_e pl
            in
            pp_dif (pp_vardec pp_pairs_dec_l)
              (pp_vardec (pp_new_nepairs old_n new_n))
              ppf
              (modif, var_t, ne_pairs_list)
        | None ->
            let pp_vardec pp_e ppf = function
              | vt, pl -> fprintf ppf "%a %a;" pp_type vt pp_e pl
            in
            pp_dif (pp_vardec pp_pairs_dec_l)
              (pp_vardec (pp_new_nepairs old_n new_n))
              ppf (var_t, ne_pairs_list) )
  | Expression expr ->
      if look_up_expr old_n expr then
        let pp_est pp_e ppf = function e -> fprintf ppf "%a;" pp_e e in
        pp_dif (pp_est pp_exp) (pp_est (pp_exp_nn old_n new_n)) ppf expr
  | StmtBlock stl -> List.iter (pp_st_nn old_n new_n ppf) stl
  | Break | Continue -> ()

let look_up_tnpairs old_n =
  let look_up_tnpair old_n = function _, Name n -> n = old_n in
  List.exists (look_up_tnpair old_n)

let pp_tn_pairs_nn old_n new_n ppf =
  let pp_tn_pair old_n new_n ppf = function
    | t, Name n ->
        if n = old_n then fprintf ppf "%a %s" pp_type t n
        else fprintf ppf "%a %s" pp_type t new_n
  in
  pp_print_list ~pp_sep:pp_sep_comma (pp_tn_pair old_n new_n) ppf

let rename_cl_elem old_n new_n ppf el =
  match el with
  | _, VarField (_, ne_pair_list) ->
      if look_up_nepairs old_n ne_pair_list then
        pp_dif pp_varfield (pp_new_varfield old_n new_n) ppf el
  | ml, Method (m_type, m_name, args, body_o) -> (
      ( if look_up_tnpairs old_n args then
        let pp_m pp_args ppf = function
          | arg ->
              fprintf ppf "%a %a %a(%a)" pp_modif_list ml pp_type m_type
                Pretty_printer.pp_name m_name pp_args arg
        in
        pp_dif (pp_m pp_tn_pairs_list)
          (pp_m (pp_tn_pairs_nn old_n new_n))
          ppf args );
      match body_o with
      | Some body -> pp_st_nn old_n new_n ppf body
      | None -> () )
  | ml, Constructor (c_name, args, body) ->
      ( if look_up_tnpairs old_n args then
        let pp_c pp_args ppf = function
          | arg ->
              fprintf ppf "%a %a(%a)" pp_modif_list ml Pretty_printer.pp_name
                c_name pp_args arg
        in
        pp_dif (pp_c pp_tn_pairs_list)
          (pp_c (pp_tn_pairs_nn old_n new_n))
          ppf args );
      pp_st_nn old_n new_n ppf body

let rename_cd old_n new_n ppf = function
  | Class (_, _, _, mfpairs_list) ->
      List.iter (rename_cl_elem old_n new_n ppf) mfpairs_list

let rename_cdlist old_n new_n ppf = List.iter (rename_cd old_n new_n ppf)

let transform_rename prog old_n new_n ppf =
  let tree =
    match apply parser prog with
    | None | Some [] -> raise (Invalid_argument "Syntax error or empty")
    | Some res -> res
  in
  try rename_cdlist old_n new_n ppf tree
  with Invalid_argument m -> fprintf ppf "%s" m
