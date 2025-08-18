type id_kind =
  | Any
  | Event
  | Partner

type error =
  | Internal of string
  | Parsing of { loc : Pos.t option; msg : string }
  | Repartition of { pool : Variable.t; rep : R.t }
  | MissingDestination
  | UnknownIdentifier of { loc : Pos.t; id : string; kind : id_kind }
  | MultipleDefRep of { pool : Variable.t }
  | Typing of { loc : Pos.t }
  | ForbiddenNonLinear of { loc : Pos.t }
  | UselessOpposition of { loc : Pos.t; towards : Variable.t }
  | MultipleOppositionProvider of { locs : Pos.t list; towards : Variable.t }
  | User of { locs : Pos.t list; msg : string }

let print_error (pinfo : ProgramInfo.t) fmt (err : error) =
  match err with
  | Internal msg -> Format.fprintf fmt "Internal error: %s" msg
  | Parsing { loc; msg } ->
    Format.fprintf fmt "Parsing error%a:@\n%s"
      Fmt.(option (any ", " ++ Pos.Text.pp)) loc
      msg
  | Repartition { pool; rep } ->
    Format.fprintf fmt "Error on repartition: ";
    if R.(rep > one) then
      Format.fprintf fmt "Pool %S is too high (%g%%), needs a deficit"
        (VarInfo.get_any_name pinfo.var_info pool)
        R.(to_float (rep * ~$100))
    else
      Format.fprintf fmt "Pool %S is too low (%g%%), needs a default"
        (VarInfo.get_any_name pinfo.var_info pool)
        R.(to_float (rep * ~$100))
  | MissingDestination ->
    Format.fprintf fmt "No destination for repartition"
  | UnknownIdentifier { loc; id; kind } ->
    Format.fprintf fmt "Error, %a:@\nUnknown %sidentifier '%s'"
      Pos.Text.pp loc
      (match kind with Any -> "" | Event -> "event " | Partner -> "partner ")
      id
  | MultipleDefRep { pool } ->
    Format.fprintf fmt "Error: Cannot have several deficit/default for pool '%s'"
      (VarInfo.get_any_name pinfo.var_info pool)
  | Typing { loc } ->
    Format.fprintf fmt "Error, %a:@\nMismatching types"
      Pos.Text.pp loc
  | ForbiddenNonLinear { loc } ->
    Format.fprintf fmt "Error, %a:@\nNon-linear expression forbidden, \
                        cannot ensure discretized continuity"
      Pos.Text.pp loc
  | UselessOpposition { loc; towards } ->
    Format.fprintf fmt "Error, %a:@\nUseless opposed value towards '%s'"
      Pos.Text.pp loc
      (VarInfo.get_any_name pinfo.var_info towards)
  | MultipleOppositionProvider { locs; towards } ->
    Format.fprintf fmt "Error, %a:@\nSeveral opposition provider towards '%s'"
      (Format.pp_print_list
         ~pp_sep:(fun fmt () -> Format.fprintf fmt ",@ ")
         Pos.Text.pp) locs
      (VarInfo.get_any_name pinfo.var_info towards)
  | User { locs = _; msg } ->
    Format.pp_print_string fmt msg

include Logs

type info = {
  pinfo : ProgramInfo.t;
  kind : error;
}

let infos_tag : info Tag.def =
  Tag.def "error_info" ~doc:"Error information" Fmt.nop

let info_err pinfo kind = Tag.(add infos_tag { pinfo; kind } empty)

let log_error fmt pinfo t =
  err (fun m -> m fmt ~tags:(info_err pinfo t));
  assert false

let raise_internal_error fmt =
  let k s =
    log_error "Internal error" ProgramInfo.dummy (Internal s)
  in
  Fmt.kstr k fmt

let raise_parsing_error ?loc msg =
  log_error "Parsing error" ProgramInfo.dummy (Parsing { loc; msg })

let raise_repartition_error pinfo pool rep =
  log_error "Repartition error" pinfo (Repartition { pool; rep })

let raise_missing_dest_error () =
  log_error "Missing destination" ProgramInfo.dummy MissingDestination

let raise_unknown_id_error ?(loc = Pos.dummy) id kind =
  log_error "Unknown identifier" ProgramInfo.dummy (UnknownIdentifier { loc; id; kind })

let raise_multiple_def_rep_error pinfo pool =
  log_error "Multiple def rep error" pinfo (MultipleDefRep { pool })

let raise_typing_error ?(loc = Pos.dummy) () =
  log_error "Typing error" ProgramInfo.dummy (Typing { loc })

let raise_nonlinear_error ?(loc = Pos.dummy) () =
  log_error "Non-linear error" ProgramInfo.dummy (ForbiddenNonLinear { loc })

let raise_useless_opposition_error ?(loc = Pos.dummy) pinfo towards =
  log_error "Useless opposition" pinfo (UselessOpposition { loc; towards })

let raise_multiple_opp_provider_error ?(locs = []) pinfo towards =
  log_error "Useless opposition" pinfo (MultipleOppositionProvider { locs; towards })

let raise_error ?(locs = []) fmt =
  let k msg = log_error "Usage error" ProgramInfo.dummy (User { locs; msg }) in
  Fmt.kstr k fmt

let cli_reporting_init () =
  let ppf = Format.err_formatter in
  let report _src _level ~over _k msgf =
    let k _ = over (); exit 50 in
    let with_infos _headers tags k ppf fmt =
      let infos =
        match tags with
        | None -> None
        | Some tags -> Logs.Tag.find infos_tag tags
      in
      match infos with
      | None -> Format.kfprintf k ppf ("Unknown error: " ^^ fmt ^^ "@.")
      | Some { pinfo; kind } ->
        Format.fprintf ppf "%a@." (print_error pinfo) kind;
        Format.ikfprintf k ppf fmt
    in
    msgf @@ (fun ?header ?tags fmt -> with_infos header tags k ppf fmt)
  in
  set_reporter { Logs.report }
