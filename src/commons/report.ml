type error =
  | Internal of string
  | Parsing of { loc : Pos.t option; msg : string }
  | Repartition of { pool : Variable.t; rep : R.t }
  | User of { locs : Pos.t list; msg : string }

let print_error (pinfo : ProgramInfo.t) fmt (err : error) =
  match err with
  | Internal msg -> Format.fprintf fmt "(internal) %s" msg
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
