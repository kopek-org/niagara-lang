
(* raw error *)
type t =
  | Internal of string
  | Parsing of { loc : Pos.t option; msg : string }
  | User of { locs : Pos.t list; msg : string }

let print_raw fmt t =
  match t with
  | Internal msg -> Format.fprintf fmt "(internal) %s" msg
  | Parsing { loc; msg } ->
    Format.fprintf fmt "Parsing error%a:@\n%s"
      Fmt.(option (any ", " ++ Pos.Text.pp)) loc
      msg
  | User { locs = _; msg } ->
    Format.pp_print_string fmt msg

include Logs

type info = {
  kind : t;
}

let infos_tag : info Tag.def =
  Tag.def "error_info" ~doc:"Error information" Fmt.nop

let info_err kind = Tag.(add infos_tag { kind } empty)

let log_error fmt t =
  err (fun m -> m fmt ~tags:(info_err t));
  assert false

let raise_internal_error fmt =
  let k s =
    log_error "Internal error" (Internal s)
  in
  Fmt.kstr k fmt

let raise_parsing_error ?loc msg =
  log_error "Parsing error" (Parsing { loc; msg })

let raise_error ?(locs = []) fmt =
  let k msg = log_error "Usage error" (User { locs; msg }) in
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
      | Some { kind } ->
        Format.fprintf ppf "%a@." print_raw kind;
        Format.ikfprintf k ppf fmt
    in
    msgf @@ (fun ?header ?tags fmt -> with_infos header tags k ppf fmt)
  in
  set_reporter { Logs.report }
