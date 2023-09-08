include Logs

let loc_tag : Pos.t Tag.def =
  Tag.def "loc" ~doc:"Error location" Pos.pp

type kind =
  | Primary
  | Secondary
  | Note
  | Hint

(* let kind_to_string : kind -> string = function
  | Primary -> "primary"
  | Secondary -> "secondary"
  | Note -> "note"
  | Hint -> "hint" *)

type info = {
  kind : kind;
  loc : Pos.t;
  msg : string;
}

(* keeps info in reverse order *)
let infos_tag : info list Tag.def =
  Tag.def "packed" ~doc:"Packed log message"
    (fun ppf _ -> Fmt.pf ppf "<infos>")

type detail = Tag.set -> Tag.set

let pack : detail list -> Tag.set = fun details ->
  List.fold_right (fun f -> f) details Tag.empty

let ( !! ) = pack

let loc : Pos.t -> detail = Tag.add loc_tag

let detail :
  ?loc:Pos.t -> kind -> ('a, Format.formatter, unit, detail) format4 -> 'a =
  fun ?(loc = Pos.dummy) kind fmt ->
    let k msg = fun tags ->
      let infos = match Tag.find infos_tag tags with
        | None -> []
        | Some infos -> infos in
      Tag.add infos_tag ({kind; loc; msg} :: infos) tags in
    Fmt.kstr k fmt

let primary :
  ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a =
  fun ?loc fmt -> detail ?loc Primary fmt

let secondary :
  ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a =
  fun ?loc fmt -> detail ?loc Secondary fmt

let note : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a =
  fun ?loc fmt -> detail ?loc Note fmt

let hint : ?loc:Pos.t -> ('a, Format.formatter, unit, detail) format4 -> 'a =
  fun ?loc fmt -> detail ?loc Hint fmt

let raise_error ?with_pos ?span fmt =
  let k s = err (fun m -> m "@[%a%a%a@]"
      Fmt.(option (Pos.pp ++ any ":@\n")) with_pos
      Fmt.(option (Fmt.string ++ any "@\n")) span
      Fmt.text s
    );
    failwith "error" in
  Fmt.kstr k fmt

let init : ?reporter:Logs.reporter -> unit -> unit =
  fun ?(reporter = Logs_fmt.reporter ()) () ->
    set_reporter reporter
