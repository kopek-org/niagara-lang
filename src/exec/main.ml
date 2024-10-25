(** Setup logging system *)
let setup_log style_renderer level =
  Fmt_tty.setup_std_outputs ?style_renderer ();
  Logs.set_level level;
  Errors.init ~reporter:(Logs_fmt.reporter ()) ()

(** Setup logging {!Cmdliner} term. *)
let setup_log_term =
  let open Cmdliner in
  Term.(const setup_log $ Fmt_cli.style_renderer () $ Logs_cli.level ())

(** {!Cmdliner} term for the source positional argument. *)
let source_term =
  let open Cmdliner in
  let doc = "The Niagara source file to compile" in
  let docv = "FILE" in
  Arg.(required & pos 0 (some file) None & info ~doc ~docv [])

(** Set GNU style flag. *)
let gnu_style_term =
  let open Cmdliner in
  let doc = "Use GNU style when applicable" in
  Arg.(value & flag & info ~doc ["gnu"])

(** Set test flag. *)
let test_flag_term =
  let open Cmdliner in
  let doc =
    "Awaits for testing inputs on stdin and print interpreter \
     outputs"
  in
  Arg.(value & flag & info ~doc ["test"])

(** The main compilation function. It simply calls the main compilation
    pipepine with a parsing on file. *)
let compile : string -> bool -> unit = fun path test ->
  let src_program = Grammar.ParserMain.parse_program path in
  let p, l = Compiler.Compile.compile src_program in
  (* let filter = Compiler.GenDot.{ *)
  (*   no_filtering with *)
  (*   variable_inclusion = Some (Variable.Set.singleton (Obj.magic 119), DepsOf) *)
  (* } *)
  (* in *)
  (* Compiler.GenDot.dot_of_program p filter; *)
  if test then
    Testing.test_stdin p l

(** [a -+ b] composes the terms [a] and [b] but ignores the
    [a] result. *)
let ( -+ ) : 'a Cmdliner.Term.t -> 'b Cmdliner.Term.t -> 'b Cmdliner.Term.t =
  fun l r ->
    let open Cmdliner.Term in
    const (fun _ r -> r) $ l $ r

(** Main entrypoint. *)
let main () =
  let open Cmdliner in
  let doc = "Niagara compiler" in
  let name = Filename.basename Sys.executable_name in
  let info = Cmd.info ~doc name in
  let cmd = Cmd.v info (Term.(
      setup_log_term -+
      gnu_style_term -+
      ((const compile) $ source_term $ test_flag_term)
      )
  ) in
  let code = Cmd.eval cmd in
  exit code


let () = main ()
