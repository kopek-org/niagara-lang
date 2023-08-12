
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

(** The main compilation function. It simply calls the main compilation
    pipepine with a parsing on file. *)
let compile : string -> unit = fun path ->
  let src_program = Frontend.ParserMain.parse_program path in
  Frontend.Compile.compile src_program

(** Main entrypoint. *)
let main () =
  let open Cmdliner in
  let doc = "Niagara compiler" in
  let name = Filename.basename Sys.executable_name in 
  let info = Cmd.info ~doc name in
  let cmd = Cmd.v info (Term.(
      (* wrapper to take log setup into account *)
      const (fun _log source -> compile source) $
      setup_log_term $
      source_term)
  ) in
  let code = Cmd.eval cmd in
  exit code


let () = main ()