open Ir

let print_variable (infos : Ast.program_infos) fmt (v : Variable.t) =
  let Variable.{ var_name } = Variable.Map.find v infos.var_info in
  let shape =
    match Variable.Map.find_opt v infos.var_shapes with
    | Some s -> s
    | None -> Context.empty_shape
  in
  Format.fprintf fmt "%s/%d%a"
    var_name v
    (Context.print_shape infos.contexts) shape

let print_literal fmt (l : literal) =
  match l with
  | LInteger i -> Format.pp_print_int fmt i
  | LRational f ->  Format.pp_print_float fmt f
  | LMoney i -> Format.fprintf fmt "%d.%d$" (i/100) (i mod 100)
  | LDate d -> CalendarLib__Printer.Date.fprint "%Y/%m/%d" fmt d
  | LDuration d ->
    let y,m,d = CalendarLib.Date.Period.ymd d in
    Format.fprintf fmt "%d year, %d month, %d day" y m d

let print_view fmt (v : flow_view) =
  match v with
  | AtInstant -> Format.fprintf fmt "instant"
  | Cumulated -> Format.fprintf fmt "total"

let print_binop fmt (op : binop) =
  let op = match op with
    | IAdd -> "+i"
    | RAdd -> "+r"
    | MAdd -> "+m"
    | DAdd -> "+d"
    | DrAdd -> "+dr"
    | ISub -> "-i"
    | RSub -> "-r"
    | MSub -> "-m"
    | DSub -> "-d"
    | DrSub -> "-dr"
    | IMult -> "*i"
    | RMult -> "*r"
    | MMult -> "*m"
    | DrMult -> "*dr"
    | IDiv -> "/i"
    | RDiv -> "/r"
    | MDiv -> "/m"
    | DrDiv -> "/dr"
    | IEq -> "=i"
    | REq -> "=r"
    | MEq -> "=m"
    | DEq -> "=d"
    | DrEq -> "=dr"
  in
  Format.pp_print_string fmt op

let rec print_formula (infos : Ast.program_infos) fmt (f : formula) =
  match f with
  | Literal l -> print_literal fmt l
  | Variable (v, view) ->
    Format.fprintf fmt "[%a]%a"
      print_view view
      (print_variable infos) v
  | Binop (op, f1, f2) ->
    Format.fprintf fmt "@[<hv 2>(%a@ %a %a)@]"
      (print_formula infos) f1
      print_binop op
      (print_formula infos) f2
  | RCast f ->
    Format.fprintf fmt "(cast %a)"
      (print_formula infos) f

let rec print_event (infos : Ast.program_infos) fmt (ev : event) =
  match ev with
  | EvtVar v -> Format.fprintf fmt "Evt %a" (print_variable infos) v
  | EvtAnd (ev1, ev2) ->
    Format.fprintf fmt "@[<hv 2>(%a@ && %a)@]"
      (print_event infos) ev1
      (print_event infos) ev2
  | EvtOr (ev1, ev2) ->
    Format.fprintf fmt "@[<hv 2>(%a@ || %a)@]"
      (print_event infos) ev1
      (print_event infos) ev2
  | EvtCond f
  | EvtDate f -> print_formula infos fmt f

let print_redist (infos : Ast.program_infos) fmt (r : RedistTree.redist) =
  match r with
  | RedistTree.NoInfo -> Format.fprintf fmt "[no info]"
  | RedistTree.Shares sh ->
    Format.fprintf fmt "@[<hv>";
    Variable.Map.iter (fun v s ->
        Format.fprintf fmt "%.2f%% -> %a@ " (s*.100.) (print_variable infos) v)
      sh;
    Format.fprintf fmt "@]"
  | RedistTree.Flats fs ->
    Format.fprintf fmt "@[<hv 2>";
    Variable.Map.iter (fun v f ->
        Format.fprintf fmt "%a -> %a@ "
          (print_formula infos) f
          (print_variable infos) v)
      fs;
    Format.fprintf fmt "@]"

let rec print_tree (infos : Ast.program_infos) fmt (t : RedistTree.tree) =
  match t with
  | RedistTree.Redist r -> (print_redist infos) fmt r
  | RedistTree.Branch (cv, tb, ta) ->
    Format.fprintf fmt "@[<hv 2>branch on %a@ "
      (print_variable infos) cv;
    Format.fprintf fmt "@[<hv 2>before@ %a@]@ "
      (print_tree infos) tb;
    Format.fprintf fmt "@[<hv 2>after@ %a@]@]@ done"
      (print_tree infos) ta

let print_program fmt (p : program) =
  Format.fprintf fmt "@[<v 2>Events:@,";
  Variable.Map.iter (fun v ev ->
      Format.fprintf fmt "@[<hv 2>%a:@ %a@]@,"
        (print_variable p.infos) v
        (print_event p.infos) ev)
    p.events;
  Format.fprintf fmt "@]@;@[<v 2>Trees:@,";
  Variable.Map.iter (fun v t ->
      Format.fprintf fmt "@[<hv 2>%a:@ %a@]@,"
        (print_variable p.infos) v
        (print_tree p.infos) t)
    p.trees;
  Format.fprintf fmt "@."
