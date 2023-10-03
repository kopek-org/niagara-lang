open Ir
open Surface

let print_variable ~(with_ctx : bool) (infos : Ast.program_infos) fmt (v : Variable.t) =
  let Variable.{ var_name } =
    match Variable.Map.find_opt v infos.var_info with
    | Some i -> i
    | None ->
      Errors.raise_error "(internal) no infos found for var %d"
        (Variable.uid v)
  in
  if with_ctx then
    let shape =
      match Variable.Map.find_opt v infos.var_shapes with
      | Some s -> s
      | None -> Context.empty_shape
    in
    Format.fprintf fmt "@[<hv 2>%s/%d@,%a@]"
      var_name (Variable.uid v)
      (Context.print_shape infos.contexts) shape
  else
    Format.fprintf fmt "@[<hv 2>%s/%d@]" var_name (Variable.uid v)

let print_literal fmt (l : literal) =
  match l with
  | LInteger i -> Format.pp_print_int fmt i
  | LRational f ->  Format.pp_print_float fmt f
  | LMoney i -> Format.fprintf fmt "%d.%d$" (i/100) (i mod 100)
  | LDate d -> CalendarLib.Printer.Date.fprint "%Y/%m/%d" fmt d
  | LDuration d ->
    let y,m,d = Date.Duration.ymd d in
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
  in
  Format.pp_print_string fmt op

let rec print_formula (infos : Ast.program_infos) fmt (f : formula) =
  match f with
  | Literal l -> print_literal fmt l
  | Variable (v, view) ->
    Format.fprintf fmt "[%a]%a"
      print_view view
      (print_variable ~with_ctx:true infos) v
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
  | EvtVar v -> Format.fprintf fmt "Evt %a" (print_variable ~with_ctx:false infos) v
  | EvtOnRaise v -> Format.fprintf fmt "when %a" (print_variable ~with_ctx:false infos) v
  | EvtAnd (ev1, ev2) ->
    Format.fprintf fmt "@[<hv 2>(%a@ && %a)@]"
      (print_event infos) ev1
      (print_event infos) ev2
  | EvtOr (ev1, ev2) ->
    Format.fprintf fmt "@[<hv 2>(%a@ || %a)@]"
      (print_event infos) ev1
      (print_event infos) ev2
  | EvtComp (Eq, f1, f2) ->
    Format.fprintf fmt "@[<hv 2>(%a@ = %a)@]"
      (print_formula infos) f1
      (print_formula infos) f2
  | EvtDate f -> print_formula infos fmt f

let print_redist (type a) (infos : Ast.program_infos) fmt (r : a RedistTree.redist) =
  match r with
  | NoInfo -> Format.fprintf fmt "[no info]"
  | Shares sh ->
    Format.fprintf fmt "@[<hv>";
    Variable.Map.iter (fun v s ->
        Format.fprintf fmt "%.2f%% -> %a@ " (s*.100.) (print_variable ~with_ctx:true infos) v)
      sh;
    Format.fprintf fmt "@]"
  | Flats fs ->
    Format.fprintf fmt "@[<hv 2>";
    Variable.Map.iter (fun v f ->
        Format.fprintf fmt "%a -> %a@ "
          (print_formula infos) f
          (print_variable ~with_ctx:true infos) v)
      fs.transfers;
    Variable.Map.iter (fun v f ->
        Format.fprintf fmt "%.2f%% as deficit -> %a@ "
          (f*.100.)
          (print_variable ~with_ctx:true infos) v)
      fs.balances;
    Format.fprintf fmt "@]"

let rec print_tree : type a. Ast.program_infos -> Format.formatter -> a RedistTree.tree -> unit =
  fun infos fmt t ->
  match t with
  | Nothing -> Format.fprintf fmt "nothing"
  | Redist r -> (print_redist infos) fmt r
  | Branch {evt; before; after} ->
    Format.fprintf fmt "@[<hv 2>branch on %a@ "
      (print_variable ~with_ctx:true infos) evt;
    Format.fprintf fmt "@[<hv 2>before@ %a@]@ "
      (print_tree infos) before;
    Format.fprintf fmt "@[<hv 2>after@ %a@]@]@ done"
      (print_tree infos) after
  | When cr ->
    Format.pp_print_list
      (fun fmt (evt, t) ->
         Format.fprintf fmt "@[<hv 2>when %a@ @[do@ %a@]@]@ done"
           (print_variable ~with_ctx:true infos) evt
           (print_tree infos) t)
      fmt cr

let print_trees (type a) (infos : Ast.program_infos) fmt (ts : a RedistTree.tree list) =
  Format.fprintf fmt "@[<v>%a@]" (Format.pp_print_list (print_tree infos)) ts

let print_balance (infos : Ast.program_infos) fmt (d : RedistTree.frac_balance) =
  match d with
  | BalanceVars { default; deficit } ->
    Format.pp_print_option
      (fun fmt -> Format.fprintf fmt "default -> %a@ " (print_variable ~with_ctx:true infos))
      fmt default;
    Format.pp_print_option
      (fun fmt -> Format.fprintf fmt "deficit <- %a" (print_variable ~with_ctx:false infos))
      fmt deficit
  | BalanceTree t -> print_tree infos fmt t

let print_t (infos : Ast.program_infos) fmt (t : RedistTree.t) =
  match t with
  | Flat fs -> print_trees infos fmt fs
  | Fractions f ->
    Format.fprintf fmt "@[<v>%a@;%a@;%a@]"
      (print_redist infos) f.base_shares
      (print_trees infos) f.branches
      (print_balance infos) f.balance

let rec print_eqex fmt (e : eqex) =
  match e with
  | EZero -> Format.fprintf fmt "0"
  | ESrc -> Format.fprintf fmt "[src]"
  | EConst l -> print_literal fmt l
  | EMult (f, e) -> Format.fprintf fmt "%g*%a" f print_eqex e
  | EAdd (e1, EMinus e2) ->
    Format.fprintf fmt "@[<hv>(%a@ - %a)@]"
      print_eqex e1 print_eqex e2
  | EAdd (e1, e2) ->
    Format.fprintf fmt "@[<hv>(%a@ + %a)@]"
      print_eqex e1 print_eqex e2
  | EMinus e ->
    Format.fprintf fmt "@[<hv>-%a@]" print_eqex e
  | EVar v -> Format.fprintf fmt "v%d" (Variable.uid v)
  | ECurrVar v -> Format.fprintf fmt "v%d'" (Variable.uid v)

let print_cond fmt (cond : cond) =
  match cond with
  | CRef evt -> Format.fprintf fmt "event %d" (Variable.uid evt)
  | CRaising evt -> Format.fprintf fmt "when %d" (Variable.uid evt)
  | CNorm (f, e) ->
    Format.fprintf fmt "@[<hv 1> %g*[src]@ = %a@]"
      f print_eqex e
  | CEq (e1, e2) ->
    Format.fprintf fmt "@[<hv 1>(%a@ = %a)@]"
      print_eqex e1
      print_eqex e2

let rec print_bdd (pp : Format.formatter -> 'a -> unit) fmt (bdd : 'a Variable.BDT.t) =
  match bdd with
  | NoAction -> Format.fprintf fmt "nothing"
  | Action e -> pp fmt e
  | Decision (c, d1, d2) ->
    Format.fprintf fmt "@[<hv>if %d@ then %a@ else %a@]"
      (Variable.uid c) (print_bdd pp) d1 (print_bdd pp) d2

let print_conditions fmt (eqs : event_eq Variable.Map.t) =
  Format.pp_open_vbox fmt 0;
  Variable.Map.iter (fun dest eqs ->
      Format.fprintf fmt "@[<hv 2>eqs %d:@ " (Variable.uid dest);
      Variable.Map.iter (fun src bdd ->
          Format.fprintf fmt "@[<hv 2>from %d:@ %a@],@ "
            (Variable.uid src)
            (print_bdd print_cond) bdd)
        eqs.pinned_src;
      Format.fprintf fmt "from another:@ %a" (print_bdd print_cond) eqs.other_src;
      Format.fprintf fmt "@]@,")
    eqs;
  Format.fprintf fmt "@]@."

let print_program fmt (p : program) =
  Format.fprintf fmt "@[<v 2>Events:@,";
  Variable.Map.iter (fun v ev ->
      Format.fprintf fmt "@[<hv 2>%a:@ %a@]@,"
        (print_variable ~with_ctx:false p.infos) v
        (print_event p.infos) ev)
    p.events;
  Format.fprintf fmt "@]@;@[<v 2>Trees:@,";
  Variable.Map.iter (fun v t ->
      Format.fprintf fmt "@[<hv 2>%a:@ %a@]@,"
        (print_variable ~with_ctx:true p.infos) v
        (print_t p.infos) t)
    p.trees;
  Format.fprintf fmt "@]@;@[<v 2>Equations:@,%a" print_conditions p.equations;
  Format.fprintf fmt "@."

