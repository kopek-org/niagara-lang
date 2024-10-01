open Equ
open Format

let print_var_info infos fmt (v : Variable.t) =
  let i = Variable.Map.find v infos.Surface.Ast.nvar_info in
  Variable.Info.print fmt i

let print_var_with_info infos fmt (v : Variable.t) =
  Format.fprintf fmt "%d/%a"
    (Variable.uid v)
    (print_var_info infos) v

let rec print_expr fmt (e : expr) =
  match e with
   | EVar v -> fprintf fmt "v%d" (Variable.uid v)
   | EPre v -> fprintf fmt "pre(v%d)" (Variable.uid v)
   | ENot e -> fprintf fmt "!%a" print_expr e
   | EAnd (e1, e2) ->
     fprintf fmt "@[<hov 2>(%a@ && %a@])"
       print_expr e1 print_expr e2
   | EGe (e1, e2) ->
     fprintf fmt "@[<hov 2>(%a@ >= %a@])"
       print_expr e1 print_expr e2
   | EConst l -> Literal.print fmt l
   | ENeg e -> fprintf fmt "(- %a)" print_expr e
   | EInv e -> fprintf fmt "(1 / %a)" print_expr e
   | EAdd (e1, ENeg e2) ->
     fprintf fmt "@[<hov 2>(%a@ - %a@])"
       print_expr e1 print_expr e2
   | EMult (e1, EInv e2) ->
     fprintf fmt "@[<hov 2>(%a@ / %a@])"
       print_expr e1 print_expr e2
   | EAdd (e1, e2) ->
     fprintf fmt "@[<hov 2>(%a@ + %a@])"
       print_expr e1 print_expr e2
   | EMult (e1, e2) ->
     fprintf fmt "@[<hov 2>(%a@ * %a@])"
       print_expr e1 print_expr e2
  | EMerge vs ->
    fprintf fmt "@[<hov 2>merge(%a@])"
      (pp_print_list ~pp_sep:(fun fmt () -> fprintf fmt ",@ ")
         (fun fmt v -> pp_print_int fmt (Variable.uid v)))
      vs

let print_eq infos fmt (var : Variable.t) (ge : guarded_eq option) =
  match ge with
  | Some ge ->
    fprintf fmt "@[<hov 2>%a =%a@ %a@]"
      (print_var_with_info infos) var
      Condition.print ge.eq_act
      print_expr ge.eq_expr
  | None -> fprintf fmt " : no equation@]"

let print_eqs infos fmt (p : program) =
  fprintf fmt "@[<v 2>Equations:@;";
  pp_print_list ~pp_sep:pp_print_cut
    (fun fmt v -> print_eq infos fmt v (Variable.Map.find_opt v p.val_eqs))
    fmt p.val_order;
  fprintf fmt "@;@]@."

let print_inputs fmt (p : program) =
  fprintf fmt "@[<v 2>Inputs:@;";
  Variable.Map.iter
    (fun v i ->
       match i.Variable.Info.kind with
       | ParameterInput { shadow = false }
       | PoolInput { shadow = false } ->
         print_eq p.infos fmt v (Variable.Map.find_opt v p.val_eqs)
       | _ -> ())
    p.infos.Surface.Ast.nvar_info;
  fprintf fmt "@]@."

let print_events fmt (p : program) =
  fprintf fmt "@[<v 2>Events:@;";
  pp_print_list ~pp_sep:pp_print_cut
    (fun fmt v ->
       print_eq p.infos fmt v (Variable.Map.find_opt v p.act_eqs))
    fmt p.act_order;
  fprintf fmt "@]@."


let print_program fmt (p : program) =
  print_inputs fmt p;
  print_events fmt p;
  print_eqs p.infos fmt p;
  pp_print_flush fmt ()
