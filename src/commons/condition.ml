
type input_presence =
  | Present of Variable.t
  | Absent of Variable.Set.t

type event_presence = bool Variable.Map.t

type conj = {
  input : input_presence;
  events : event_presence;
}

type disj = conj list

exception Contradiction

let canon_conj_input i1 i2 =
  match i1, i2 with
  | Present v1, Present v2 ->
    if Variable.equal v1 v2 then i1 else raise Contradiction
  | Absent s1, Absent s2 -> Absent (Variable.Set.union s1 s2)
  | Present v, Absent s
  | Absent s, Present v ->
    if Variable.Set.mem v s
    then raise Contradiction
    else Present v

let disj_input i1 i2 =
  match i1, i2 with
  | Present v1, Present v2 ->
    if Variable.equal v1 v2 then [], Some i1, []
    else [i1], None, [i2]
  | Present v, Absent s ->
    if Variable.Set.mem v s then [i1],None,[i2] else
      [], Some (Present v), [Absent (Variable.Set.add v s)]
  | Absent s, Present v ->
    if Variable.Set.mem v s then [i1], None, [i2] else
      [Absent (Variable.Set.add v s)], Some (Present v), []
  | Absent s1, Absent s2 ->
    let us = Variable.Set.union s1 s2 in
    let ex_present s =
      Variable.Set.fold (fun v l-> Present v::l)
        (Variable.Set.diff us s) []
    in
    ex_present s1, Some (Absent us), ex_present s2

let conj_event e1 e2 : event_presence =
  Variable.Map.union
    (fun _v p1 p2 ->
       if p1 = p2 then Some p1 else raise Contradiction)
    e1 e2

let disj_event e1 e2 =
  match conj_event e1 e2 with
  | exception Contradiction -> [e1], None, [e2]
  | conj ->
    let conjc = Variable.Map.cardinal conj in
    let conj_excl e =
      if Variable.Map.cardinal e = conjc then [] else
        let lm =
          Variable.Map.fold (fun v pc lm ->
              match Variable.Map.find_opt v e with
              | Some _ ->
                List.map (Variable.Map.add v pc) lm
              | None ->
                (List.map (Variable.Map.add v pc) lm)
                @(List.map (Variable.Map.add v (not pc)) lm)
            )
            conj [Variable.Map.empty]
        in
        match lm with
        | [] -> assert false
        | _conj_case::others -> others
    in
    conj_excl e1, Some conj, conj_excl e2

let conj_conj c1 c2 =
  let input = canon_conj_input c1.input c2.input in
  let events = conj_event c1.events c2.events in
  { input; events }

let disj_conj c1 c2 =
  let ex_i1, com_i, ex_i2 = disj_input c1.input c2.input in
  match com_i with
  | None -> [c1],[],[c2]
  | Some com_i ->
    let ex_e1, com_e, ex_e2 = disj_event c1.events c2.events in
    match com_e with
    | None -> [c1],[],[c2]
    | Some com_e ->
      let conj input events =
        match input with
        | Absent s ->
          if Variable.Set.is_empty s && Variable.Map.is_empty events
          then None else Some { input; events }
        | _ -> Some { input; events }
      in
      let disj_ex1 =
        List.map (fun input -> {c1 with input}) ex_i1
        @ List.filter_map (conj com_i) ex_e1
      in
      let disj_com =
        Option.to_list (conj com_i com_e)
      in
      let disj_ex2 =
        List.map (fun input -> {c2 with input}) ex_i2
        @ List.filter_map (conj com_i) ex_e2
      in
      (disj_ex1, disj_com, disj_ex2)

let conj_disj d1 d2 =
  List.fold_left (fun conj c2 ->
      let cd1 =
        List.filter_map (fun c1 ->
            match conj_conj c1 c2 with
            | exception Contradiction -> None
            | conj -> Some conj)
          d1
      in
      cd1@conj)
    [] d2

let disj_disj d1 d2 =
  let rec iter_d1 exd1 com exd2 rd1 rd2 =
    match rd2 with
    | [] -> (rd1@exd1), com, exd2, rd2
    | c2::rd2 ->
      match rd1 with
      | [] ->
        exd1, com, (c2::exd2), rd2
      | c1::rd1 ->
        let exc1, ccom, exc2 = disj_conj c1 c2 in
        match ccom with
        | [] ->
          iter_d1 (exc1@exd1) com exd2 rd1 (exc2@rd2)
        | _ ->
          (exc1@exd1@rd1), (ccom@com), exd2, (exc2@rd2)
  in
  let rec iter_d2 com exd2 rd1 rd2 =
    match rd2 with
    | [] -> rd1, com, exd2
    | rd2 ->
      let exd1, com, exd2, rd2 = iter_d1 [] com exd2 rd1 rd2 in
      iter_d2 com exd2 exd1 rd2
  in
  iter_d2 [] [] d1 d2

let print_conj fmt { input; events } =
  begin match input with
    | Present v -> Format.fprintf fmt "%d@ /\\ " (Variable.uid v)
    | Absent a ->
      if Variable.Set.is_empty a then Format.fprintf fmt "T@ /\\ " else
      Format.fprintf fmt "@[<hov 2>!(%a@,@])@ /\\ "
        (Format.pp_print_list
           ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ \\/ ")
           (fun fmt v -> Format.pp_print_int fmt (Variable.uid v)))
        (Variable.Set.elements a)
  end;
  if Variable.Map.is_empty events then Format.fprintf fmt "T" else
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ /\\ ")
      (fun fmt (v, p) ->
         if p then Format.pp_print_int fmt (Variable.uid v) else
           Format.fprintf fmt "!%d" (Variable.uid v))
      fmt
      (Variable.Map.bindings events)

let print_disj fmt t =
  match t with
  | [] -> Format.fprintf fmt "never"
  | [{ input = Absent a; events = e }]
    when Variable.Set.is_empty a && Variable.Map.is_empty e ->
    Format.fprintf fmt "always"
  | l ->
    Format.pp_print_list
      ~pp_sep:(fun fmt () -> Format.fprintf fmt "@ \\/ ")
      (fun fmt conj -> Format.fprintf fmt "@[<hov 2>(%a@,@])" print_conj conj)
      fmt
      l

type t = disj

let never = []

let always = [{ input = Absent Variable.Set.empty; events = Variable.Map.empty }]

let is_never t = t == never

let is_always t = let _,_,exa = disj_disj t always in is_never exa

let of_input v = [{ input = Present v; events = Variable.Map.empty }]

let of_event v p = [{ input = Absent Variable.Set.empty; events = Variable.Map.singleton v p }]

let conj = conj_disj

let disj t1 t2 = let a,b,c = disj_disj t1 t2 in a@b@c

let excluded t1 t2 = let ex1,_,_ = disj_disj t1 t2 in ex1

let print = print_disj
