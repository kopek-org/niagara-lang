type part_or_def = Part of R.t | Default | Deficit

type 'a share = {
  dest : Variable.t;
  part : 'a;
  condition : Condition.t;
}

type 'a t = 'a share list

let add_part (rep : (Condition.t * R.t) list) (cond : Condition.t) (part : R.t) =
  (* [rep] conditions are always exclusive *)
  let rem_cond, rep =
    List.fold_left (fun (cond, rep) (pc, p) ->
        let com = Condition.conj cond pc in
        if Condition.is_never com then cond, (pc, p)::rep else
          let rep_ex = Condition.excluded pc com in
          let cond_ex = Condition.excluded cond com in
          let rep = (com, R.(p+part))::rep in
          let rep =
            if Condition.is_never rep_ex then rep else
              (rep_ex, p)::rep
          in
          cond_ex, rep)
      (cond, []) rep
  in
  if Condition.is_never rem_cond then rep else
    (rem_cond, part)::rep

let default_parts (rep : (Condition.t * R.t) list) (def_cond : Condition.t) =
  let defaults, rem_cond, rep =
    List.fold_left (fun (defs, cond, rep) (pc, p) ->
        let com = Condition.conj cond pc in
        if Condition.is_never com then defs, cond, (pc, p)::rep else
          let cond_ex = Condition.excluded cond com in
          if R.(p >= one) then defs, cond_ex, (pc, p)::rep else
            let rep_ex = Condition.excluded pc com in
            let rep =
              if Condition.is_never rep_ex then rep else
                (rep_ex, p)::rep
            in
            (com, R.(one - p))::defs, cond_ex, (pc, R.one)::rep)
      ([], def_cond, []) rep
  in
  let rem_def = rem_cond, R.one in
  rem_def::defaults, rem_def::rep

let deficit_parts (rep : (Condition.t * R.t) list) (def_cond : Condition.t) =
  List.fold_left (fun (defs, rep) (pc, p) ->
      let com = Condition.conj def_cond pc in
      if Condition.is_never com then defs, (pc, p)::rep else
      if R.(p <= one) then defs, (pc, p)::rep else
        let rep_ex = Condition.excluded pc com in
        let rep =
          if Condition.is_never rep_ex then rep else
            (rep_ex, p)::rep
        in
        (com, R.(p - one))::defs, (pc, R.one)::rep)
    ([], []) rep

type def_star = {
  global_default : part_or_def share option;
  local_defaults : part_or_def share list;
  deficit : part_or_def share option;
}

let sort_shares (rep : part_or_def t) =
  let empty_defs = {
    global_default = None;
    local_defaults = [];
    deficit = None;
  }
  in
  List.fold_left (fun (parts, defs) share ->
      match share.part with
      | Deficit ->
        let defs =
          { defs with
            deficit =
              match defs.deficit with
              | None -> Some share
              | Some _ -> Errors.raise_error "Cannot have several deficits for a pool"
          }
        in
        parts, defs
      | Default ->
        let defs =
          if Condition.is_always share.condition then
            { defs with
              global_default =
                match defs.global_default with
                | None -> Some share
                | Some _ -> Errors.raise_error "Cannot have several global defaults for a pool"
            }
          else
            match
              List.find_opt
                (fun sh -> not @@ Condition.(is_never (conj share.condition sh.condition)))
                defs.local_defaults
            with
            | Some _ -> Errors.raise_error "Cannot have coexisiting local defaults for a pool"
            | None ->
              { defs with local_defaults = share::defs.local_defaults }
        in
        parts, defs
      | Part p ->
        let parts = add_part parts share.condition p in
        parts, defs)
    ([], empty_defs) rep

let check_fullness (rep : (Condition.t * R.t) list) =
  let parts_cond =
    List.fold_left (fun cond (pc, p) ->
        let cond = Condition.disj pc cond in
        if R.(p < one) then Errors.raise_error "Pool needs default"
        else if R.(p > one) then Errors.raise_error "Pool needs deficit"
        else cond)
      Condition.never rep
  in
  let rem_cond = Condition.(excluded always parts_cond) in
  if not @@ Condition.is_never rem_cond then
    Errors.raise_error "Pool needs default"

type fullness_result = {
  parts : R.t t;
  defaults : R.t t;
  deficits : R.t t;
}

let resolve_fullness (rep : part_or_def t) =
  let parts, defs = sort_shares rep in
  let parts, local_defaults =
    List.fold_left (fun (parts, defs) def_share ->
        let ds, parts = default_parts parts def_share.condition in
        let defs =
          (List.map (fun (condition, p) ->
              { dest = def_share.dest; condition; part = p })
            ds)
          @ defs
        in
        parts, defs
      )
      (parts, []) defs.local_defaults
  in
  let parts, global_default =
    match defs.global_default with
    | None -> parts, []
    | Some share ->
      let ds, parts = default_parts parts share.condition in
      let defs =
        List.map (fun (condition, p) ->
            { dest = share.dest; condition; part = p })
          ds
      in
      parts, defs
  in
  let parts, deficit =
    match defs.deficit with
    | None -> parts, []
    | Some share ->
      let ds, parts = deficit_parts parts share.condition in
      let defs =
        List.map (fun (condition, p) ->
            { dest = share.dest; condition; part = p })
          ds
      in
      parts, defs
  in
  check_fullness parts;
  { parts =
      List.filter_map (fun { part; dest; condition } ->
          match part with
          | Part part -> Some { part; dest; condition }
          | _ -> None)
        rep;
    defaults = global_default @ local_defaults;
    deficits = deficit;
  }
