
type opposed_part = { opp_value : R.t; opp_target : Variable.t }

type opposable_part = R.t * opposed_part list

type part_or_def = Part of opposable_part | Default | Deficit

type 'a share = {
  label : string option;
  dest : Variable.t;
  part : 'a;
  condition : Condition.t;
}

type 'a t = 'a share list

type eqs = part_or_def t Variable.Map.t

type unified_parts = Condition.t R.Map.t

let add_upart (uni : unified_parts) (cond : Condition.t) (part : R.t) =
  R.Map.update part (function
      | None -> Some cond
      | Some ucond -> Some (Condition.disj ucond cond))
    uni

let add_part (rep : unified_parts) (cond : Condition.t) (part : R.t) =
  (* [rep] conditions are always exclusive *)
  let rem_cond, rep =
    R.Map.fold (fun p pc (cond, rep) ->
        let com = Condition.conj cond pc in
        if Condition.is_never com then cond, add_upart rep pc p else
          let rep_ex = Condition.excluded pc com in
          let cond_ex = Condition.excluded cond com in
          let rep = add_upart rep com R.(p+part) in
          let rep =
            if Condition.is_never rep_ex then rep else
              add_upart rep rep_ex p
          in
          cond_ex, rep)
      rep (cond, R.Map.empty)
  in
  if Condition.is_never rem_cond then rep else
    add_upart rep rem_cond part

let default_parts (rep : unified_parts) (def_cond : Condition.t) =
  let defaults, rem_cond, rep =
    R.Map.fold (fun p pc (defs, cond, rep) ->
        let com = Condition.conj cond pc in
        if Condition.is_never com then defs, cond, add_upart rep pc p else
          let cond_ex = Condition.excluded cond com in
          if R.(p >= one) then defs, cond_ex, add_upart rep pc p else
            let rep_ex = Condition.excluded pc com in
            let rep =
              if Condition.is_never rep_ex then rep else
                add_upart rep rep_ex p
            in
            add_upart defs com R.(one - p), cond_ex, add_upart rep pc R.one)
      rep (R.Map.empty, def_cond, R.Map.empty)
  in
  if Condition.is_never rem_cond then defaults, rep else
    add_upart defaults rem_cond R.one,
    add_upart rep rem_cond R.one

let deficit_parts (rep : unified_parts) (def_cond : Condition.t) =
  R.Map.fold (fun p pc (defs, rep) ->
      let com = Condition.conj def_cond pc in
      if Condition.is_never com then defs, add_upart rep pc p else
      if R.(p <= one) then defs, add_upart rep pc p else
        let rep_ex = Condition.excluded pc com in
        let rep =
          if Condition.is_never rep_ex then rep else
            add_upart rep rep_ex p
        in
        add_upart defs com R.(p - one), add_upart rep pc R.one)
    rep (R.Map.empty, R.Map.empty)

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
      | Part (p, _) ->
        let parts = add_part parts share.condition p in
        parts, defs)
    (R.Map.empty, empty_defs) rep

let check_fullness (rep : unified_parts) =
  let parts_cond =
    R.Map.fold (fun p pc cond ->
        let cond = Condition.disj pc cond in
        if R.(p < one) then Errors.raise_error "Pool needs default"
        else if R.(p > one) then Errors.raise_error "Pool needs deficit"
        else cond)
      rep Condition.never
  in
  let rem_cond = Condition.(excluded always parts_cond) in
  if not @@ Condition.is_never rem_cond then
    Errors.raise_error "Pool needs default"

type fullness_result = {
  parts : opposable_part t;
  defaults : unified_parts t;
  deficits : unified_parts share option;
}

let resolve_fullness (rep : part_or_def t) =
  let parts, defs = sort_shares rep in
  let parts, local_defaults =
    List.fold_left (fun (parts, defs) def_share ->
        let ds, parts = default_parts parts def_share.condition in
        let condition =
          R.Map.fold (fun _p -> Condition.disj) ds Condition.never
        in
        let share = { label = def_share.label; dest = def_share.dest; condition; part = ds } in
        parts, share::defs
      )
      (parts, []) defs.local_defaults
  in
  let parts, global_default =
    match defs.global_default with
    | None -> parts, None
    | Some share ->
      let ds, parts = default_parts parts share.condition in
      let condition =
        R.Map.fold (fun _p -> Condition.disj) ds Condition.never
      in
      let gd_share = { label = share.label; dest = share.dest; condition; part = ds } in
      parts, Some gd_share
  in
  let parts, deficit =
    match defs.deficit with
    | None -> parts, None
    | Some share ->
      let ds, parts = deficit_parts parts share.condition in
      let condition =
        R.Map.fold (fun _p -> Condition.disj) ds Condition.never
      in
      let deficit_share = { label = share.label; dest = share.dest; condition; part = ds } in
      parts, Some deficit_share
  in
  check_fullness parts;
  { parts =
      List.filter_map (fun { label; part; dest; condition } ->
          match part with
          | Part part -> Some { label; part; dest; condition }
          | _ -> None)
        rep;
    defaults = (Option.to_list global_default) @ local_defaults;
    deficits = deficit;
  }
