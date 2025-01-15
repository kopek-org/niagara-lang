type user_substitution = {
  expr : Equ.expr;
  source : Variable.t;
  condition : Condition.t;
  delta : R.t;
}

type user_substitutions = user_substitution Variable.Map.t

type result = {
  opp_var_info : VarInfo.collection;
  opp_value_eqs : Equ.aggregate_eqs;
  opp_event_eqs : Equ.expr Variable.Map.t;
  opp_pertinence_sets : ProgramInfo.pertinence_set Variable.Map.t;
}

val resolve
  : VarInfo.collection
  -> Equ.aggregate_eqs
  -> Equ.expr Variable.Map.t
  -> user_substitutions Variable.Map.t
  -> Variable.t Variable.Map.t
  -> result
(** [resolve var_info val_eqs evt_eqs user_substs cumulatives] returns
    the updated [var_infos], [val_eqs] and [evt_eqs] with the
    additionnal variables representing alternative computations
    induced by [user_substs]. This parameter is a map from targets of
    opposability to maps of alternative variable affectations (opposed
    percentages). This also ensure alternatif versions of cumulative
    variables given by [cumulatives] map, when relevant. *)
