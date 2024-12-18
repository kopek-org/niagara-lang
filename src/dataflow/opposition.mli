type user_substitutions = Equ.expr Variable.Map.t

type result = {
  var_info : VarInfo.collection;
  value_eqs : Equ.aggregate_eqs;
  event_eqs : Equ.expr Variable.Map.t;
}

val resolve
  : VarInfo.collection
  -> Equ.aggregate_eqs
  -> Equ.expr Variable.Map.t
  -> user_substitutions Variable.Map.t
  -> result
