type opposed_part = { opp_value : R.t; opp_target : Variable.t }

type opposable_part = R.t * opposed_part list

type part_or_def = Part of opposable_part | Default | Deficit

type 'a share = {
  dest : Variable.t;
  part : 'a;
  condition : Condition.t;
}

type 'a t = 'a share list

type eqs = part_or_def t Variable.Map.t

type unified_parts = Condition.t R.Map.t

type fullness_result = {
  parts : opposable_part t;
  defaults : unified_parts t;
  deficits : unified_parts share option;
}

val resolve_fullness : part_or_def t -> fullness_result
