type part_or_def = Part of R.t | Default | Deficit

type 'a share = {
  dest : Variable.t;
  part : 'a;
  condition : Condition.t;
}

type 'a t = 'a share list

type eqs = part_or_def t Variable.Map.t

type fullness_result = {
  parts : R.t t;
  defaults : R.t t;
  deficits : R.t t;
}

val resolve_fullness : part_or_def t -> fullness_result
