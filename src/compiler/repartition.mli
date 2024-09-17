type part_kind = Part of R.t | Default | Deficit

type share = {
  dest : Variable.t;
  part : part_kind;
  condition : Condition.t;
}

type t = share list

type fullness_result = {
  parts : t;
  defaults : t;
  deficits : t;
}

val resolve_fullness : t -> fullness_result
