
type opposed_part = {
  opp_value : R.t;
  opp_target : Variable.t;
  opp_provider : Variable.t
}

type opposable_part = R.t * opposed_part list

type part_or_def =
  | Part of { part : opposable_part; non_opp : bool }
  | Default
  | Deficit

type 'a share = {
  label : string option;
  main_event : VarInfo.event_loc;
  dest : Variable.t;
  part : 'a;
  condition : Condition.t;
}

type 'a t = 'a share list

type eqs = part_or_def t Variable.Map.t

type unified_parts = Condition.t R.Map.t

type fullness_result = {
  parts : opposable_part t;
  non_opp_parts : opposable_part t;
  defaults : unified_parts t;
  deficits : unified_parts share option;
}

type err =
  | ImperfectSum of R.t
  | MultipleDefRep

val resolve_fullness : part_or_def t -> (fullness_result, err) Result.t
