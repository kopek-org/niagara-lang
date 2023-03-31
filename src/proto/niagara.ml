module StrMap = Map.Make(String)

type flux_ext = string

type thune = int

type ratio = float

let (/$) thune ratio = int_of_float @@ (float_of_int thune) *. ratio

module Flux = struct

  type t =
    | Emission of flux_ext
    | Until of t * thune * t
    | Split of (t * ratio) list

  let emission e = Emission e

  let until before limit after =
    assert (limit > 0);
    Until (before, limit, after)

  let split ratioed default =
    match ratioed with
    | [] -> assert false
    | (_f0, r0)::rfs ->
      let sum_ratios = List.fold_left (fun x (_, y) -> x +. y) r0 rfs in
      assert (0. <= sum_ratios && sum_ratios <= 1.);
      let ratioed = ratioed@[default, (1.-.sum_ratios)] in
      Split (ratioed)

  let rec exists_emissaire prop t =
    match t with
    | Until (bef, _, aft) ->
      exists_emissaire prop bef
      || exists_emissaire prop aft
    | Split rfs ->
      List.exists (fun (rf, _) -> exists_emissaire prop rf) rfs
    | Emission e -> prop e

  let rec eval t input out : thune StrMap.t =
    match t with
    | Until (bef, lim, aft) ->
      if input < lim
      then eval bef input out
      else eval bef lim out
           |> eval aft (input - lim)
    | Split rfs ->
      List.fold_left (fun out (f, ratio) -> eval f (input /$ ratio) out) out rfs
    | Emission e ->
      StrMap.update e (function None -> Some input | Some t -> Some (t + input)) out

end

type level_id =
  | Source of flux_ext
  | Intermediate of flux_ext

type reseau = {
  sources : flux_ext list;
  assignees : flux_ext list;
  levels : (level_id * Flux.t) list;
}

let reseau sources assignees levels =
  let rev_paliers = List.rev levels in
  let all_outputs_defined flux defined =
    not @@ Flux.exists_emissaire
      (fun id ->
         if List.exists (String.equal id) defined
         then false
         else (Printf.eprintf "Error: Undefined output %s\n" id; true))
      flux
  in
  let _ =
    List.fold_left
      (fun seen_outputs (palier_id, flux) ->
         assert (all_outputs_defined flux seen_outputs);
         match palier_id with
         | Intermediate id -> id::seen_outputs
         | Source id ->
           if List.exists (String.equal id) sources
           then seen_outputs
           else (Printf.eprintf "Error: Undeclared source %s\n" id; assert false)
      )
      assignees
      rev_paliers
  in
  { sources; assignees; levels }

type state = {
  inputed : thune StrMap.t;
  outputed : thune StrMap.t;
}

let init_state = { inputed = StrMap.empty; outputed = StrMap.empty }

let eval_with_state reseau inputs state =
  let val_of flux_ext vals =
    match StrMap.find_opt flux_ext vals with
    | Some t -> t
    | None -> 0
  in
  let merged_inputs =
    StrMap.merge
      (fun id v1 v2 ->
         if List.exists (String.equal id) reseau.sources
         then
           match v1, v2 with
           | None, None -> None
           | None, Some v
           | Some v, None -> Some v
           | Some v1, Some v2 -> Some (v1+v2)
         else begin
           Printf.eprintf "Inputed value for %s ignored, %s is not a source.\n%!" id id;
           None
         end
      )
      state.inputed inputs
  in
  let output =
    List.fold_left
      (fun vals (palier_id, flux) ->
         match palier_id with
         | Source id | Intermediate id ->
         let flux_input = val_of id vals in
         Flux.eval flux flux_input vals
      )
      merged_inputs
      reseau.levels
  in
  let outputed =
    StrMap.filter (fun id _ -> List.exists (String.equal id) reseau.assignees) output
  in
  { inputed = merged_inputs; outputed }

let eval reseau inputs = eval_with_state reseau inputs init_state
