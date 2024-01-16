open Internal
open Interpreter

let make_input v i = Execution.{
    input_variable = v;
    input_date = Date.Date.mardi_gras 2023; (* irrelevant *)
    input_value = i;
  }

let var : int -> Variable.t = (* carry on... *) Obj.magic

let inputs =
  IntMap.singleton 1 (make_input (var 43) Z.(Ir.LMoney ~$1000000))
  |> IntMap.add 2 (make_input (var 46) Z.(Ir.LMoney ~$600000))
  |> IntMap.add 3 (make_input (var 44) Z.(Ir.LMoney ~$2000000))
  |> IntMap.add 4 (make_input (var 49) Z.(Ir.LMoney ~$8000000))
  |> IntMap.add 5 (make_input (var 42) Z.(Ir.LInteger ~$150000))
  |> IntMap.add 6 (make_input (var 61) Z.(Ir.LMoney ~$1000000))
  |> IntMap.add 7 (make_input (var 54) Z.(Ir.LMoney ~$1200000))
  |> IntMap.add 8 (make_input (var 57) Z.(Ir.LMoney ~$5783824))
  |> IntMap.add 9 (make_input (var 43) Z.(Ir.LMoney ~$1000000))
  |> IntMap.add 10 (make_input (var 44) Z.(Ir.LMoney ~$1000000))

let test p =
  let desc = Interface.description_from_program p in
  let fmt = Format.formatter_of_out_channel stdout in
  Interface.print_program_desc fmt desc;
  Format.fprintf fmt "@.@.";
  Printer.print_intepreter_inputs desc fmt inputs;
  Format.fprintf fmt "@.@.";
  let outputs = Execution.compute_input_lines p inputs in
  Printer.print_intepreter_outputs desc fmt outputs;
  Format.fprintf fmt "@."
