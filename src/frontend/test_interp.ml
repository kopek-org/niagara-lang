let make_input v i = Interpreter.{
    input_variable = v;
    input_date = Date.Date.mardi_gras 2023; (* irrelevant *)
    input_value = i;
  }

let inputs =
  IntMap.singleton 1 (make_input 43 (Ir.LMoney 1000000))
  |> IntMap.add 2 (make_input 46 (Ir.LMoney 600000))
  |> IntMap.add 3 (make_input 44 (Ir.LMoney 2000000))
  |> IntMap.add 4 (make_input 49 (Ir.LMoney 8000000))
  |> IntMap.add 5 (make_input 42 (Ir.LInteger 150000))
  |> IntMap.add 6 (make_input 61 (Ir.LMoney 1000000))
  |> IntMap.add 7 (make_input 54 (Ir.LMoney 1200000))
  |> IntMap.add 8 (make_input 57 (Ir.LMoney 5783824))
  |> IntMap.add 9 (make_input 43 (Ir.LMoney 1000000))
  |> IntMap.add 10 (make_input 44 (Ir.LMoney 1000000))

let test p =
  let desc = Interface.description_from_program p in
  let fmt = Format.formatter_of_out_channel stdout in
  Interface.print_program_desc fmt desc;
  Format.fprintf fmt "@.";
  Format.fprintf fmt "@.";
  Interface.print_intepreter_inputs desc fmt inputs;
  Format.fprintf fmt "@.";
  Format.fprintf fmt "@.";
  let outputs = Interpreter.compute_input_lines p inputs in
  Interface.print_intepreter_outputs desc fmt outputs;
  Format.fprintf fmt "@."
