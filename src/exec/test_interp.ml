open Interpreter

let make_input v i = Execution.{
    input_variable = v;
    input_value = i;
  }

let var : int -> Variable.t = (* carry on... *) Obj.magic

let inputs =
  IntMap.singleton 1 (make_input (var 14) Z.(Literal.LMoney ~$1000000))
  |> IntMap.add 2 (make_input (var 54) Z.(Literal.LMoney ~$600000))
  |> IntMap.add 3 (make_input (var 46) Z.(Literal.LMoney ~$2000000))
  |> IntMap.add 4 (make_input (var 60) Z.(Literal.LMoney ~$8000000))
  |> IntMap.add 5 (make_input (var 16) Z.(Literal.LInteger ~$150000))
  |> IntMap.add 6 (make_input (var 87) Z.(Literal.LMoney ~$1000000))
  |> IntMap.add 7 (make_input (var 15) Z.(Literal.LMoney ~$1200000))
  |> IntMap.add 8 (make_input (var 78) Z.(Literal.LMoney ~$5783824))
  |> IntMap.add 9 (make_input (var 14) Z.(Literal.LMoney ~$1000000))
  |> IntMap.add 10 (make_input (var 46) Z.(Literal.LMoney ~$1000000))

let test p l =
  let fmt = Format.formatter_of_out_channel stdout in
  (* Interface.print_program_desc fmt desc; *)
  Format.fprintf fmt "@.@.";
  Format.fprintf fmt "@.@.";
  let outputs = Execution.compute_input_lines p l inputs in
  Printer.print_intepreter_outputs p fmt outputs;
  Format.fprintf fmt "@."
