let state_reset_funs = ref []

let register_on_reset f =
  state_reset_funs := f::!state_reset_funs

let reset () = List.iter ((|>) ()) !state_reset_funs
