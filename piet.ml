let () =
  let codel_size = ref 1 in
  let debug = ref false in
  let filename = ref "" in
  Arg.parse
    [ ("-codel-size", Arg.Set_int codel_size, "Codel size in pixel");
      ("-debug", Arg.Set debug, "Enable debug print") ]
    (fun f -> filename := f)
    ("Usage: " ^ Sys.executable_name ^ " [-codel-size 1] [-debug] filename");
  let picture = Picture.load_file ~codel_size:!codel_size !filename in
  Evaluator.run ~debug:!debug picture
