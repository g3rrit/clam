open Pipe
open Stdio

let arg_spec
    = [ ( "-cpp"
        , Arg.String (fun s -> Config.set_cpp_compiler s)
        , "c++ compiler used"
        )
      ]

let usage_msg
    = "Usage: clam [options] file..."

let () 
    = printf "%s\n" "----- CLAM -----"
    ; let files = ref [] in
      Arg.parse arg_spec (fun s -> files := (String.split_on_char '/' s) :: !files) usage_msg
    ; run !files