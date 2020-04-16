open Pipe
open Stdio

let () =
    printf "%s\n" "----- CLAM -----"
    ; run [["."; "test"]]