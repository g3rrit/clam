
let version_major = "0"
let version_minor = "0"
let version_patch = "0"

let version 
    = String.concat "." [version_major; version_minor; version_patch]

let cpp_compiler = ref "clang++"
let cpp_compiler_set =
    [ "clang++"; "g++" ]
let set_cpp_compiler c 
    = if List.exists (fun s -> c = s) cpp_compiler_set 
      then cpp_compiler := c 
      else (Stdio.printf "Invalid cpp compiler (%s)\n" c ; exit 0)

let to_string ()
    = String.concat "\n"
    [ "CONFIG"
    ; "VERSION: " ^ version 
    ; "C++ COMPILER: " ^  !cpp_compiler
    ]