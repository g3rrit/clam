
let version_major = "0"
let version_minor = "0"
let version_patch = "0"

let version 
    = String.concat "." [version_major; version_minor; version_patch]

let cc_comp = ref "clang++"
let cc_comp_set =
    [ "clang++"; "g++" ]
let set_cc_comp c 
    = if List.exists (fun s -> c = s) cc_comp_set 
      then cc_comp := c 
      else (Stdio.printf "Invalid cpp compiler (%s)\n" c ; exit 0)

let to_string ()
    = String.concat "\n"
    [ "CONFIG"
    ; "VERSION: " ^ version 
    ; "C++ COMPILER: " ^  !cc_comp
    ]