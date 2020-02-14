
data Bool = True | False

data Pair b = Pair Int b

data Arrow a b = Arrow (a -> b -> c)

data Arrow a b = Arrow ((a -> b) -> c)

data Arrow a b = Arrow (((Arrow True b) -> b) -> c)

let main : IO 
  = print
  ; a := 10
  | b := [ a b -> 20 ]
