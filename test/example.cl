
data Bool = True | False

data Pair b = Pair Int b

data Arrow a b = Arrow (a -> b -> c)

data Arrow a b = Arrow ((a -> b) -> c)

data Arrow a b = Arrow (((Arrow True b) -> b) -> c)

let main : IO 
  = print
  ; a := 10
  | b := [ a b -> 20 ]
  ; c := 1

let foo : IO 
  = a := match a {
    > Cons a b -> foo a b c
    > List -> match b {
        > G -> a := 0 ; b := 1; 10
        > F a b b c -> [ a b -> f a b ] a b
      }
  }
  ; a b c
