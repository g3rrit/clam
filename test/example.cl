
data Bool = True | False

data Pair b = Pair Int b

data Arrow a b = Arrow (a -> b -> c)

data Arrow a b = Arrow ((a -> b) -> c)

data Arrow a b = Arrow (Arrow True b -> b -> c)

let main : IO a
  = print
  ; b : A b c = [ a b -> 20 ]
  ; b : B b c = [ a b -> 20 ]
  ; a := 10
  ; e := 1
  ; c := 1

let foo : IO 
  = a := match a {
    | Cons a b -> foo a b c
    | List -> match b {
        | G -> a := 0 ; b := 1; 10
        | F a b b c -> [ a b -> f a b ] a b
      }
  }
  ; a b c

let bar : IO
  = a := 10
  ; b := if (if a then b else c) then 10 else f
  ; 10 
  ; b := 20
  ; c := if a then b else >>
  ; d := 4
  ; match a { | D -> f }


-- let foo : IO
--   = a := a b $ c d $ e f g
--   ; if a $ b c then b c $ d e else (f $ g h)
--  ; 10
