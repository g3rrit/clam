mod Main

let a <class a> a b : A 
  = 10

data Bool = True | False

data Pair b 
  = Pair *Int b

data Arrow <class a> a b = Arrow (a -> b -> c)

data Arrow <class a, class b> a b = Arrow ((a -> b) -> c)

data Arrow <class b, class <class, class<class, class>, class<class>> a> a b = Arrow (Arrow True b -> b -> c)

let main <class b, class <class, class<class, class>, class<class>> a> : IO a
  = print
  ; b : A b c = [ a b -> 20 ]
  ; b : B b c = [ a b -> 20 ]
  ; a := 10
  ; e := 1
  ; c := 1

let foo : IO
  = match a 
      | A -> f a
      | B -> b

let foo : IO 
  = a := match a 
    | Cons a b -> foo a b c
    | Bla a b -> a b $ c d
    | List -> (match b 
        | G -> (a := 0 ; b := 1; 10)
        | F a b b c -> [ a b -> f a b ] a b)
    | List -> (match b 
        | G -> (a := 0 ; b := 1; 10)
        | F a b b c -> [ a b -> f a b ] a b)
  ; a b c

let bar : IO
  = a := 10
  ; if a then b else c
  ; b := if (if a then b else c) then 10 else f
  ; 10 
  ; b := 20
  ; c := if a then b else >>
  ; d := a b c
  ; match a | D -> f
  ; match a | D -> g | E -> >>
  ; a := 10
  ; print


let foo : IO
  = a := a b $ c d $ e f g
  ; if a $ b c then b c $ d e else f $ g h
  ; 10
