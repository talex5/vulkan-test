type side = A | B

let invert = function
  | A -> B
  | B -> A

type 'a t = 'a * 'a

let init f = (f A, f B)

let get (a, b) = function
  | A -> a
  | B -> b

let to_index = function
  | A -> 0
  | B -> 1
