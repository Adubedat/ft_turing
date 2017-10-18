(* add module sig end if multiples modules *)
type letter
type state
type direction
type transition

val name: string
val alphabet: letter list
val blank: letter
val states: state list
val initial: state
val finals: state list
val transitions: (state * transition list) list
