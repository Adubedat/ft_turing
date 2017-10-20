type letter = char
type state = string
type direction = Right | Left
type transition = {read: letter; to_state: state; write: letter; action: direction}

val name: string
val alphabet: letter list
val blank: letter
val states: state list
val initial: state
val finals: state list
val transitions: (state * transition list) list

val print_intro: unit
val launch_tape: unit
