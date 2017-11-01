val name: string
val alphabet: Parsing.letter list
val blank: Parsing.letter
val states: Parsing.state list
val initial: Parsing.state
val finals: Parsing.state list
val transitions: (Parsing.state * Parsing.transition list) list

val print_intro: unit -> unit
val launch_tape: unit -> unit
val linear_regression: unit -> unit
