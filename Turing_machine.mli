module Turing_machine :
    sig
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
    end
