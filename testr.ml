let name = "unary_sub"
let alphabet = ['1'; '.'; '-'; '=']
let blank = '.'
let states = ["scanright"; "eraseone"; "subone"; "skip"; "HALT"]
let initial = "scanright"
let finals = ["HALT"]
let transitions = [
    ("scanright",
    [
        {read = '.'; to_state = "scanright"; write = '.'; action = Right};
        {read = '1'; to_state = "scanright"; write = '1'; action = Right};
        {read = '-'; to_state = "scanright"; write = '-'; action = Right};
        {read = '='; to_state = "eraseone"; write = '.'; action = Left}
    ]);
    ("eraseone",
    [
        {read = '1'; to_state = "subone"; write = '='; action = Left};
        {read = '-'; to_state = "HALT"; write = '.'; action = Left}
    ]);
    ("subone",
    [
        {read = '1'; to_state = "subone"; write = '1'; action = Left};
        {read = '-'; to_state = "skip"; write = '-'; action = Left}
    ]);
    ("skip",
    [
        {read = '.'; to_state = "skip"; write = '.'; action = Left};
        {read = '1'; to_state = "scanright"; write = '.'; action = Right}
    ])
]

(**************************************************************************)

let name = "unary_add"
let alphabet = ['1'; '.'; '+'; '=']
let blank = '.'
let states = ["scanright"; "addone"; "HALT"]
let initial = "scanright"
let finals = ["HALT"]
let transitions = [
    ("scanright",
    [
        {read = '.'; to_state = "scanright"; write = '.'; action = Right};
        {read = '1'; to_state = "scanright"; write = '1'; action = Right};
        {read = '+'; to_state = "addone"; write = '1'; action = Right};
    ]);
    ("addone",
    [
        {read = '1'; to_state = "addone"; write = '1'; action = Right};
        {read = '.'; to_state = "addone"; write = '.'; action = Right};
        {read = '='; to_state = "eraselast"; write = '.'; action = Left};
    ]);
    ("eraselast",
    [
        {read = '1'; to_state = "HALT"; write = '.'; action = Right};
    ]);
]
