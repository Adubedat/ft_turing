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

(**************************************************************************)

let name = "is_palindrome"
let alphabet = ['0'; '1'; '.'; '=']
let blank = '.'
let states = ["scanright"; "lastzero"; "iszero"; "lastone"; "isone"; "scanleft"; "HALT"]
let initial = "scanright"
let finals = ["HALT"]
let transitions = [
    ("scanright",
    [
        {read = '0'; to_state = "lastzero"; write = '.'; action = Right};
        {read = '1'; to_state = "lastone"; write = '.'; action = Right};
        {read = '='; to_state = "HALT"; write = 'y'; action = Right};
    ]);
    ("lastzero",
    [
        {read = '0'; to_state = "lastzero"; write = '0'; action = Right};
        {read = '1'; to_state = "lastzero"; write = '1'; action = Right};
        {read = '='; to_state = "iszero"; write = '.'; action = Left};
    ]);
    ("iszero",
    [
        {read = '0'; to_state = "scanleft"; write = '='; action = Left};
        {read = '1'; to_state = "HALT"; write = 'n'; action = Right};
        {read = '.'; to_state = "HALT"; write = 'y'; action = Right};
    ]);
    ("lastone",
    [
        {read = '0'; to_state = "lastone"; write = '0'; action = Right};
        {read = '1'; to_state = "lastone"; write = '1'; action = Right};
        {read = '='; to_state = "isone"; write = '.'; action = Left};
    ]);
    ("isone",
    [
        {read = '1'; to_state = "scanleft"; write = '='; action = Left};
        {read = '0'; to_state = "HALT"; write = 'n'; action = Right};
        {read = '.'; to_state = "HALT"; write = 'y'; action = Right};
    ]);
    ("scanleft",
    [
        {read = '1'; to_state = "scanleft"; write = '1'; action = Left};
        {read = '0'; to_state = "scanleft"; write = '0'; action = Left};
        {read = '.'; to_state = "scanright"; write = '.'; action = Right};
    ]);
]

(**************************************************************************)

let name = "is_same_power"
let alphabet = ['0'; '1'; '.'; '=']
let blank = '.'
let states = ["scanright"; "lastone"; "isone"; "scanleftzero"; "iszero"; "HALT"]
let initial = "scanright"
let finals = ["HALT"]
let transitions = [
    ("scanright",
    [
        {read = '0'; to_state = "lastone"; write = '.'; action = Right};
    ]);
    ("lastone",
    [
        {read = '0'; to_state = "lastone"; write = '0'; action = Right};
        {read = '1'; to_state = "lastone"; write = '1'; action = Right};
        {read = '='; to_state = "isone"; write = '.'; action = Left};
    ]);
    ("isone",
    [
        {read = '0'; to_state = "HALT"; write = 'n'; action = Right};
        {read = '.'; to_state = "HALT"; write = 'n'; action = Right};
        {read = '1'; to_state = "scanleftzero"; write = '='; action = Left};
    ]);
    ("scanleftzero",
    [
        {read = '1'; to_state = "scanleftzero"; write = '1'; action = Left};
        {read = '0'; to_state = "scanleftzero"; write = '0'; action = Left};
        {read = '.'; to_state = "iszero"; write = '.'; action = Right};
        {read = '='; to_state = "HALT"; write = 'y'; action = Right};
    ]);
    ("iszero",
    [
        {read = '0'; to_state = "lastone"; write = '.'; action = Right};
        {read = '1'; to_state = "HALT"; write = 'n'; action = Right};
        {read = '='; to_state = "HALT"; write = 'y'; action = Right};
    ]);
]
