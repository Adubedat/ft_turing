# ft_turing
Creation of an universal turing machine

We were two on this project.

The aim was to create a turing machine that take a json containing all the instruction for the machine as a first argument.

For example our turing machine can compute fonction like is_palindrome(words that reads the same backwards as forwards), json are listed in `json` directory.

Example: `./turing_machine json/is_palindrome.json "110011="`

Our turing machine is universal because it is able to take an other turing machine as input amd simulate it (`json/universal_tm.json`)

We also have a graphical representation of time complexity for any turing machine computation.

To activate it you have to first install lymp and owl: `opam install lymp && opam install owl`,

then:

* in `Makefile`: uncomment `lymp, owl`

* in `Turing_machine.ml`: uncomment line 203 to the end

* in `main.ml`: comment line 6 / uncomment line 7

Now when you launch `turing_machine` with `-Olr` as first argument you will have an image of time complexity (Big O) in the current directory.

More details in `ft_turing.en.pdf`
