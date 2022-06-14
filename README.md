# Fuzz-completer

Tidal autonomous agent "Fuzz" is a code generator combined with the autocomplete package. It allows you to produce unique and syntactically correct Tidal code produced by a machine counterpart for use in collaborative improvisation.


## To run

* Git clone this repository to somewhere local on your computer.

* Also add this repository to atom package directory. This can be found in this:

 ` ~/Users/yourname/.atom/packages `

* Install dependencies for the project, by copying and pasting the following command(s) into the terminal and running:

 ` cabal install --lib split scientific random network hosc `

* Open the terminal, cd into this repository>src

* Run terminal command: runhaskell listen.hs

* In atom, run ctl+y h to start

* Type a $ symbol to receive suggestions

* To exit, run ctrl + c in terminal


## To-do

- [ ] Mini-notation tokenisation and inclusion
- [ ] Extend from bi-gram representation to n-gram
- [ ] Develop capabilities for machine evaluation of patterns
- [ ] Develop a machine (artificial) aesthetic to distinguish patterns?
- [ ] Add cabal file for the dependencies

## Citation

@article{wilson2021autonomous,
  title={Autonomous Creation of Musical Pattern from Types and Models in Live Coding},
  author={Wilson, E and Lawson, S and McLean, A and Stewart, J and others},
  year={2021}
}
