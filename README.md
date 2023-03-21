# Continuation Based Semantics: Parser and Interpreter

This project was built as part of Ye Joo Han's undergraduate thesis in Computer Science and Linguistics at Harvard College. The project is an implementation of continuation-based semantics, a semantics theory that was developed by Shan & Barker in their 2006 paper EXPLAINING CROSSOVER AND SUPERIORITY AS LEFT-TO-RIGHT EVALUATION.

The project has the following functionality:
* generate a continuation-based semantics for a plain English sentence
* reduce a continuation-based semantics construct to a lambda expression that represents a sentence
* find all possible derivations for a given sentence

## How to Use
1. Clone the github respository by running `git clone https://github.com/yejoo104/continuation-parser.git` on the terminal or download the ZIP file on github
2. Run the following command on the terminal in order to compile the parser.
```console
make parser
```
3. In order to run the parser, run the following on the terminal and then type a sentence as input.
```console
./parser.byte
```

## Usage
Below are some example inputs and outputs for the parser.
Input: yejoo thought whansung left
```console
(B yejoo (F thought (B whansung left)))
thought(left(whansung))(yejoo)
```

Input: someone saw everyone
```console
(E (S E (M (M B) (L someone) (M (M F) (L (L saw)) (S L everyone)))))
∀x.∃x'.saw(x)(x')
(E (M B someone (M F (L saw) everyone)))
∃x.∀x'.saw(x')(x)
```

Input: everyone loves her mother
```console
(E (S E (M (M B) (L everyone) (M (M F) (L (L loves)) (S L (M B her (L mother)))))))
λl.∀x.love(mother(l))(x)
(E (M B (BIND everyone) (M F (L loves) (M B her (L mother)))))
∀x.love(mother(x))(x)
```

## How to Tweak for Own Use
The current lexicon of this project is fairly limited (around 15 words). One can navigate to `lexicon.ml` and `lexicon.mli` and easily add more words. After adding more words, compiling and running the program locally should allow the user to use the continuation0based parser with a more expansive vocabulary.

## How to Test
If you make additional edits and are unsure of the functionality, you can run a fairly robust test suite which can be found in `test.ml`. New tests can also easily be added to `test.ml` using the existing unit testing infrastructure. In order to run tests locally, run the following commands on terminal.
```console
make test
./test.byte
```

## Ideas? Feedback?
Please feel free to [reach out](mailto:yejoo_han@college.harvard.edu) via email.
