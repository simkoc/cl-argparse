# cl-argparse
A common lisp cmd argument parser inspired by python argparse

## Dependencies

None


## API

`create-main-parser (var &optional description) elements*` => parser

Create a parser with the given name and the variable of name

- var: the variable name for the parser
- description: the description for this parser
- elements: the element forms that add parsing instructions to the parser


`create-sub-parser (name &optional description) elements*` => parser

Create a parser with the given name and the variable of name

- name: the variable name and the name for the parser
- description: the description for this parser
- elements: the element forms that add parsing instructions to the parser


`add-flag parser &key short long help var`

Add a flag to the current parser

- parser: the parser to which to add the flag
- short: the short form (i.e., one character) of the flag
- long: the long form of the flag
- help: the help message to be displayed for this flag
- var: the variable into which the truth value is stored


`add-optional parser &key short long default help var`

Add a parameter to the current parser

- parser: the parser to which to add the parameter
- short: the short form (i.e., one character) of the flag
- long: the long form of the flag
- default: the default value to be used in case the parameter is not used
- help: the help message to be displayed for this flag
- var: the variable into which the value is stored


`add-positional parser &key name help`

Add a positional argument to the parser

- parser: the parser to which to add the positional argument
- name: the name of the positional element which is also the variable name into which the value is stored
- help:  the help message to be displayed for this flag


`add-subparser parser subparser`

adds a subparser to the parser

- parser: the parser to which to add the subparser
- subparser: the subparser to be added


`add-generic-parser parser gen`

adds the parsing elements of the gen parser to the given parser

- parser: the parser which is to be extended
- gen: the parser from which the parser is to be extended

`add-default parser &key var default`

adds a default value to the parser

- parser: the parser to which the default value is added
- var: the variable name to which the default value is bound
- default: the default value


`get-value name parser`

get the cmd provided variable value of name from parser

- name: the name of the variable the value is required
- parser: the parser from which to extract the value


`parse parser argv`

parse the stringlist argv using parser parser

- parser: the parser to be used
- argv: the arguments (i.e., list of strings) to be parsed


`create-mockup-parser key-value-list*`

create a parser simulating a parsing resulting in the given key-value-list

- key-value-list: a paired list of strings to create the parser from

## Usage

You can either use the short example below to get started or the longer example provided as commented source code in `./src/examples/example.lisp`.

### Short Example

```
(in-package :cl-argparse)

(format t "~a~%"
        (parse
         (create-main-parser (main-parser "here comes the description of the program")
           (add-flag main-parser :short "g" :long "goo" :help "this is an example flag" :var "goo")
           (add-optional main-parser :short "t" :long "test" :help "this is an example optional argument" :var "test")
           (add-positional main-parser :name "pos" :help "this is an example for a positional")
           (add-subparser main-parser (create-sub-parser (sub "this is a sub action parser")
                                        (add-positional sub :name "action" :help "this is an example for a positional"))))
         (list "-g" "-t" "alle" "positional" "sub" "theaction")))

=> <PARSER
     goo -> T
     test -> alle
     pos -> positional
     action -> theaction
   >
```
