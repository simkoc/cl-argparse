# cl-argparse
A common lisp cmd argument parser inspired by python argparse

## Dependencies

None


## API

### `create-parser (name) elements*` => parser

Create a parser with the given name and the variable of name

- name: name of the parser
- elements: the element forms that add parsing instructions to the parser

### `add-flag parser &key short long help var`

Add a flag to the current parser

- parser: the parser to which to add the flag
- short: the short form (i.e., one character) of the flag
- long: the long form of the flag
- help: the help message to be displayed for this flag
- var: the variable into which the truth value is stored


### `add-optional parser &key short long default help var`

Add a parameter to the current parser

- parser: the parser to which to add the parameter
- short: the short form (i.e., one character) of the flag
- long: the long form of the flag
- default: the default value to be used in case the parameter is not used
- help: the help message to be displayed for this flag
- var: the variable into which the value is stored


### `add-positional parser &key name help`

Add a positional argument to the parser

- parser: the parser to which to add the positional argument
- name: the name of the positional element which is also the variable name into which the value is stored
- help:  the help message to be displayed for this flag


### `add-subparser parser subparser`

adds a subparser to the parser

- parser: the parser to which to add the subparser
- subparser: the subparser to be added


### `add-generic-parser parser gen`

adds the parsing elements of the gen parser to the given parser

- parser: the parser which is to be extended
- gen: the parser from which the parser is to be extended


### `get-value name parser`

get the cmd provided variable value of name from parser

- name: the name of the variable the value is required
- parser: the parser from which to extract the value


### `parse parser argv`

parse the stringlist argv using parser parser

- parser: the parser to be used
- argv: the arguments (i.e., list of strings) to be parsed
