; Mnemonics

(instruction_mnemonic) @function.builtin
(directive_mnemonic) @function.builtin
(control_mnemonic) @keyword

(size) @attribute

(macro_definition name: (symbol)) @function
(macro_call name: (symbol)) @function

(symbol) @variable

(string_literal) @string
(decimal_literal) @number
(hexadecimal_literal) @number
(octal_literal) @number
(binary_literal) @number

(path) @string

(reptn) @variable.builtin
(carg) @variable.builtin
(narg) @variable.builtin

(address_register) @keyword
(data_register) @keyword
(float_register) @keyword
(named_register) @keyword

(interpolated (macro_arg)) @embedded
(macro_arg) @variable.builtin

(comment) @comment

[
  (operator)
  "="
  "#"
] @operator

[
  "."
  ","
  "/"
  "-"
] @punctuation.delimiter

[
  "("
  ")"
  ")+"
] @punctuation.bracket
