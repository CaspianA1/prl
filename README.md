## prl

A Lisp interpreter written in GNU Prolog.
- It supports the special forms `define`, `lambda`, `quote`, `if`, `cons`, `car`, `cdr`, `eq?`, `null?`, and `atom?`.
- Numerical support includes `+`, `-`, `*`, and `/`.
- Error checking is present for malformed expressions and unbound symbols.
- To exit the REPL at any time, simply write `:exit` (no parentheses). Use `:debug` to trace a program's execution. Info on using the debugger can be found [here](http://www.gprolog.org/manual/gprolog.html#sec22).
- Prolog is pretty snazzy for writing interpreters, so it's recommended for writing your own!