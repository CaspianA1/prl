interpret:
	gprolog --query-goal load_parser --query-goal main --consult-file lisp_interpreter.pl
compile:
	gplc -o prl lisp_interpreter.pl lisp_parser.pl
	@echo Once in the top-level, type this to start the interpreter: main.