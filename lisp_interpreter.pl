%%%%%%%%%%
from_env([], N, _) :- format('`~w` is not in the global environment.\n', [N]).
from_env([[N | B]], N, B).
from_env([[N | B] | _], N, B).
from_env([_ | Rest], N, B) :- from_env(Rest, N, B).
%%%%%%%%%%
special_form(define).
special_form(lambda).
special_form(quote).
special_form(if).
special_form(cons).
special_form(car).
special_form(cdr).
special_form('eq?').
special_form('null?').
special_form('atom?').

operator('+').
operator('-').
operator('*').
operator('/').
%%%%%%%%%%
eval_special_form(Env, define, [Name | [Val]], New_Env, []) :-
	eval(Env, Val, _, Evaluated_Val), New_Env = [[Name | Evaluated_Val] | Env].

eval_special_form(_, lambda, [list(Terms), list(Body)], _, [[lambda, Terms, Body]]).

eval_special_form(_, quote, [Val], _, Val).

eval_special_form(Env, if, [Condition, True_Branch, False_Branch], _, Result) :-
	eval(Env, Condition, _, State),
	(State = t -> eval(Env, True_Branch, _, Result);
	eval(Env, False_Branch, _, Result)).
%%%%%%%%%%
eval_special_form(Env, cons, [A, B], _, [EA, EB]) :-
	eval(Env, A, _, EA), eval(Env, B, _, EB).

eval_special_form(Env, car, [Arg], _, Result) :-
	eval(Env, Arg, _, [Result | _]).

eval_special_form(Env, cdr, [Arg], _, Result) :-
	eval(Env, Arg, _, [_ | Result]).
%%%%%%%%%%
eval_special_form(Env, 'eq?', [A1, A2], _, Result) :-
	eval(Env, A1, _, R1), eval(Env, A2, _, R2),
	(R1 =:= R2 -> Result = t; Result = f).

eval_special_form(Env, 'null?', [E], _, Result) :-
	eval(Env, E, _, Eval_E),
	(Eval_E = list([]) -> Result = t; Result = f).

eval_special_form(Env, 'atom?', [A], _, Result) :-
	eval(Env, A, _, Eval_A),
	(([_] = Eval_A ; [_ | _] = Eval_A ; list([]) = Eval_A)
		-> Result = f; Result = t).
%%%%%%%%%%
eval_special_form(Env, Op, [Num | Nums], _, Result) :-
	operator(Op),
	right_associative(Op) ->
		op_foldr(Env, Op, Nums, Num, Result);
	op_foldl(Env, Op, Nums, Num, Result).

right_associative('+').
right_associative('*').

eval_operator(Env, Op, A, B, Result) :-
	eval(Env, A, _, EA),
	eval(Env, B, _, EB),
	C =.. [Op, EA, EB],
	Result is C.

op_foldr(_, _, [], Seed, Seed).
op_foldr(Env, Op, [Head | Tail], Seed, Output) :-
	op_foldr(Env, Op, Tail, Seed, Folded_Tail),
	eval_operator(Env, Op, Head, Folded_Tail, Output).

op_foldl(_, _, [], Seed, Seed).
op_foldl(Env, Op, [Head | Tail], Seed, Output) :-
	eval_operator(Env, Op, Seed, Head, Folded_Head),
	op_foldl(Env, Op, Tail, Folded_Head, Output).
%%%%%%%%%%
load_env([], [], Env, Env).

load_env([Term | Terms], [Value | Values], Env, New_Env) :-
	eval(Env, Value, Env, Eval_Value),
	load_env(Terms, Values, [[Term | Eval_Value] | Env], New_Env).
%%%%%%%%%%
eval(Env, [], Env, []).

eval(Env, list([F | Args]), New_Env, Result) :-
	(special_form(F) ; operator(F)) -> eval_special_form(Env, F, Args, New_Env, Result);

	eval(Env, F, _, Eval_F),

	(Eval_F = [[lambda, Terms, Body]] ->
		load_env(Terms, Args, Env, Temp_Env),
		eval(Temp_Env, list(Body), _, Result);
	format('`~w` is not callable.\n', [F]), Result = []).

eval(Env, Atom, _, Result) :-
	(integer(Atom) ; float(Atom)) -> Result is Atom;
	from_env(Env, Atom, Result).

eval(_, [lambda, Terms, Body], _, [lambda, Terms, Body]).
%%%%%%%%%%
read_input(Output) :-
	get_char(Char),
	(Char = '\n' -> Output = '';
		read_input(Next),
		atom_concat(Char, Next, Output)).

repl_actions(Env, [':debug']) :- trace, repl(Env).
repl_actions(_, [':exit']) :- halt.
repl_actions(_, _).
no_op.
%%%%%%%%%%
repl(Env) :-
	write('☯ > '),
	read_input(Expr),
	tokenize(Expr, Tokens),
	repl_actions(Env, Tokens),
	parse(Expr, Tokens, AST),
	establish_types(AST, TAST),
	eval(Env, TAST, New_Env, Result),
	% format('Env: ~w\nTAST: ~w\n', [New_Env, TAST]),
	(Result \= [] -> format('~w\n', [Result]); no_op),
	(New_Env = Env -> repl(Env) ; repl(New_Env)).

load_parser :- consult(lisp_parser). % for the interpreter
main :- repl([]).
%%%%%%%%%%