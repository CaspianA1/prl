base_tokenize([], Buffer, [Buffer]).
base_tokenize([Char | Chars], Buffer, Tokens) :-
	(Char = '(' ; Char = ')') ->
		base_tokenize(Chars, '', Tail_Tokens),
		Tokens = [Buffer, Char | Tail_Tokens];

	Char = ' ' ->
		base_tokenize(Chars, '', Tail_Tokens),
		Tokens = [Buffer | Tail_Tokens];

	atom_concat(Buffer, Char, New_Buffer),
	base_tokenize(Chars, New_Buffer, Tokens).

filter_empty_blank([], []).
filter_empty_blank([Head | Tail], Result) :-
	filter_empty_blank(Tail, Tail_Result),
	((Head = [] ; Head = '') ->
		Result = Tail_Result;
		Result = [Head | Tail_Result]).

tokenize(Expr, Tokens) :-
	atom_chars(Expr, Chars),
	base_tokenize(Chars, '', Dirty_Tokens),
	filter_empty_blank(Dirty_Tokens, Tokens).

a_seq([]) --> [].
a_seq([A | Tail]) --> an_atom(A), a_seq(Tail).
a_seq([L | Tail]) --> a_list(L), a_seq(Tail).
a_list(list(L)) --> ['('], a_seq(L), [')'].
an_atom(A) --> [A], {\+ member(A, ['(', ')']), \+ list(A)}.

parse(_, [], []).

parse(Expr, Tokens, AST) :-
	phrase(a_list(AST), Tokens) ;
	phrase(an_atom(AST), Tokens) ;
	format('The following expression is invalid:\n`~w`.\n\n', [Expr]).

establish_types([], []).

establish_types(list([H | T]), list([HT | TT])) :-
	establish_types(H, HT), establish_types(T, TT).

establish_types([H | T], [HT | TT]) :-
	establish_types(H, HT), establish_types(T, TT).

establish_types(A, TA) :-
	catch(number_atom(TA, A), _, TA = A).