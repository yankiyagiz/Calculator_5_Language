:- initialization(main).
:- use_module(library(readutil)).

main :-

    write('Enter an expression type "exit" to quit): '), nl,
    read_line_to_string(user_input, InputStr),
    (InputStr == "exit" ->
        write(''), nl, halt
    ;   string_trim(InputStr, TrimmedStr),
        (TrimmedStr == "" ->
            nl, main
        ;   atom_string(InputAtom, TrimmedStr),
            catch(
                (atom_chars(InputAtom, Chars),
                 phrase(expression(Expr), Chars),
                 eval(Expr, [], Result),
                 write('Result: '), write(Result), nl),
                Error,
                (write('Error encountered: '), write(Error), nl, fail)
            ),
            nl, main
        )
    ).

string_trim(Str, Trimmed) :-

    atom_string(AStr, Str),
    atom_codes(AStr, Codes),
    trim_whitespace(Codes, TrimmedCodes),
    atom_codes(TrimmedAtom, TrimmedCodes),
    atom_string(TrimmedAtom, Trimmed).

trim_whitespace([], []).

trim_whitespace([C|Rest], Trimmed) :-

    (char_type(C, space) -> trim_whitespace(Rest, Trimmed) ; trim_leading(Rest, [C], Trimmed)).

trim_leading([], Acc, Acc).

trim_leading([C|Rest], Acc, Trimmed) :-

    (char_type(C, space) -> trim_leading(Rest, Acc, Trimmed) ; trim_leading(Rest, [C|Acc], Trimmed)).

expression(Expr) --> term(T), expression_tail(T, Expr).

expression_tail(T, Expr) -->
    ['+'], term(T2),
    { Expr = T + T2 }.

expression_tail(T, Expr) -->
    ['-'], term(T2),
    { Expr = T - T2 }.

expression_tail(T, T) --> [].

term(T) --> factor(F), term_tail(F, T).

term_tail(F, T) -->
    ['*'], factor(F2),
    { T = F * F2 }.

term_tail(F, T) -->
    ['/'], factor(F2),
    { T = F / F2 }.

term_tail(F, F) --> [].

factor(N) --> number(N).
factor(V) --> variable(V).
factor(E) --> ['('], expression(E), [')'].

number(N) --> 
    digits(Ds),
    { number_chars(N, Ds) }.

digits([D|Ds]) -->
    [D], { char_type(D, digit) },
    digits(Ds).
digits([D]) -->
    [D], { char_type(D, digit) }.

variable(V) -->
    [C], { char_type(C, alpha) },
    variable_tail([C], V).

variable_tail(Chars, V) -->
    [C], { char_type(C, alnum) },
    variable_tail([C|Chars], V).
    
variable_tail(Chars, V) -->
    { reverse(Chars, CharsRev),
      atom_chars(V, CharsRev) }.


eval(N, _, N) :- number(N).

eval(V, Env, Value) :- atom(V), lookup(V, Env, Value).

eval(A + B, Env, Value) :-
    eval(A, Env, VA), eval(B, Env, VB),
    Value is VA + VB.

eval(A - B, Env, Value) :-
    eval(A, Env, VA), eval(B, Env, VB),
    Value is VB - VA.

eval(A * B, Env, Value) :-
    eval(A, Env, VA), eval(B, Env, VB),
    Value is VA * VB.

eval(A / B, Env, Value) :-
    eval(A, Env, VA), eval(B, Env, VB),
    (VB =\= 0 -> Value is VB / VA ; throw(error(division_by_zero))).


lookup(Var, [(Var,Value)|_], Value).
lookup(Var, [_|Rest], Value) :- lookup(Var, Rest, Value).