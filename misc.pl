%% 7.01 (**) Eight queens problem
%% This is a classical problem in computer science. The objective is to place
%% eight queens on a chessboard so that no two queens are attacking each
%% other; i.e., no two queens are in the same row, the same column, or on
%% the same diagonal.
%% Hint: Represent the positions of the queens as a list of numbers 1..N.
%% Example: [4,2,7,3,6,8,5,1] means that the queen in the first column is
%% in row 4, the queen in the second column is in row 2, etc. Use the
%% generate-and-test paradigm.
n_queues(N, Chessboard) :-
    n_queues0([], Chessboard, 0, N).

n_queues0(Chessboard, Chessboard, N, N) :- !.
n_queues0(Chessboard0, Chessboard, M, N) :-
    between(1, N, V),
    column_check(V, Chessboard0),
    diagonal_check(V, Chessboard0, N),
    M1 is M + 1,
    n_queues0([V|Chessboard0], Chessboard, M1, N).

column_check(V, Chessboard) :-
    \+ member(V, Chessboard).

diagonal_check(V, Chessboard, N) :-
    %% go down left
    V1 is V - 1,
    diagonal_check0(left, V1, Chessboard, N),
    %% go down right
    V2 is V + 1,
    diagonal_check0(right, V2, Chessboard, N).

diagonal_check0(_, _, [], _) :- !.
diagonal_check0(left, V, _, _) :-
    V < 1, !.
diagonal_check0(right, V, _, N) :-
    V > N, !.
diagonal_check0(left, V, [H|T], _) :-
    H \= V,
    V1 is V - 1,
    diagonal_check0(left, V1, T, _).
diagonal_check0(right, V, [H|T], N) :-
    H \= V,
    V1 is V + 1,
    diagonal_check0(right, V1, T, N).

%% 7.02 (**) Knight's tour
%% Another famous problem is this one: How can a knight jump on an
%% NxN chessboard in such a way that it visits every square exactly
%% once?
%% Hints: Represent the squares by pairs of their coordinates of
%% the form X/Y, where both X and Y are integers between 1 and N.
%% (Note that '/' is just a convenient functor, not division!) Define
%% the relation jump(N,X/Y,U/V) to express the fact that a knight can
%% jump from X/Y to U/V on a NxN chessboard. And finally, represent
%% the solution of our problem as a list of N*N knight positions
%% (the knight's tour).

knight_tour(N, Path) :-
    knight_tour0([jump(1/1, 1/1)], Path, N).

knight_tour0(Path, Path, N) :-
    Size is N * N,
    length(Path, Size), !.
knight_tour0([jump(From, Last)|T], Path, N) :-
    jump(Last, Next, N),
    Next \= From,
    \+ member(jump(Next, _), T),
    knight_tour0([jump(Last, Next),jump(From, Last)|T], Path, N).

jump(X/Y, U/V, N) :-
    jump_distance(X0, Y0),
    U is X + X0,
    U >= 1, U =< N,
    V is Y + Y0,
    V >= 1, V =< N.

jump_distance(1, 2).
jump_distance(2, 1).
jump_distance(-1, 2).
jump_distance(-2, 1).
jump_distance(-1, -2).
jump_distance(-2, -1).
jump_distance(1, -2).
jump_distance(2, -1).

%% 7.03 (***) Von Koch's conjecture
%% Several years ago I met a mathematician who was intrigued by a
%% problem for which he didn't know a solution. His name was
%% Von Koch, and I don't know whether the problem has been solved
%% since.
%% Anyway, the puzzle goes like this: Given a tree with N nodes
%% (and hence N-1 edges). Find a way to enumerate the nodes from 1
%% to N and, accordingly, the edges from 1 to N-1 in such a way,
%% that for each edge K the difference of its node numbers equals
%% to K. The conjecture is that this is always possible.
%% For small trees the problem is easy to solve by hand. However,
%% for larger trees, and 14 is already very large, it is extremely
%% difficult to find a solution. And remember, we don't know for
%% sure whether there is always a solution!
vkc(t(R, S), N, Solution) :-
    make_range_list(N, [], LN),
    N1 is N - 1,
    make_range_list(N1, [], LE),
    member(X, LN),
    delete(LN, X, LN1),
    vkc(S, X, LN1, LE, [], [], [R-X], Solution),
    write(Solution).

vkc([], _, LN, LE, LN, LE, Solution, Solution) :- !.
vkc([t(K, S)|T], P, LN0, LE0, LN, LE, Solution0, Solution) :-
    member(X, LN0),
    E is abs(X - P),
    member(E, LE0),
    delete(LN0, X, LN1),
    delete(LE0, E, LE1),
    vkc(S, X, LN1, LE1, LN2, LE2, [K-X|Solution0], Solution1),
    vkc(T, P, LN2, LE2, LN, LE, Solution1, Solution).

make_range_list(0, L, L) :- !.
make_range_list(N, L0, L) :-
    L1 = [N|L0],
    N1 is N - 1,
    make_range_list(N1, L1, L).

%% 7.04 (***) An arithmetic puzzle
%% Given a list of integer numbers, find a correct way of inserting
%% arithmetic signs (operators) such that the result is a correct
%% equation. Example: With the list of numbers [2,3,5,7,11] we can
%%form the equations 2-3+5+7 = 11 or 2 = (3*5+7)/11 (and ten others!).
arithmetic_puzzle(L, P) :-
    split(L, [HL|TL], [HR|TR]),
    arithmetic_puzzle(TL, [], HL, R, HL, PL0),
    arithmetic_puzzle(TR, [], HR, R, HR, PR0),
    format(PL0, PL),
    format(PR0, PR),
    atomic_list_concat([PL,'=',PR], P).

arithmetic_puzzle([], [], R, R, P, P) :- !.
arithmetic_puzzle([X], [], Y, R, P0, opt(P0, O, X)) :- !,
    compute(Y, X, O, R).
arithmetic_puzzle([X|T], L, Y, R, P0, P):-
    compute(Y, X, O, R1),
    arithmetic_puzzle(T, L, R1, R, opt(P0, O, X), P).
arithmetic_puzzle([X|T], T, Y, R, P0, opt(P0, O, X)):-
    compute(Y, X, O, R).
arithmetic_puzzle([X|T], L, Y, R, P0, P) :-
    arithmetic_puzzle(T, L0, X, R1, X, P1),
    compute(Y, R1, O, R2),
    arithmetic_puzzle(L0, L, R2, R, opt(P0, O, P1), P).

split(L,L1,L2) :-
    append(L1,L2,L),
    L1 \= [],
    L2 \= [].

compute(X, Y, -, Z) :-
    Z is X - Y.
compute(X, Y, +, Z) :-
    Z is X + Y.
compute(X, Y, *, Z) :-
    Z is X * Y.
compute(X, Y, /, Z) :-
    Y =\= 0,
    Z is X / Y.

format(opt(X, O, Y), P) :- !,
    format(X, P1),
    format(Y, P2),
    atomic_list_concat(['(',P1,O,P2,')'], P).
format(P, P).

%% 7.05 (**) English number words
%% On financial documents, like cheques, numbers must sometimes be
%% written in full words. Example: 175 must be written as
%% one-seven-five. Write a predicate full_words/1 to print
%% (non-negative) integer numbers in full words.
full_words(N) :-
    integer(N),
    N >= 0,
    full_words0(N, '').
full_words0(N, D) :-
    divmod(N, 10, 0, R), !,
    map(R, W),
    writef('%w%w', [W,D]).
full_words0(N, D) :-
    divmod(N, 10, Q, R),
    full_words0(Q, '-'),
    map(R, W),
    writef('%w%w', [W,D]).

map(0, zero).
map(1, one).
map(2, two).
map(3, three).
map(4, four).
map(5, five).
map(6, six).
map(7, seven).
map(8, eight).
map(9, nine).

%%  7.06 (**) Syntax checker
%% In a certain programming language (Ada) identifiers are defined
%% by the syntax diagram (railroad chart) opposite. Transform the
%% syntax diagram into a system of syntax diagrams which do not
%% contain loops; i.e. which are purely recursive. Using these
%% modified diagrams, write a predicate identifier/1 that can
%% check whether or not a given string is a legal identifier.
%% identifier(Str) :- Str is a legal identifier
identifier(S) :-
    atom_chars(S, Chars),
    i_letter(Chars), !.

:- use_module(library(dialect/ifprolog)).

i_letter([]).
i_letter([H|T]) :-
    letter(H),
    (
     i_dash(T);
     i_letter(T);
     i_digit(T)
    ).

i_dash(['-']) :- !, fail.
i_dash(['-'|T]) :-
    (
     i_letter(T);
     i_digit(T)
    ).

i_digit([]) :- !.
i_digit([H|T]) :-
    digit(H),
    (
     i_dash(T);
     i_letter(T);
     i_digit(T)
    ).