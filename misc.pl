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