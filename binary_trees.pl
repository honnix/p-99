%% 4.01 (*) Check whether a given term represents a binary tree
%% Write a predicate istree/1 which succeeds if and only if its argument is a Prolog
%% term representing a binary tree.
%% Example:
%% ?- istree(t(a,t(b,nil,nil),nil)).
%% Yes
%% ?- istree(t(a,t(b,nil,nil))).
%% No
istree(nil) :- !.
istree(t(_, L, R)) :-
    istree(L),
    istree(R).

%% 4.02 (**) Construct completely balanced binary trees
%% In a completely balanced binary tree, the following property holds for every node:
%% The number of nodes in its left subtree and the number of nodes in its right subtree
%% are almost equal, which means their difference is not greater than one.
%% Write a predicate cbal_tree/2 to construct completely balanced binary trees for a given
%% number of nodes. The predicate should generate all solutions via backtracking. Put the
%% letter 'x' as information into all nodes of the tree.
%% Example:
%% ?- cbal_tree(4,T).
%% T = t(x, t(x, nil, nil), t(x, nil, t(x, nil, nil))) ;
%% T = t(x, t(x, nil, nil), t(x, t(x, nil, nil), nil)) ;
%% etc......No
cbal_tree(0, nil) :- !.
cbal_tree(N, t(x, L, R)) :-
    N1 is N - 1,
    N2 is N1 // 2,
    N3 is N1 - N2,
    set_number(N2, N3, NL, NR),
    cbal_tree(NL, L),
    cbal_tree(NR, R).

set_number(N, N, N, N) :- !.
set_number(N1, N2, N1, N2).
set_number(N1, N2, N2, N1).

%% 4.03 (**) Symmetric binary trees
%% Let us call a binary tree symmetric if you can draw a vertical line through the root node
%% and then the right subtree is the mirror image of the left subtree. Write a predicate
%% symmetric/1 to check whether a given binary tree is symmetric. Hint: Write a predicate
%% mirror/2 first to check whether one tree is the mirror image of another. We are only
%% interested in the structure, not in the contents of the nodes.
symmetric(nil) :- !.
symmetric(t(_, L, R)) :-
    mirror(L, R).

mirror(nil, nil) :- !.
mirror(t(_, L1, R1), t(_, L2, R2)) :-
    mirror(L1, R2),
    mirror(R1, L2).

%% 4.04 (**) Binary search trees (dictionaries)
%% Use the predicate add/3, developed in chapter 4 of the course, to write a predicate
%% to construct a binary search tree from a list of integer numbers.
%% Example:
%% ?- construct([3,2,5,7,1],T).
%% T = t(3, t(2, t(1, nil, nil), nil), t(5, nil, t(7, nil, nil)))
%% Then use this predicate to test the solution of the problem P56.
%% Example:
%% ?- test_symmetric([5,3,18,1,4,12,21]).
%% Yes
%% ?- test_symmetric([3,2,5,7,4]).
%% No
construct(L, T) :-
    construct0(L, nil, T).

construct0([], T, T) :- !.
construct0([H|T], T1, Tree) :-
    add(H, T1, Tree1),
    construct0(T, Tree1, Tree).

add(V, nil, t(V, nil, nil)) :- !.
add(V, t(V1, L, R), t(V1, T, R)) :-
    V =< V1, !,
    add(V, L, T).
add(V, t(V1, L, R), t(V1, L, T)) :-
    V > V1,
    add(V, R, T).

test_symmetric(L) :-
    construct(L, T),
    symmetric(T).

%% 4.05 (**) Generate-and-test paradigm
%% Apply the generate-and-test paradigm to construct all symmetric, completely balanced
%% binary trees with a given number of nodes. Example:
%% ?- sym_cbal_trees(5,Ts).
%% Ts = [t(x, t(x, nil, t(x, nil, nil)), t(x, t(x, nil, nil), nil)), t(x, t(x, t(x, nil, nil), nil), t(x, nil, t(x, nil, nil)))] 
%% How many such trees are there with 57 nodes? Investigate about how many solutions
%% there are for a given number of nodes? What if the number is even? Write an appropriate predicate.
sym_cbal_trees(N, Ts) :-
    setof(T, (
              cbal_tree(N, T),
              symmetric(T)
             ), Ts).

count_sym_cbal_trees(N, 0) :-
    0 is N mod 2, !.
count_sym_cbal_trees(N, C) :-
    sym_cbal_trees(N, Ts),
    length(Ts, C).

%% 4.06 (**) Construct height-balanced binary trees
%% In a height-balanced binary tree, the following property holds for every node:
%% The height of its left subtree and the height of its right subtree are almost
%% equal, which means their difference is not greater than one.
%% Write a predicate hbal_tree/2 to construct height-balanced binary trees for a
%% given height. The predicate should generate all solutions via backtracking.
%% Put the letter 'x' as information into all nodes of the tree.
%% Example:
%% ?- hbal_tree(3,T).
%% T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), t(x, nil, nil))) ;
%% T = t(x, t(x, t(x, nil, nil), t(x, nil, nil)), t(x, t(x, nil, nil), nil)) ;
%% etc......No
hbal_tree(0, nil) :- !.
hbal_tree(N, t(x, L, R)) :-
    N1 is N - 1,
    set_number(N1, NL, NR),
    hs(NL, L),
    hs(NR, R).

set_number(0, 0, 0) :- !.
set_number(N, N, N).
set_number(N, NL, NR) :-
    NL = N,
    NR is N - 1.
set_number(N, NL, NR) :-
    NR = N,
    NL is N - 1.

%% N is number of nodes. There are duplicates now.
hbal_tree1(0, nil) :- !.
hbal_tree1(N, t(x, L, R)) :-
    N1 is N - 1,
    set_number1(N1, NL, NR),
    hbal_tree1(NL, L),
    hbal_tree1(NR, R).

set_number1(0, 0, 0) :- !.
set_number1(1, 1, 0).
set_number1(1, 0, 1).
set_number1(N, NL, NR) :-
    N0 is N - 1,
    between(1, N0, N1),
    N2 is N - N1,
    celling_log2(N1, 1, C1),
    celling_log2(N2, 1, C2),
    abs(C1 - C2) =< 1,
    (   N1 = N2
    ->  NL = N1,
        NR = N2
    ;   (
         NL = N1, NR = N2;
         NL = N2, NR = N1
        )
    ).

celling_log2(N, C0, C) :-
    (   2 ** C0 < N
    ->  C1 is C0 + 1,
        celling_log2(N, C1, C)
    ;   C = C0
    ).
