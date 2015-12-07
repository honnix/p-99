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

%% 4.07 (**) Construct height-balanced binary trees with a given number of nodes
%% Consider a height-balanced binary tree of height H. What is the maximum number
%% of nodes it can contain?
%% Clearly, MaxN = 2**H - 1. However, what is the minimum number MinN? This question
%% is more difficult.
%% Try to find a recursive statement and turn it into a predicate minNodes/2 defined
%% as follwos:
%% % minNodes(H,N) :- N is the minimum number of nodes in a height-balanced binary
%% tree of height H.
%% (integer,integer), (+,?)
%% On the other hand, we might ask: what is the maximum height H a height-balanced
%% binary tree with N nodes can have?
%% % maxHeight(N,H) :- H is the maximum height of a height-balanced binary tree with
%% N nodes
%% (integer,integer), (+,?)
%% Now, we can attack the main problem: construct all the height-balanced binary trees
%% with a given nuber of nodes.
%% % hbal_tree_nodes(N,T) :- T is a height-balanced binary tree with N nodes.
%% Find out how many height-balanced trees exist for N = 15.
hbal_tree_nodes(0, nil) :- !.
hbal_tree_nodes(N, t(x, L, R)) :-
    N1 is N - 1,
    set_number1(N1, NL, NR),
    hbal_tree_nodes(NL, L),
    hbal_tree_nodes(NR, R).

set_number1(0, 0, 0) :- !.
set_number1(1, 1, 0).
set_number1(1, 0, 1).
set_number1(N, NL, NR) :-
    N0 is ceiling((N + 1) / 2 - 1),
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

%% 4.08 (*) Count the leaves of a binary tree
%% A leaf is a node with no successors. Write a predicate count_leaves/2 to
%% count them. 
%% % count_leaves(T,N) :- the binary tree T has N leaves 
count_leaves(nil, 0) :- !.
count_leaves(t(_, nil, nil), 1) :- !.
count_leaves(t(_, L, R), N) :-
    count_leaves(L, N1),
    count_leaves(R, N2),
    N is N1 + N2.

%% 4.09 (*) Collect the leaves of a binary tree in a list
%% A leaf is a node with no successors. Write a predicate leaves/2 to collect
%% them in a list. 
%% % leaves(T,S) :- S is the list of all leaves of the binary tree T 
leaves(nil, []) :- !.
leaves(t(X, nil, nil), [t(X, nil, nil)]) :- !.
leaves(t(_, L, R), S) :-
    leaves(L, S1),
    leaves(R, S2),
    append(S1, S2, S).

%% 4.10 (*) Collect the internal nodes of a binary tree in a list
%% An internal node of a binary tree has either one or two non-empty successors.
%% Write a predicate internals/2 to collect them in a list. 
%% % internals(T,S) :- S is the list of internal nodes of the binary tree T. 
internals(nil, []) :- !.
internals(t(_, nil, nil), []) :- !.
internals(t(X, L, R), [t(X)|S3]) :-
    internals(L, S1),
    internals(R, S2),
    append(S1, S2, S3).

%% 4.11 (*) Collect the nodes at a given level in a list
%% A node of a binary tree is at level N if the path from the root to the node has
%% length N-1. The root node is at level 1. Write a predicate atlevel/3 to collect
%% all nodes at a given level in a list. 
%% % atlevel(T,L,S) :- S is the list of nodes of the binary tree T at level L
%% Using atlevel/3 it is easy to construct a predicate levelorder/2 which creates
%% the level-order sequence of the nodes. However, there are more efficient ways to
%% do that.
atlevel(nil, _, []) :- !.
atlevel(t(X, _, _), 1, [t(X)]) :- !.
atlevel(t(_, L, R), Level, S) :-
    Level1 is Level - 1,
    atlevel(L, Level1, S1),
    atlevel(R, Level1, S2),
    append(S1, S2, S).

levelorder(T, S) :-
    levelorder0(T, 1, S).

levelorder0(T, L, S) :-
    atlevel(T, L, S1),
    (   S1 \= []
    ->  L1 is L + 1,
        levelorder0(T, L1, S2),
        append(S1, S2, S)
    ;   S = S1
    ).

%% 4.12 (**) Construct a complete binary tree
%% A complete binary tree with height H is defined as follows: The levels 1,2,3,...,H-1
%% contain the maximum number of nodes (i.e 2**(i-1) at the level i, note that we start
%% counting the levels from 1 at the root). In level H, which may contain less than the
%% maximum possible number of nodes, all the nodes are "left-adjusted". This means that
%% in a levelorder tree traversal all internal nodes come first, the leaves come second,
%% and empty successors (the nil's which are not really nodes!) come last.
%% Particularly, complete binary trees are used as data structures (or addressing schemes)
%% for heaps.
%% We can assign an address number to each node in a complete binary tree by enumerating
%% the nodes in levelorder, starting at the root with number 1. In doing so, we realize
%% that for every node X with address A the following property holds: The address of X's
%% left and right successors are 2*A and 2*A+1, respectively, supposed the successors do
%% exist. This fact can be used to elegantly construct a complete binary tree structure.
%% Write a predicate complete_binary_tree/2 with the following specification: 
%% % complete_binary_tree(N,T) :- T is a complete binary tree with N nodes. (+,?)
%% Test your predicate in an appropriate way.
complete_binary_tree(0, nil) :- !.
complete_binary_tree(N, t(x, L, R)) :-
    N1 is N - 1,
    N2 is ceiling(N1 / 2),
    N3 is N1 - N2,
    complete_binary_tree(N2, L),
    complete_binary_tree(N3, R).

%% 4.13 (**) Layout a binary tree (1)
%% Given a binary tree as the usual Prolog term t(X,L,R) (or nil). As a preparation for
%% drawing the tree, a layout algorithm is required to determine the position of each
%% node in a rectangular grid. Several layout methods are conceivable, one of them is
%% shown in the illustration below.
%% In this layout strategy, the position of a node v is obtained by the following two rules:
%% x(v) is equal to the position of the node v in the inorder
%% y(v) is equal to the depth of the node v in the tree
%% sequence
%% In order to store the position of the nodes, we extend the Prolog term representing a
%% node (and its successors) as follows: 
%% % nil represents the empty tree (as usual)
%% % t(W,X,Y,L,R) represents a (non-empty) binary tree with root W "positioned" at (X,Y),
%% and subtrees L and R 
%% Write a predicate layout_binary_tree/2 with the following specification: 
%% % layout_binary_tree(T,PT) :- PT is the "positioned" binary tree obtained from the binary
%% tree T. (+,?)
%% Test your predicate in an appropriate way. 
layout_binary_tree(T, PT) :-
    layout_binary_tree0(T, 1, 0, _, PT).

layout_binary_tree0(nil, _, CurrentOrder, CurrentOrder, nil) :- !.
layout_binary_tree0(t(W, L, R), Depth, CurrentOrder, Order, t(W, X, Depth, PL, PR)) :-
    Depth1 is Depth + 1,
    layout_binary_tree0(L, Depth1, CurrentOrder, Order1, PL),
    X is Order1 + 1,
    layout_binary_tree0(R, Depth1, X, Order, PR).

%% 4.14 (**) Layout a binary tree (2)
%% An alternative layout method is depicted in the above illustration. Find out the rules
%% and write the corresponding Prolog predicate. Hint: On a given level, the horizontal
%% distance between neighboring nodes is constant.
%% Use the same conventions as in problem 4.13 and test your predicate in an appropriate way. 
layout_binary_tree1(T, PT) :-
    distance(T, D0),
    D1 is D0 // 2,
    layout_binary_tree10(T, 1, _, D1, PT).

distance(nil, 1) :- !.
distance(t(_, L, R), Distance) :-
    distance(L, D1),
    distance(R, D2),
    Distance is 2 * max(D1, D2).

layout_binary_tree10(nil, _, 0, _, nil) :- !.
layout_binary_tree10(t(W, L, R), Depth, X, D, t(W, X, Depth, PL, PR)) :-
    Depth1 is Depth + 1,
    D1 is D // 2,
    layout_binary_tree10(L, Depth1, XL, D1, PL),
    X is XL + D1,
    XR is XL + D,
    layout_binary_tree11(R, Depth1, XR, D1, PR).

layout_binary_tree11(nil, _, _, _, nil) :- !.
layout_binary_tree11(t(W, L, R), Depth, X, D, t(W, X, Depth, PL, PR)) :-
    Depth1 is Depth + 1,
    D1 is D // 2,
    XL is X - D1,
    layout_binary_tree11(L, Depth1, XL, D1, PL),
    XR is X + D1,
    layout_binary_tree11(R, Depth1, XR, D1, PR).

%% 4.16 (**) A string representation of binary trees
%% Somebody represents binary trees as strings of the following type (see example):
%% a(b(d,e),c(,f(g,)))
%% a Write a Prolog predicate which generates this string representation, if the tree is given
%% as usual (as nil or t(X,L,R) term). Then write a predicate which does this inverse; i.e.
%% given the string representation, construct the tree in the usual form. Finally, combine the
%% two predicates in a single predicate tree_string/2 which can be used in both directions.
%% b) Write the same predicate tree_string/2 using difference lists and a single predicate
%% tree_dlist/2 which does the conversion between a tree and a difference list in both directions.
%% For simplicity, suppose the information in the nodes is a single letter and there are no spaces
%% in the string. 
tree_stringa(T, S) :-
    var(S), !,
    tree_to_string(T, S).
tree_stringa(T, S) :-
    var(T),
    atom_chars(S, L),
    string_to_tree(L, T).

tree_to_string(nil, '') :- !.
tree_to_string(t(X, nil, nil), X) :- !.
tree_to_string(t(X, L, R), S) :-
    tree_to_string(L, LS),
    tree_to_string(R, RS),
    atomic_list_concat([X, '(', LS, ',', RS, ')'], S).

string_to_tree([], nil) :- !.
string_to_tree([X], t(X, nil, nil)) :- !.
string_to_tree([X,'('|T], t(X, L, R)) :-
    append(List, [')'], T),
    append(LL, [','|RL], List), !,
    string_to_tree(LL, L),
    string_to_tree(RL, R).

tree_stringb(T, S) :-
    var(S),
    tree_dl(T, L-[]), !,
    atom_chars(S, L).
tree_stringb(T, S) :-
    var(T),
    atom_chars(S, L),
    tree_dl(T, L-[]), !.

tree_dl(nil, L-L).
tree_dl(t(X, nil, nil), L1-L) :-
    move(X, L1-L).
tree_dl(t(X, L, R), L1-L7) :-
    move(X, L1-L2),
    move('(', L2-L3),
    tree_dl(L, L3-L4),
    move(',', L4-L5),
    tree_dl(R, L5-L6),
    move(')', L6-L7).

move(X, [X|T]-T).

%% 4.17 (**) Preorder and inorder sequences of binary trees
%% We consider binary trees with nodes that are identified by single lower-case letters, as in
%% the example of problem 4.16.
%% a) Write predicates preorder/2 and inorder/2 that construct the preorder and inorder sequence
%% of a given binary tree, respectively. The results should be atoms, e.g. 'abdecfg' for the
%% preorder sequence of the example in problem 4.16.
%% b) Can you use preorder/2 from problem part a) in the reverse direction; i.e. given a preorder
%% sequence, construct a corresponding tree? If not, make the necessary arrangements.
%% c) If both the preorder sequence and the inorder sequence of the nodes of a binary tree are
%% given, then the tree is determined unambiguously. Write a predicate pre_in_tree/3 that does the job.
%% d) Solve problems a) to c) using difference lists. Cool! Use the predefined predicate time/1 to
%% compare the solutions.
%% What happens if the same character appears in more than one node. Try for instance
%% pre_in_tree(aba,baa,T). 
preorder(nil, '') :- !.
preorder(t(X, L, R), S) :-
    preorder(L, SL),
    preorder(R, SR),
    atomic_list_concat([X,SL,SR], S).

inorder(nil, '') :- !.
inorder(t(X, L, R), S) :-
    inorder(L, SL),
    inorder(R, SR),
    atomic_list_concat([SL,X,SR], S).

pre_in_tree(Pre, In, T) :-
    atom_chars(Pre, PreS),
    atom_chars(In, InS),
    pre_in_tree0(PreS, InS, T).

pre_in_tree0([], [], nil) :- !.
pre_in_tree0([H|T], InS, t(H, Left, Right)) :-
    append(L, [H|R], InS), !,
    length(L, Length),
    split_at(T, Length, T1, T2),
    pre_in_tree0(T1, L, Left),
    pre_in_tree0(T2, R, Right).

split_at(L, 0, [], L) :- !.
split_at([H|T], N, L1, L2) :-
    N1 is N - 1,
    split_at(T, N1, L3, L2),
    L1 = [H|L3].

preorder_dl(T, S) :-
    preorder_dl0(T, L-[]),
    atom_chars(S, L).

preorder_dl0(nil, L-L) :- !.
preorder_dl0(t(X, L, R), S1-S) :-
    move(X, S1-S2),
    preorder_dl0(L, S2-S3),
    preorder_dl0(R, S3-S).

inorder_dl(T, S) :-
    inorder_dl0(T, L-[]),
    atom_chars(S, L).

inorder_dl0(nil, L-L) :- !.
inorder_dl0(t(X, L, R), S1-S) :-
    inorder_dl0(L, S1-S2),
    move(X, S2-S3),
    inorder_dl0(R, S3-S).

pre_in_tree_dl(Pre, In, T) :-
    atom_chars(Pre, PreS),
    atom_chars(In, InS),
    pre_in_tree_dl0(PreS-[], InS-[], T).

pre_in_tree_dl0(Pre-Pre, In-In, nil) :- !.
pre_in_tree_dl0(Pre1-Pre4, In1-In4, t(H, L, R)) :-
    move(H, Pre1-Pre2),
    move(H, In2-In3),
    pre_in_tree_dl0(Pre2-Pre3, In1-In2, L),
    pre_in_tree_dl0(Pre3-Pre4, In3-In4, R).

%% 4.18 (**) Dotstring representation of binary trees
%% We consider again binary trees with nodes that are identified by single lower-case
%% letters, as in the example of problem 4.16. Such a tree can be represented by the
%% preorder sequence of its nodes in which dots (.) are inserted where an empty
%% subtree (nil) is encountered during the tree traversal. For example, the tree shown
%% in problem 4.16 is represented as 'abd..e..c.fg...'. First, try to establish a
%% syntax (BNF or syntax diagrams) and then write a predicate tree_dotstring/2 which
%% does the conversion in both directions. Use difference lists. 
tree_dotstring_dl(T, S) :-
    var(T), !,
    atom_chars(S, L),
    tree_dotstring_dl0(T, L-[]).
tree_dotstring_dl(T, S) :-
    var(S),
    tree_dotstring_dl0(T, L-[]),
    atom_chars(S, L).

tree_dotstring_dl0(nil, ['.'|T]-T) :- !.
tree_dotstring_dl0(t(X, L, R), S1-S4) :-
    move(X, S1-S2),
    tree_dotstring_dl0(L, S2-S3),
    tree_dotstring_dl0(R, S3-S4).

tree_dotstring(T, S) :-
    var(T), !,
    atom_chars(S, L),
    tree_dotstring0(T, L).
tree_dotstring(T, S) :-
    var(S),
    tree_dotstring0(T, L),
    atom_chars(S, L).

tree_dotstring0(nil, ['.'|_]) :- !.
tree_dotstring0(t(H, L, R), [H|T]) :-
    append(T1, T2, T),
    tree_dotstring0(L, T1),
    tree_dotstring0(R, T2), !.