%% 5.01 (*) Check whether a given term represents a multiway tree
%% Write a predicate istree/1 which succeeds if and only if its argument is a
%% Prolog term representing a multiway tree.
%% Example:
%% ?- istree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
%% Yes
istree(t(X, L)) :-
    atom(X),
    forall(member(T, L), istree(T)).

%% 5.02 (*) Count the nodes of a multiway tree
%% Write a predicate nnodes/1 which counts the nodes of a given multiway tree.
%% Example:
%% ?- nnodes(t(a,[t(f,[])]),N).
%% N = 2
%% Write another version of the predicate that allows for a flow pattern (o,i).
nnodes(t(_, L), N) :-
    nnodes(L, N1),
    N is N1 + 1, !.
nnodes([], 0) :- !.
nnodes([H|T], N) :-
    nnodes(H, N1),
    nnodes(T, N2),
    N is N1 + N2.

%% 5.03 (**) Tree construction from a node string
%% We suppose that the nodes of a multiway tree contain single characters. In the
%% depth-first order sequence of its nodes, a special character ^ has been
%% inserted whenever, during the tree traversal, the move is a backtrack to the
%% previous level.
%% By this rule, the tree in the figure opposite is represented as: afg^^c^bd^e^^^ 
%% Define the syntax of the string and write a predicate tree(String,Tree) to
%% construct the Tree when the String is given. Work with atoms (instead of strings).
%% Make your predicate work in both directions. 
move(X, [X|L]-L).

tree(S, T) :-
    var(T), !,
    atom_chars(S, C),
    tree0(C-[], T).
tree(S, T) :-
    var(S), !,
    tree0(C-[], T),
    atom_chars(S, C).

tree0(C1-C3, t(X, L)) :-
    move(X, C1-C2),
    trees(C2-C3, L).
trees([^|T]-T, []) :- !.
trees(C1-C3, [H|T]) :-
    tree0(C1-C2, H),
    trees(C2-C3, T).

%% 5.04 (*) Determine the internal path length of a tree
%% We define the internal path length of a multiway tree as the total sum of the path
%% lengths from the root to all nodes of the tree. By this definition, the tree in the
%% figure of problem 5.03 has an internal path length of 9. 
%% Write a predicate ipl(Tree,IPL) for the flow pattern (+,-). 
ipl(T, IPL) :-
    ipl(T, 0, IPL).
ipl(t(_, L), Depth, IPL) :-
    length(L, IPL1),
    Depth1 is Depth + 1,
    ipl(L, Depth1, IPL2),
    IPL is Depth1 * IPL1 + IPL2.
ipl([], _, 0) :- !.
ipl([H|T], Depth, IPL) :-
    ipl(H, Depth, IPL1),
    ipl(T, Depth, IPL2),
    IPL is IPL1 + IPL2.