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
    var(S),
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

%% 5.05 (*) Construct the bottom-up order sequence of the tree nodes
%% Write a predicate bottom_up(Tree,Seq) which constructs the bottom-up sequence of the
%% nodes of the multiway tree Tree. Seq should be a Prolog list. 
%% What happens if you run your predicate backwords? 
bottom_up(t(X, L), S) :-
    bottom_up(L, S1),
    append(S1, [X], S).
bottom_up([], []) :- !.
bottom_up([H|T], S) :-
    bottom_up(H, S1),
    bottom_up(T, S2),
    append(S1, S2, S).

bottom_up1(t(X, L), S) :-
    append(S1, [X], S),
    bottom_up2(L, S1).
bottom_up2([], []) :- !.
bottom_up2([H|T], S) :-
    append(S1, S2, S),
    bottom_up1(H, S1),
    bottom_up2(T, S2).

%% 5.06 (**) Lisp-like tree representation
%% There is a particular notation for multiway trees in Lisp. Lisp is a prominent
%% functional programming language, which is used primarily for artificial intelligence
%% problems. As such it is one of the main competitors of Prolog. In Lisp almost
%% everything is a list, just as in Prolog everything is a term. 
%% The following pictures show how multiway tree structures are represented in Lisp. 
%% Note that in the "lispy" notation a node with successors (children) in the tree is
%% always the first element in a list, followed by its children. The "lispy" representation
%% of a multiway tree is a sequence of atoms and parentheses '(' and ')', which we shall
%% collectively call "tokens". We can represent this sequence of tokens as a Prolog list;
%% e.g. the lispy expression (a (b c)) could be represented as the Prolog list
%% ['(', a, '(', b, c, ')', ')']. Write a predicate tree_ltl(T,LTL) which constructs the
%% "lispy token list" LTL if the tree is given as term T in the usual Prolog notation.
%% Example:
%% ?- tree_ltl(t(a,[t(b,[]),t(c,[])]),LTL).
%% LTL = ['(', a, b, c, ')'] 
%% As a second, even more interesting exercise try to rewrite tree_ltl/2 in a way that
%% the inverse conversion is also possible: Given the list LTL, construct the Prolog tree T.
%% Use difference lists. 
tree_ltl(T, LTL) :-
    var(T), !,
    atom_chars(LTL, C),
    tree_ltl0(T, C-[]).
tree_ltl(T, LTL) :-
    var(LTL),
    tree_ltl0(T, C-[]),
    atom_chars(LTL, C).

tree_ltl0(t(X, []), [X|T]-T) :-
    X \= '(', !.
tree_ltl0(t(X, L), C1-C5) :-
    move('(', C1-C2),
    move(X, C2-C3),
    tree_ltl0_f(L, C3-C4),
    move(')', C4-C5), !.
tree_ltl0_f([], C-C).
tree_ltl0_f([H|T], C1-C3) :-
    tree_ltl0(H, C1-C2),
    tree_ltl0_f(T, C2-C3).