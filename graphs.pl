%% 6.02 (**) Path from one node to another one
%% Write a predicate path(G,A,B,P) to find an acyclic path P from node A to
%% node B in the graph G. The predicate should return all paths via backtracking.
path(G, A, B, P) :-
    path(G, A, B, [A], P0),
    reverse(P0, P).
path(_, A, A, P, P) :- !.
path(G, A, B, P0, P) :-
    neighbour(G, A, N),
    \+ memberchk(N, P0),
    path(G, N, B, [N|P0], P).

neighbour(graph(_, E), A, N) :-
    member(e(A, N), E).

%% 6.03 (*) Cycle from a given node
%% Write a predicate cycle(G,A,P) to find a closed path (cycle) P starting at a
%% given node A in the graph G. The predicate should return all cycles via
%% backtracking. 
cycle(G, A, P) :-
    cycle(G, A, A, [A], P0),
    reverse(P0, P).
cycle(G, A, C, P0, P) :-
    neighbour(G, C, N),
    (   N \= A
    ->  cycle(G, A, N, [N|P0], P)
    ;   P = P0
    ).
    
%% 6.04 (**) Construct all spanning trees
%% Write a predicate s_tree(Graph,Tree) to construct (by backtracking) all
%% spanning trees of a given graph. With this predicate, find out how many
%% spanning trees there are for the graph depicted to the left. The data of
%% this example graph can be found in the file p6_04.dat. When you have a
%% correct solution for the s_tree/2 predicate, use it to define two other
%% useful predicates: is_tree(Graph) and is_connected(Graph). Both are
%% five-minutes tasks! 
s_tree(graph([H|T], EG), graph([H|T], ET)) :-
    select_edges(T, EG, ET0),
    sort(ET0, ET).

select_edges([], _, []) :- !.
select_edges(N, EG, [E|T]) :-
    select(E, EG, EG1),
    E = e(P1, P2, _),
    acceptable(N, P1, P2),
    delete(N, P1, N1),
    delete(N1, P2, N2),
    select_edges(N2, EG1, T).

acceptable(N, P1, P2) :-
    memberchk(P1, N),
    \+ memberchk(P2, N), !.
acceptable(N, P1, P2) :-
    \+ memberchk(P1, N),
    memberchk(P2, N).

is_tree(G) :-
    s_tree(G, G), !.
is_connected(G) :-
    s_tree(G, _), !.

%% 6.05 (**) Construct the minimal spanning tree 
%% Write a predicate ms_tree(Graph,Tree,Sum) to construct the minimal
%% spanning tree of a given labelled graph. Hint: Use the algorithm of Prim.
%% A small modification of the solution of 6.04 does the trick. The data of
%% the example graph to the right can be found in the file p6_05.dat.
ms_tree(graph([H|T], EG), graph([H|T], ET), Sum) :-
    sort(3, @=<, EG, EG1),
    select_edges(T, EG1, ET0), !,
    sort(ET0, ET),
    edge_sum(ET, Sum).

edge_sum([], 0) :- !.
edge_sum([e(_, _, V)|T], Sum) :-
    edge_sum(T, Sum1),
    Sum is Sum1 + V.
