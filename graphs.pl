%% 6.02 (**) Path from one node to another one
%% Write a predicate path(G,A,B,P) to find an acyclic path P from node A to
%% node B in the graph G. The predicate should return all paths via backtracking.
path(G, A, B, P) :-
    path(G, A, B, [A], P0),
    reverse(P0, P).
path(_, A, A, P, P) :- !.
path(G, A, B, P0, P) :-
    neighbour(G, A, N),
    \+ member(N, P0),
    path(G, N, B, [N|P0], P).

neighbour(graph(_, E), A, N) :-
    member(X, E),
    X = e(A, N).

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
    
