:- use_module(library(yall)).

%% !!
%% graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])
%%
%% We call this graph-term form. Note, that the lists are kept sorted, they
%% are really sets, without duplicated elements. Each edge appears only once
%% in the edge list; i.e. an edge from a node x to another node y is
%% represented as e(x,y), the term e(y,x) is not present. The graph-term form
%% is our default representation. In SWI-Prolog there are predefined
%% predicates to work with sets. 

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
    memberchk(e(A, N), E);
    memberchk(e(N, A), E).

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

%% 6.06 (**) Graph isomorphism
%% Two graphs G1(N1,E1) and G2(N2,E2) are isomorphic if there is a bijection
%% f: N1 -> N2 such that for any nodes X,Y of N1, X and Y are adjacent if
%% and only if f(X) and f(Y) are adjacent.
%% Write a predicate that determines whether two graphs are isomorphic.
%% Hint: Use an open-ended list to represent the function f.
isomorphic(graph(N1, E1), graph(N2, E2), F) :-
    isomorphic0(N1, E1, N2, E2, [], F), !.

isomorphic0([], _, [], _, F, F) :- !.
isomorphic0([H|T], E1, N2, E2, F0, F) :-
    select(P, N2, N21),
    neighbours(H, E1, Neighbours1),
    neighbours(P, E2, Neighbours2),
    remove_edges(H, E1, E11),
    remove_edges(P, E2, E21),
    isomorphic0(Neighbours1, E11, Neighbours2, E21, F0, F1),
    F2 = [H-P|F1],
    %% forest case
    leftover(F2, T, E1, N21, E2, LT, LE1, LN21, LE2),
    isomorphic0(LT, LE1, LN21, LE2, F2, F).

neighbours(P, E, Neighbours) :-
    findall(Neighbour, neighbour(graph(_, E), P, Neighbour), Neighbours).

leftover([], P1, E1, P2, E2, P1, E1, P2, E2) :- !.
leftover([X-Y|T], P1, E1, P2, E2, LP1, LE1, LP2, LE2) :-
    delete(P1, X, P11),
    delete(P2, Y, P21),
    remove_edges(X, E1, E11),
    remove_edges(Y, E2, E21),
    leftover(T, P11, E11, P21, E21, LP1, LE1, LP2, LE2).

remove_edges(P, E0, E) :-
    delete(E0, e(P, _), E1),
    delete(E1, e(_, P), E).

%% 6.07 (**) Node degree and graph coloration
%% a) Write a predicate degree(Graph,Node,Deg) that determines the degree
%%    of a given node.
%% b) Write a predicate that generates a list of all nodes of a graph
%%    sorted according to decreasing degree.
%% c) Use Welch-Powell's algorithm to paint the nodes of a graph in such
%%    a way that adjacent nodes have different colors.
degree(graph(_, E), Node, Deg) :-
    neighbours(Node, E, Neighbours),
    length(Neighbours, Deg).

sort_by_degree(graph(N, E), List) :-
    maplist({N}/[In,Out]>>(degree(graph(N, E), In, Deg), Out = node(In, Deg)),
            N, List0),
    sort(2, '@>=', List0, List1),
    maplist([In,Out]>>(In = node(Out, _)), List1, List).

welsh_powell_coloring(Graph, Colors) :-
    sort_by_degree(Graph, List),
    welsh_powell_coloring0(List, Graph, 1, [], Colors).

welsh_powell_coloring0([], _, _, Colors, Colors) :- !.
welsh_powell_coloring0([H|T], Graph, N, Colors0, Colors) :-
    Colors1 = [H-N|Colors0],
    exclude({Graph, H}/[In]>>neighbour(Graph, H, In), T, T1),
    maplist({N}/[In,Out]>>(Out = In-N), T1, Colors2),
    append(Colors2, Colors1, Colors3),
    subtract(T, T1, T2),
    N1 is N + 1,
    welsh_powell_coloring0(T2, Graph, N1, Colors3, Colors).

%% 6.08 (**) Depth-first order graph traversal
%% Write a predicate that generates a depth-first order graph traversal
%% sequence. The starting point should be specified, and the output
%% should be a list of nodes that are reachable from this starting point
%% (in depth-first order).

traversal(graph(N, E), Start, List) :-
    neighbours(Start, E, Neighbours),
    traversal0(graph(N, E), Neighbours, [Start], List0),
    reverse(List0, List).

traversal0(_, [], List, List) :- !.
traversal0(graph(N, E), [H|T], List0, List) :-
    (   \+ memberchk(H, List0)
    ->  neighbours(H, E, Neighbours),
        traversal0(graph(N, E), Neighbours, [H|List0], List1)
    ;   List1 = List0
    ),
    traversal0(graph(N, E), T, List1, List).

%% 6.09 (**) Connected components
%% Write a predicate that splits a graph into its connected components. 
split(graph(N, E), Components) :-
    split0(N, graph(N, E), [], Components).

split0([], _, Components, Components) :- !.
split0([H|T], Graph, Components0, Components) :-
    traversal(Graph, H, List),
    subtract([H|T], List, Left),
    split0(Left, Graph, [List|Components0], Components).

%% 6.10 (**) Bipartite graphs
%% Write a predicate that finds out whether a given graph is bipartite.

%% 6.11 (***) Generate K-regular simple graphs with N nodes
%% In a K-regular graph all nodes have a degree of K; i.e. the number of
%% edges incident in each node is K. How many (non-isomorphic!)
%% 3-regular graphs with 6 nodes are there? 
%% See also the table of results in p6_11.txt.