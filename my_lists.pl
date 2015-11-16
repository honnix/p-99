:- module(my_lists, []).

%% 1.01 (*) Find the last element of a list.
%% Example:
%% ?- my_last(X,[a,b,c,d]).
%% X = d
my_last(X, [X|[]]) :- !.
my_last(X, [_|T]) :-
    my_last(X, T).

%% 1.02 (*) Find the last but one element of a list.
%% (de: zweitletztes Element, fr: avant-dernier élément)
last_but_one(X, [X|[_]]) :- !.
last_but_one(X, [_|T]) :-
    last_but_one(X, T).

%% 1.03 (*) Find the K'th element of a list.
%% The first element in the list is number 1.
%% Example:
%% ?- element_at(X,[a,b,c,d,e],3).
%% X = c
element_at(X, [X|_], 1) :- !.
element_at(X, [_|T], N) :-
    N1 is N - 1,
    element_at(X, T, N1).

%% 1.04 (*) Find the number of elements of a list.
my_length([], 0) :- !.
my_length([_|T], Length) :-
    my_length(T, Length0),
    Length is Length0 + 1.

%% 1.05 (*) Reverse a list.
reverse(L1, L2) :-
    reverse0(L1, L2, []).
reverse0([], L, L) :- !.
reverse0([H|T], L, L0) :-
    L1 = [H|L0],
    reverse0(T, L, L1).

%% 1.06 (*) Find out whether a list is a palindrome.
%% A palindrome can be read forward or backward; e.g. [x,a,m,a,x].
palindrome(L) :-
    reverse(L, L).

%% 1.07 (**) Flatten a nested list structure.
%% Transform a list, possibly holding lists as elements into a 'flat' list by replacing each list with its elements (recursively).
%% Example:
%% ?- my_flatten([a, [b, [c, d], e]], X).
%% X = [a, b, c, d, e]
%% Hint: Use the predefined predicates is_list/1 and append/3
my_flatten(L1, L2) :-
    is_list(L1),
    my_flatten0(L1, L2).
my_flatten0(X, [X]) :-
    \+ is_list(X), !.
my_flatten0([], []) :- !.
my_flatten0([H|T], L) :-
    my_flatten0(H, L1),
    my_flatten0(T, L2),
    append(L1, L2, L).

%% 1.08 (**) Eliminate consecutive duplicates of list elements.
%% If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.
%% Example:
%% ?- compress([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% X = [a,b,c,a,d,e]
compress([], []) :- !.
compress([H|[H|T]], L) :- !,
    compress([H|T], L).
compress([H|T], L) :-
    compress(T, L0),
    L = [H|L0].

%% 1.09 (**) Pack consecutive duplicates of list elements into sublists.
%% If a list contains repeated elements they should be placed in separate sublists.
%% Example:
%% ?- pack([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% X = [[a,a,a,a],[b],[c,c],[a,a],[d],[e,e,e,e]]
pack([], []) :- !.
pack([H|T], L) :- !,
    sub(H, T, T1, L1),
    pack(T1, L2),
    L = [L1|L2].

sub(X, [X|T], T1, L) :- !,
    sub(X, T, T1, L1),
    L = [X|L1].
sub(X, T, T, [X]).

%% 1.10 (*) Run-length encoding of a list.
%% Use the result of problem 1.09 to implement the so-called run-length encoding data compression method.
%% Consecutive duplicates of elements are encoded as terms [N,E] where N is the number of duplicates of the element E.
%% Example:
%% ?- encode([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% X = [[4,a],[1,b],[2,c],[2,a],[1,d][4,e]]
encode(L1, L2) :-
    pack(L1, L3),
    encode0(L3, L2).

encode0([], []) :- !.
encode0([H|T], L) :-
    encode0(T, L1),
    H = [X|_],
    my_length(H, Length),
    L = [[Length,X]|L1].

%% 1.11 (*) Modified run-length encoding.
%% Modify the result of problem 1.10 in such a way that if an element has no duplicates it is simply
%% copied into the result list. Only elements with duplicates are transferred as [N,E] terms.
%% Example:
%% ?- encode_modified([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_modified(L1, L2) :-
    pack(L1, L3),
    encode_modified0(L3, L2).

encode_modified0([], []) :- !.
encode_modified0([[X]|T], L) :- !,
    encode_modified0(T, L1),
    L = [X|L1].
encode_modified0([H|T], L) :- !,
    encode_modified0(T, L1),
    H = [X|_],
    my_length(H, Length),
    L = [[Length,X]|L1].

%% 1.12 (**) Decode a run-length encoded list.
%% Given a run-length code list generated as specified in problem 1.11. Construct its uncompressed version.
decode([], []) :- !.
decode([[N,X]|T], L) :- !,
    decode(T, L1),
    length(L2, N),
    findall(X, member(X, L2), L2),
    append(L2, L1, L).
decode([H|T], L) :- !,
    decode(T, L1),
    L = [H|L1].

%% 1.13 (**) Run-length encoding of a list (direct solution).
%% Implement the so-called run-length encoding data compression method directly.
%% I.e. don't explicitly create the sublists containing the duplicates, as in problem 1.09,
%% but only count them. As in problem 1.11, simplify the result list by replacing the singleton terms [1,X] by X.
%% Example:
%% ?- encode_direct([a,a,a,a,b,c,c,a,a,d,e,e,e,e],X).
%% X = [[4,a],b,[2,c],[2,a],d,[4,e]]
encode_direct([], []) :- !.
encode_direct([H|T], L) :-
    count(1, H, T, T1, X),
    encode_direct(T1, L1),
    L = [X|L1].

count(N, X, [X|T], T1, Y) :- !,
    N1 is N + 1,
    count(N1, X, T, T1, Y).
count(1, X, T, T, X) :- !.
count(N, X, T, T, [N,X]).

%% 1.14 (*) Duplicate the elements of a list.
%% Example:
%% ?- dupli([a,b,c,c,d],X).
%% X = [a,a,b,b,c,c,c,c,d,d]
dupli([], []) :- !.
dupli([H|T], L) :-
    dupli(T, L1),
    L = [H,H|L1].

%% 1.15 (**) Duplicate the elements of a list a given number of times.
%% Example:
%% ?- dupli([a,b,c],3,X).
%% X = [a,a,a,b,b,b,c,c,c]
%% What are the results of the goal:
%% ?- dupli(X,3,Y).
dupli([], _, []) :- !.
dupli([H|T], N, L) :-
    dupli(T, N, L1),
    length(L2, N),
    findall(H, member(H, L2), L2),
    append(L2, L1, L).

%% 1.16 (**) Drop every N'th element from a list.
%% Example:
%% ?- drop([a,b,c,d,e,f,g,h,i,k],3,X).
%% X = [a,b,d,e,g,h,k]
drop(L1, N, L2) :-
    drop0(L1, N, N, L2).

drop0([], _, _, []) :- !.
drop0([_|T], N, 1, L) :- !,
    drop0(T, N, N, L).
drop0([H|T], N, N1, L) :-
    N2 is N1 - 1,
    drop0(T, N, N2, L1),
    L = [H|L1].

%% 1.17 (*) Split a list into two parts; the length of the first part is given.
%% Do not use any predefined predicates.
%% Example:
%% ?- split([a,b,c,d,e,f,g,h,i,k],3,L1,L2).
%% L1 = [a,b,c]
%% L2 = [d,e,f,g,h,i,k]
split(L, 0, [], L) :- !.
split([H|T], N, L1, L2) :-
    N1 is N - 1,
    split(T, N1, L3, L2),
    L1 = [H|L3].

%% 1.18 (**) Extract a slice from a list.
%% Given two indices, I and K, the slice is the list containing the elements between the I'th and K'th element of the original list (both limits included). Start counting the elements with 1.
%% Example:
%% ?- slice([a,b,c,d,e,f,g,h,i,k],3,7,L).
%% X = [c,d,e,f,g]
slice(L1, I, K, L2) :-
    slice0(L1, I, K, 1, L2).
slice0([H|_], _, K, K, [H]) :- !.
slice0([H|T], I, K, X, L) :-
    X >= I, !,
    X1 is X + 1,
    slice0(T, I, K, X1, L1),
    L = [H|L1].
slice0([_|T], I, K, X, L) :-
    X1 is X + 1,
    slice0(T, I, K, X1, L).

%% 1.19 (**) Rotate a list N places to the left.
%% Examples:
%% ?- rotate([a,b,c,d,e,f,g,h],3,X).
%% X = [d,e,f,g,h,a,b,c]
%% ?- rotate([a,b,c,d,e,f,g,h],-2,X).
%% X = [g,h,a,b,c,d,e,f]
%% Hint: Use the predefined predicates length/2 and append/3, as well as the result of problem 1.17.
rotate(L1, N, L2) :-
    my_length(L1, Length),
    Length1 is (Length + N) mod Length,
    split(L1, Length1, L3, L4),
    append(L4, L3, L2).

%% 1.20 (*) Remove the K'th element from a list.
%% Example:
%% ?- remove_at(X,[a,b,c,d],2,R).
%% X = b
%% R = [a,c,d]
remove_at(H, [H|T], 1, T) :- !.
remove_at(X, [H|T], N, L) :-
    N1 is N - 1,
    remove_at(X, T, N1, L1),
    L = [H|L1].

%% 1.21 (*) Insert an element at a given position into a list.
%% Example:
%% ?- insert_at(alfa,[a,b,c,d],2,L).
%% L = [a,alfa,b,c,d]
insert_at(X, L, 1, [X|L]) :- !.
insert_at(X, [H|T], N, L2) :-
    N1 is N - 1,
    insert_at(X, T, N1, L3),
    L2 = [H|L3].

%% 1.22 (*) Create a list containing all integers within a given range.
%% Example:
%% ?- range(4,9,L).
%% L = [4,5,6,7,8,9]
range(K, K, [K]) :- !.
range(I, K, L) :-
    I1 is I + 1,
    range(I1, K, L1),
    L = [I|L1].

%% 1.23 (**) Extract a given number of randomly selected elements from a list.
%% The selected items shall be put into a result list.
%% Example:
%% ?- rnd_select([a,b,c,d,e,f,g,h],3,L).
%% L = [e,d,a]
%% Hint: Use the built-in random number generator random/2 and the result of problem 1.20.
rnd_select(L1, N, L2) :-
    rnd_select0(L1, N, L2, _).

rnd_select0(L, 1, [X], R) :- !,
    my_length(L, Length),
    Length1 is Length + 1,
    random(1, Length1, I),
    remove_at(X, L, I, R).
rnd_select0(L1, N, L2, R) :-
    N1 is N - 1,
    rnd_select0(L1, N1, L3, R1),
    my_length(R1, Length),
    Length1 is Length + 1,
    random(1, Length1, I),
    remove_at(X, R1, I, R),
    L2 = [X|L3].

%% 1.24 (*) Lotto: Draw N different random numbers from the set 1..M.
%% The selected numbers shall be put into a result list.
%% Example:
%% ?- rnd_select(6,49,L).
%% L = [23,1,17,33,21,37]
%% Hint: Combine the solutions of problems 1.22 and 1.23.
rnd_select_from_range(N, M, L) :-
    range(1, M, L1),
    rnd_select(L1, N, L).

%% 1.25 (*) Generate a random permutation of the elements of a list.
%% Example:
%% ?- rnd_permu([a,b,c,d,e,f],L).
%% L = [b,a,d,c,e,f]
%% Hint: Use the solution of problem 1.23.
rnd_permu(L1, L2) :-
    my_length(L1, Length),
    rnd_select(L1, Length, L2).

%% 1.26 (**) Generate the combinations of K distinct objects chosen from the N elements of a list
%% In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there
%% are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure
%% mathematicians, this result may be great. But we want to really generate all the possibilities (via backtracking).
%% Example:
%% ?- combination(3,[a,b,c,d,e,f],L).
%% L = [a,b,c] ;
%% L = [a,b,d] ;
%% L = [a,b,e] ;
%% ... 
combination(1, [H|T], L) :- !,
    (
     L = [H];
     combination(1, T, L)
    ).
combination(N, [H|T], L) :-
    N1 is N - 1,
    combination(N1, T, L1),
    L = [H|L1].
combination(N, [_|T], L) :-
    combination(N, T, L).

%% 1.27 (**) Group the elements of a set into disjoint subsets.
%% a) In how many ways can a group of 9 people work in 3 disjoint subgroups of 2, 3 and 4 persons? Write a predicate that generates all the possibilities via backtracking.
%% Example:
%% ?- group3([aldo,beat,carla,david,evi,flip,gary,hugo,ida],G1,G2,G3).
%% G1 = [aldo,beat], G2 = [carla,david,evi], G3 = [flip,gary,hugo,ida]
%% ...
%% b) Generalize the above predicate in a way that we can specify a list of group sizes and the predicate will return a list of groups.
%% Example:
%% ?- group([aldo,beat,carla,david,evi,flip,gary,hugo,ida],[2,2,5],Gs).
%% Gs = [[aldo,beat],[carla,david],[evi,flip,gary,hugo,ida]]
%% ...
%% Note that we do not want permutations of the group members; i.e. [[aldo,beat],...] is the same solution as [[beat,aldo],...]. However,
%% we make a difference between [[aldo,beat],[carla,david],...] and [[carla,david],[aldo,beat],...].
%% You may find more about this combinatorial problem in a good book on discrete mathematics under the term "multinomial coefficients". 
group3(L, G1, G2, G3) :-
    combination(2, L, G1),
    subtract(L, G1, L1),
    combination(2, L1, G2),
    subtract(L1, G2, G3).

group(L, [_], [L]) :- !.
group(L, [H|T], Gs) :-
    combination(H, L, G),
    subtract(L, G, L1),
    group(L1, T, Gs1),
    Gs = [G|Gs1].

subtract([], _, []) :- !.
subtract([H|T], L, L1) :-
    (   member(H, L)
    ->  subtract(T, L, L1)
    ;   subtract(T, L, L2),
        L1 = [H|L2]
    ).

%% 1.28 (**) Sorting a list of lists according to length of sublists
%% a) We suppose that a list (InList) contains elements that are lists themselves. The objective is to sort
%% the elements of InList according to their length. E.g. short lists first, longer lists later, or vice versa.
%% Example:
%% ?- lsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%% L = [[o], [d, e], [d, e], [m, n], [a, b, c], [f, g, h], [i, j, k, l]]
%% b) Again, we suppose that a list (InList) contains elements that are lists themselves. But this time the
%% objective is to sort the elements of InList according to their length frequency; i.e. in the default, where sorting
%% is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.
%% Example:
%% ?- lfsort([[a,b,c],[d,e],[f,g,h],[d,e],[i,j,k,l],[m,n],[o]],L).
%% L = [[i, j, k, l], [o], [a, b, c], [f, g, h], [d, e], [d, e], [m, n]]
%% Note that in the above example, the first two lists in the result L have length 4 and 1, both lengths appear just once.
%% The third and forth list have length 3; there are two list of this length. And finally, the last three lists have length 2.
%% This is the most frequent length.
lsort(L1, L2) :-
    tag(L1, L3),
    sort(1, @<, L3, L4),
    untag(L4, L2).

lfsort(L1, L2) :-
    tag(L1, L3),
    count_and_tag(L3, L3, L4),
    sort(1, @=<, L4, L5),
    untag(L5, L2).

tag([], []) :- !.
tag([H|T], L) :-
    tag(T, L1),
    my_length(H, Length),
    L = [(Length,H)|L1].

untag([], []) :- !.
untag([(_,L0)|T], L) :-
    untag(T, L1),
    L = [L0|L1].

count_and_tag(_, [], []) :- !.
count_and_tag(List, [(N,L0)|T], L) :-
    count_and_tag(List, T, L1),
    count_and_tag0(N, List, Count),
    L = [(Count,L0)|L1].

count_and_tag0(_, [], 0) :- !.
count_and_tag0(N, [(N,_)|T], Count) :- !,
    count_and_tag0(N, T, Count1),
    Count is Count1 + 1.
count_and_tag0(N, [_|T], Count) :-
    count_and_tag0(N, T, Count).