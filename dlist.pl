dl_append(X-Xs, Y-Ys, Z-Zs) :-
    Xs = Y,
    Z = X,
    Ys = Zs.

dl_append1(X-Y, Y-Z, X-Z).

dl_rotate([H|T]-[H|A], T-A).

dl_double(A-A, B-B) :- !.
dl_double([H|T]-T1, [R|S]-S1) :-
    R is H * 2,
    dl_double(T-T1, S-S1).

dl_reverse([], L-L) :- !.
dl_reverse([H|T], Y-Ys) :-
    dl_reverse(T, Y-[H|Ys]).

dl_flatten([], L-L) :- !.
dl_flatten([H|T], X-Xs) :- !,
    dl_flatten(H, X-A),
    dl_flatten(T, A-Xs).
dl_flatten(X, [X|T]-T).

quicksort_dl([], S-S) :- !.
quicksort_dl([X|Xs], Sorted-Tail) :-
  partition(X, Xs, Smalls, Bigs),
  quicksort_dl(Smalls, Sorted-[X|T]),  
  quicksort_dl(Bigs, T-Tail).

partition(_, [], [], []) :- !.
partition(Pivot, [X|Xs], [X|Ys], Zs) :-
   X =< Pivot, !,
   partition(Pivot, Xs, Ys, Zs).
partition(Pivot, [X|Xs], Ys, [X|Zs]) :-
   X > Pivot,
   partition(Pivot, Xs, Ys, Zs).