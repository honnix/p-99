dl_append(X-Xs, Y-Ys, Z-Zs) :-
    Xs = Y,
    Z = X,
    Ys = Zs.

dl_append1(X-Y, Y-Z, X-Z).

dl_rotate([H|T]-[H|A], T-A).

dl_double(A-A,B-B) :- !.
dl_double([H|T]-T1,[R|S]-S1) :-
    R is H*2,
    dl_double(T-T1,S-S1).