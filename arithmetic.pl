:- use_module(my_lists).

%% 2.01 (**) Determine whether a given integer number is prime.
%% Example:
%% ?- is_prime(7).
%% Yes
is_prime(2) :- !.
is_prime(N) :-
    N >= 3,
    is_prime0(N, 2).

is_prime0(N, N) :- !.
is_prime0(N, M) :-
    \+ 0 is N mod M,
    M1 is M + 1,
    is_prime0(N, M1).

%% 2.02 (**) Determine the prime factors of a given positive integer.
%% Construct a flat list containing the prime factors in ascending order.
%% Example:
%% ?- prime_factors(315, L).
%% L = [3,3,5,7]
prime_factors(1, []) :- !.
prime_factors(N, L) :-
    next_prime_factor(N, 1, F),
    N1 is N / F,
    prime_factors(N1, L1),
    L = [F|L1], !.

next_prime_factor(N, N, N) :- !,
    is_prime(N).
next_prime_factor(N, I, I) :-
    is_prime(I),
    0 is N mod I.
next_prime_factor(N, I, F) :-
    I1 is I + 1,
    next_prime_factor(N, I1, F).

%% 2.03 (**) Determine the prime factors of a given positive integer (2).
%% Construct a list containing the prime factors and their multiplicity.
%% Example:
%% ?- prime_factors_mult(315, L).
%% L = [[3,2],[5,1],[7,1]]
%% Hint: The solution of problem 1.10 may be helpful.
prime_factors_mult(N, L) :-
    prime_factors(N, L1),
    my_lists:encode(L1, L2),
    exchange_position(L2, L).

exchange_position([], []) :- !.
exchange_position([[X,Y]|T], L) :-
    exchange_position(T, L1),
    L = [[Y,X]|L1].

%% 2.04 (*) A list of prime numbers.
%% Given a range of integers by its lower and upper limit, construct a list
%% of all prime numbers in that range.
prime_list(M, M, [M]) :-
    is_prime(M), !.
prime_list(M, M, []) :- !.
prime_list(N, M, L) :-
    N1 is N + 1,
    prime_list(N1, M, L1),
    (   is_prime(N)
    ->  L = [N|L1]
    ;   L = L1
    ).

%% 2.05 (**) Goldbach's conjecture.
%% Goldbach's conjecture says that every positive even number greater than 2
%% is the sum of two prime numbers. Example: 28 = 5 + 23. It is one of the most
%% famous facts in number theory that has not been proved to be correct in the
%% general case. It has been numerically confirmed up to very large numbers
%% (much larger than we can go with our Prolog system). Write a predicate to find
%% the two prime numbers that sum up to a given even integer.
%% Example:
%% ?- goldbach(28, L).
%% L = [5,23]
goldbach(N, L) :-
    next_prime(P, 1),
    Q is N - P,
    is_prime(Q),
    L = [P,Q], !.

next_prime(I, I) :-
    is_prime(I).
next_prime(N, I) :-
    I1 is I + 1,
    next_prime(N, I1).

%% 2.06 (**) A list of Goldbach compositions.
%% Given a range of integers by its lower and upper limit, print a list of all
%% even numbers and their Goldbach composition.
%% Example:
%% ?- goldbach_list(9,20).
%% 10 = 3 + 7
%% 12 = 5 + 7
%% 14 = 3 + 11
%% 16 = 3 + 13
%% 18 = 5 + 13
%% 20 = 3 + 17
%% In most cases, if an even number is written as the sum of two prime numbers,
%% one of them is very small. Very rarely, the primes are both bigger than say 50.
%% Try to find out how many such cases there are in the range 2..3000.
%% Example (for a print limit of 50):
%% ?- goldbach_list(1,2000,50).
%% 992 = 73 + 919
%% 1382 = 61 + 1321
%% 1856 = 67 + 1789
%% 1928 = 61 + 1867
goldbach_list(N, M, _) :-
    N > M, !.
goldbach_list(N, M, C) :-
    N > 2,
    0 is N mod 2,
    goldbach(N, [P,Q]),
    P > C, !,
    writeln(N=P+Q),
    N1 is N + 2,
    goldbach_list(N1, M, C).
goldbach_list(N, M, C) :-
    N1 is N + 1,
    goldbach_list(N1, M, C).

%% 2.07 (**) Determine the greatest common divisor of two positive integer numbers.
%% Use Euclid's algorithm.
%% Example:
%% ?- gcd(36, 63, G).
%% G = 9
%% Define gcd as an arithmetic function; so you can use it like this:
%% ?- G is gcd(36,63).
%% G = 9
:- arithmetic_function(my_gcd/2).

my_gcd(N, N, N) :- !.
my_gcd(N, M, D) :-
    N > M, !,
    N1 is N - M,
    my_gcd(N1, M, D).
my_gcd(N, M, D) :-
    N < M, !,
    M1 is M - N,
    my_gcd(N, M1, D).

%% 2.08 (*) Determine whether two positive integer numbers are coprime.
%% Two numbers are coprime if their greatest common divisor equals 1.
%% Example:
%% ?- coprime(35, 64).
%% Yes
coprime(N, M) :-
    1 is my_gcd(N, M).

%% 2.09 (**) Calculate Euler's totient function phi(m).
%% Euler's so-called totient function phi(m) is defined as the number of
%% positive integers r (1 <= r < m) that are coprime to m.
%% Example: m = 10: r = 1,3,7,9; thus phi(m) = 4. Note the special case: phi(1) = 1.
%% ?- Phi is totient_phi(10).
%% Phi = 4
%% Find out what the value of phi(m) is if m is a prime number. Euler's totient function
%% plays an important role in one of the most widely used public key cryptography methods (RSA).
%% In this exercise you should use the most primitive method to calculate this function.
%% There is a smarter way that we shall use in 2.10.
:- arithmetic_function(totient_phi/1).

totient_phi(1, 1) :- !.
totient_phi(M, C) :-
    totient_phi0(M, 1, C).

totient_phi0(M, M, 0) :- !.
totient_phi0(M, I, C) :-
    I1 is I + 1,
    totient_phi0(M, I1, C1),
    (   coprime(M, I)
    ->  C is C1 + 1
    ;   C = C1
    ).

%% 2.10 (**) Calculate Euler's totient function phi(m) (2).
%% See problem 2.09 for the definition of Euler's totient function. If the list of
%% the prime factors of a number m is known in the form of problem 2.03 then the
%% function phi(m) can be efficiently calculated as follows: Let [[p1,m1],[p2,m2],[p3,m3],...]
%% be the list of prime factors (and their multiplicities) of a given number m. Then phi(m)
%% can be calculated with the following formula:
%% phi(m) = (p1 - 1) * p1**(m1 - 1) * (p2 - 1) * p2**(m2 - 1) * (p3 - 1) * p3**(m3 - 1) * ...
%% Note that a**b stands for the b'th power of a.
:- arithmetic_function(phi/1).

phi(M, C) :-
    prime_factors_mult(M, L),
    phi0(L, C).

phi0([], 1) :- !.
phi0([[P,N]|T], C) :-
    phi0(T, C1),
    C is (P - 1) * P ** (N - 1) * C1.

%% 2.11 (*) Compare the two methods of calculating Euler's totient function.
%% Use the solutions of problems 2.09 and 2.10 to compare the algorithms.
%% Take the number of logical inferences as a measure for efficiency. Try to
%% calculate phi(10090) as an example.
