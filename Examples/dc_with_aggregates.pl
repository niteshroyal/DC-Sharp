%%% -*- Mode: Prolog; -*-

:- use_module('../Inference/dc_plus.pl').
:- initialization(initial).

% Configuration to run Davide's DC
%	set_default(2): RVs are not assigned any value (assignment fails) if distributions for them are not defined
%	set_combining_rule(0): no combining rule
%   
%   Davide's DC (DC_Old) can be found here: https://github.com/davidenitti/DC
initial :-
	display_transformed_rules(1),
	init,
	set_sample_size(10000),
	set_default(2),
	set_combining_rule(0).

% Declare builtin predicates
builtin(maximum(_,_)).

% Prolog code for finding the maximum in the list of numbers.
maximum([M], M) :- !.
maximum([H|T], M) :-
	maximum(T,M1),
	( H > M1 -> M=H; M=M1).


do(client(ann)~=1). 
do(client(bob)~=1).

do(loan(l_1)~=1).
do(loan(l_2)~=1).

has_loan(C,L) ~ finite([0.3:1, 0.7:0]) := client(C)~=1, loan(L)~=1.

amount(L) ~ gaussian(100000,250.5) := loan(L)~=1.

age(C) ~ gaussian(30,5.4) := client(C)~=1.

credit_score(C) ~ gaussian(500,10.2) := findall_dc(X, (has_loan(C,L)~=1, amount(L)~=X), L), maximum(L, Y), Y>=100000.

credit_score(C) ~ gaussian(600,20.3) := findall_dc(X, (has_loan(C,L)~=1, amount(L)~=X), L), maximum(L, Y), Y<100000.

credit_score(C) ~ gaussian(650,40.5) := findall_dc(X, (has_loan(C,L)~=1, amount(L)~=X), L), \+maximum(L, _), age(C)~=Z, Z =< 30.

credit_score(C) ~ gaussian(700,20.7) := findall_dc(X, (has_loan(C,L)~=1, amount(L)~=X), L), \+maximum(L, _), age(C)~=Z, Z > 30.


% ?- query(has_loan(ann,l_1)~=1, [credit_score(ann)~=500.5, credit_score(bob)~=649.7], P).
% P = 0.6312025033527883.






