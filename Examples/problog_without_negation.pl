%%% -*- Mode: Prolog; -*-
:- use_module('../Inference/dc_plus.pl').
:- initialization(initial).

initial :-
	init,
	set_sample_size(10000),
	set_default(1). % 1 for ProbLog


/*
 This DC program is equivalent to the following ProbLog program:

	person(angelika).
	person(joris).
	person(jonas).
	person(dimitar).

	friend(joris,jonas).
	friend(joris,angelika).
	friend(joris,dimitar).
	friend(angelika,jonas).

	0.3::stress(X) :- person(X).
	0.2::influences(X,Y) :- person(X), person(Y).

	smokes(X) :- stress(X).
	smokes(X) :- friend(X,Y), influences(Y,X), stress(Y).

	0.4::asthma(X) :- smokes(X).

	evidence(asthma(dimitar), true).
	evidence(asthma(angelika), false).

	query(stress(jonas)).

 Probability is 0.28637627
*/


do(person(angelika)~=1).
do(person(joris)~=1).
do(person(jonas)~=1).
do(person(dimitar)~=1).

do(friend(joris,jonas)~=1).
do(friend(joris,angelika)~=1).
do(friend(joris,dimitar)~=1).
do(friend(angelika,jonas)~=1).

0.3::stress(X) := person(X)~=1.
0.2::influences(X,Y) := person(X)~=1, person(Y)~=1.

1.0::smokes(X) := stress(X)~=1.
1.0::smokes(X) := friend(X,Y)~=1, influences(Y,X)~=1, stress(Y)~=1.

0.4::asthma(X) := smokes(X)~=1.

evd(asthma(dimitar)~=1).
evd(asthma(angelika)~=0).

query(P) :-
	query(stress(jonas)~=1, [], P).

% ?- query(P).





