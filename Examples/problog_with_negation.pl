%%% -*- Mode: Prolog; -*-
:- use_module('../Inference/dc_plus.pl').
:- initialization(initial).

initial :-
	init,
	set_sample_size(10000),
	set_default(1). % 1 for ProbLog


/*
 This DC program is equivalent to the following ProbLog program:

	client(c1).
	client(c2).

	account(a1).
	account(a2).

	loan(l1).
	loan(l2).

	0.2::client_account(C,A) :- client(C), account(A).
	0.3::account_loan(A,L) :- account(A), loan(L).

	0.8::client_loan(C,L) :- client_account(C,A), account_loan(A,L).

	0.6::in_debt(C) :- client_loan(C,_).
	0.1::in_debt(C) :- \+client_loan(C,_).

	0.9::dead_account(X) :- account(X), \+account_loan(X,Y), \+client_loan(_,Y).
	0.8::error :- dead_account(_).

	evidence(dead_account(a1), false).
	evidence(in_debt(c1), true).

	query(error).

 Probability is 0.095187014
*/



do(client(c1)~=1).
do(client(c2)~=1).

do(account(a1)~=1).
do(account(a2)~=1).

do(loan(l1)~=1).
do(loan(l2)~=1).


0.2::client_account(C,A) := client(C)~=1, account(A)~=1.
0.3::account_loan(A,L) := account(A)~=1, loan(L)~=1.

0.8::client_loan(C,L) := client_account(C,A)~=1, account_loan(A,L)~=1.

0.6::in_debt(C) := client_loan(C,_)~=1.
0.1::in_debt(C) := \+client_loan(C,_)~=1.

0.9::dead_account(X) := account(X)~=1, \+account_loan(X,Y)~=1, \+client_loan(_,Y)~=1.
0.8::error := dead_account(_)~=1.

evd(dead_account(a1)~=0).
evd(in_debt(c1)~=1).

query(P) :-
	query(error~=1, [], P).

% ?- query(P).
% P = 0.09556543042561197.





