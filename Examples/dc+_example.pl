%%% -*- Mode: Prolog; -*-

:- use_module('../Inference/dc_plus.pl').
:- initialization(initial).


/*
 set_default(0): 
	users' responsibility to ensure that distributions for RVs are defined in each possible case.
 set_combining_rule(1): 
	noisy-or for boolean, mixture for discrete, mixture for gaussian, none for val, none for uniform
*/
initial :-
	init,
	set_sample_size(10000),
	set_default(0),
	set_combining_rule(1).


client(ann) ~ val(true).
client(bob) ~ val(true).

loan(l_1) ~ val(true).
loan(l_2) ~ val(true).
loan(l_3) ~ val(true).

account(a_1) ~ val(true).
account(a_2) ~ val(true).

has_loan(C,L) ~ discrete([0.3:true, 0.7:false]) := client(C)~=true, loan(L)~=true.

has_account(C,A) ~ discrete([0.6:true, 0.4:false]) := client(C)~=true, account(A)~=true.

account_loan(A,L) ~ discrete([0.2:true, 0.8:false]) := has_loan(C,L)~=true, has_account(C,A)~=true.

% Added the following three clauses to ensure exhaustiveness of clauses.
account_loan(A,L) ~ val(false) := has_loan(C,L)~=true, has_account(C,A)~=false.
account_loan(A,L) ~ val(false) := has_loan(C,L)~=false, has_account(C,A)~=true.
account_loan(A,L) ~ val(false) := has_loan(C,L)~=false, has_account(C,A)~=false.

amount(L) ~ gaussian(100000.0, 40000.9) := loan(L)~=true.

status(L) ~ discrete([0.4:appr, 0.5:pend, 0.1:decl]) := amount(L)~=X, X < 95000.
status(L) ~ discrete([0.1:appr, 0.3:pend, 0.6:decl]) := amount(L)~=X, X >= 95000.

balance(A) ~ gaussian(20000, 5000.9) := account_loan(A,L)~=true, amount(L)~=X, X < 97000.
balance(A) ~ gaussian(50000, 6000.9) := account_loan(A,L)~=true, amount(L)~=X, X >= 97000.

% Added the following clause to ensure the distribution is defined in the case account is not associated to any loan.
balance(A) ~ gaussian(25000, 6000.9) := account(A)~=true.

credit_score(C) ~ gaussian(750, 300.5) := has_loan(C,L)~=true, status(L)~=appr.
credit_score(C) ~ gaussian(650, 100.2) := has_loan(C,L)~=true, status(L)~=pend.
credit_score(C) ~ gaussian(600, 700.9) := has_loan(C,L)~=true, status(L)~=decl.
credit_score(C) ~ gaussian(M, 500.6) := has_loan(C,_)~=false, has_account(C,A)~=true, balance(A)~=X, M is 300 + X/100.
credit_score(C) ~ gaussian(630, 400.7) := has_loan(C,_)~=false, has_account(C,_)~=false.


/* 
  Answer to the following query:

	?- query((status(l_1)~=X, X==appr), [credit_score(bob)~=600, balance(a_1)~=50010], P).
	P = 0.0705437229417181.
*/






