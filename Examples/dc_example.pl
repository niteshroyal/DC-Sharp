%%% -*- Mode: Prolog; -*-

:- use_module('../Inference/dc_plus.pl').
:- initialization(initial).

% Configuration to run Davide's DC 
%	set_default(2): RVs are not assigned any value (assignment fails) if distributions for them are not defined
%	set_combining_rule(0): no combining rule
%   
%   Davide's DC (DC_Old) can be found here: https://github.com/davidenitti/DC
initial :-
	init,
	set_sample_size(10000),
	set_default(2),
	set_combining_rule(0).



client(ann) ~ val(true).
client(bob) ~ val(true).

loan(l_1) ~ val(true).
loan(l_2) ~ val(true).
loan(l_3) ~ val(true).

account(a_1) ~ val(true).
account(a_2) ~ val(true).

has_loan(C,L) ~ finite([0.3:true, 0.7:false]) := client(C)~=true, loan(L)~=true.

has_account(C,A) ~ finite([0.6:true, 0.4:false]) := client(C)~=true, account(A)~=true.

account_loan(A,L) ~ val(true) := has_loan(C,L)~=true, has_account(C,A)~=true.

amount(L) ~ gaussian(100000.0, 40000.9) := loan(L)~=true.

status(L) ~ finite([0.4:appr, 0.5:pend, 0.1:decl]) := amount(L)~=X, X < 95000.
status(L) ~ finite([0.1:appr, 0.3:pend, 0.6:decl]) := amount(L)~=X, X >= 95000.

balance(A) ~ gaussian(20000, 5000.9) := account_loan(A,L)~=true, amount(L)~=X, X < 97000.
balance(A) ~ gaussian(50000, 6000.9) := account_loan(A,L)~=true, amount(L)~=X, X >= 97000.

credit_score(C) ~ gaussian(550, 20.9) := has_account(C,A)~=true, balance(A)~=X, X < 25000.
credit_score(C) ~ gaussian(750, 30.5) := has_account(C,A)~=true, balance(A)~=X, X >= 25000, has_loan(C,L)~=true, status(L)~=appr.
credit_score(C) ~ gaussian(650, 10.2) := has_account(C,A)~=true, balance(A)~=X, X >= 25000, has_loan(C,L)~=true, status(L)~=pend.
credit_score(C) ~ gaussian(600, 70.9) := has_account(C,A)~=true, balance(A)~=X, X >= 25000, has_loan(C,L)~=true, status(L)~=decl.

credit_score(C) ~ gaussian(670, 50.6) := client(C)~=true.

% ?- query((status(l_1)~=X, X==appr), [credit_score(bob)~=600, balance(a_1)~=50010], P).
% P = 0.054214315967807486.





