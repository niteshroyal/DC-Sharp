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

/*
%  Use the following to run Davide's DC

	:- use_module('../DC_Old/dcpf.pl'). 
	:- use_module('../DC_Old/random/sampling.pl'). 
	:- use_module('../DC_Old/distributionalclause.pl'). 
	:- set_options(default).
	:- initialization(init).

	qry(Query,Evidence,NumSamples,Prob) :-
		query(Evidence,[],Query,NumSamples,Prob).
*/


% Declare builtin predicates
builtin(between(_,_,_)).

nballs ~ poisson(2) := true.
ball(X) ~ val(1) := nballs~=N, between(1,N,X).
color(X) ~ uniform([green,red,white]) := ball(X)~=1.

% ?- query(color(1)~=green, [nballs~=3], P).
% P = 0.33471.






