%%% -*- Mode: Prolog; -*-

:- use_module('../Inference/dc_plus.pl').
:- initialization(initial).

/*
	Note
	====
	In case of unknown objects query_naive/3 should be used instead of query/3.

	Example
	=======

	?- query_naive(color(1)~=green, [nballs~=3, color(1)~=white], P).
	P = 0.2283.
	
*/


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




% Declare builtin predicates
builtin(between(_,_,_)).


nballs ~ poisson(4) := true.

ball(X) ~ finite([0.8:1, 0.2:0]) := nballs~=N, between(1,N,X).

color(X) ~ finite([0.2:green, 0.5:red, 0.3:white]) := ball(X)~=1.
color(X) ~ uniform([green,red,white]) := \+ball(X)~=1.








