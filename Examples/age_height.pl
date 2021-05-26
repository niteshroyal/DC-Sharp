%%% -*- Mode: Prolog; -*-
:- use_module('../Inference/dc_plus.pl').

%:- use_module('../DC_Old/dcpf.pl'). 
%:- use_module('../DC_Old/random/sampling.pl'). 
%:- use_module('../DC_Old/distributionalclause.pl'). 
%:- set_options(default). 
%:- set_debug(true).

:- initialization(init).





person(ann) ~ finite([0.4:0, 0.6:1]).

sex(X) ~ finite([0.3:m, 0.6:f, 0.1:t]) := person(X)~=1.
sex(X) ~ val(t) := person(X)~=0.

age(X) ~ gaussian(30,2.0) := person(X)~=1.

height(X) ~ gaussian(M, 3.2) := age(X)~=Y, M is 0.9*Y + 1.0.
height(X) ~ gaussian(32, 2.2) := \+age(X)~=_, sex(X)~=m.
height(X) ~ gaussian(25, 1.2) := \+age(X)~=_, sex(X)~=f.
height(X) ~ gaussian(26, 5.2) := \+age(X)~=_, sex(X)~=t.

qry(Q,Evd,N,P) :-
	query(Evd,[],Q,N,P).

