%%% -*- Mode: Prolog; -*-
/*
 * Author:	Nitesh Kumar
 * E-mail:	nitesh.kr369@gmail.com
 * WWW:		https://sites.google.com/view/niteshroyal 
 * Copyright (C) 2020 Nitesh Kumar
 * 
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 3 of the License, or (at
 * your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

:- module('dc_plus', 
	[	init/0, 
		query/3,
		query_naive/3, 
		query/4, 
		query_timeout/6,
		query_timeout_naive/6, 
		set_debug/1,  
		set_default/1, 
		set_sample_size/1,
		set_time_out/1, 
		set_seed/1,
		set_combining_rule/1,
		display_transformed_rules/1,
		set_residual_approach/1
	]).

:- load_foreign_library('../Sampling/sampling').

:- dynamic 
	intervention/1, 
	evidence/1, 
	sample_size/1, 
	debug/1, 
	default/1, 
	approach/1, 
	rule/2, 
	time_out/1, 
	has_rv/1, 
	trans_rules/1, 
	combining_rule/1.

:- multifile 
	user:builtin/1.

:- op(690,xfx,user:'::').
:- op(690,xfx,user:'~').
:- op(681,xfx,user:'~=').
:- op(1100,xfx,user:':=').

:- discontiguous 
	user:(::)/2, 
	user:(~)/2, 
	user:(:=)/2.

:- table (rv/1) as subsumptive.
:- table (child/2) as subsumptive.

:- initialization(init).

/* Built-in predicates: invokes Prolog predicates */
user:builtin(_ = _).
user:builtin(_ == _).
user:builtin(_ \== _).
user:builtin(_ \= _).
user:builtin(_ is _).
user:builtin(_ > _).
user:builtin(_ < _).
user:builtin(_ >= _).
user:builtin(_ =< _).
user:builtin(true).
user:builtin(write(_)).
user:builtin(writeln(_)).

/*
initialization options:
	display_transformed_rules/1:
		1: displays the transformed distributional clauses
		0: does not display that
	set_debug/1:
		1: debug mode on, i.e., displays sampled safe contextual assignments
		0: debug mode off
	set_default/1:
		0: users' responsibility to ensure that distributions for RVs are defined in each possible case.
		1: by default Boolean RVs are false if distributions for them are not defined
			(same as ProbLog and works only when all RVs are Boolean in the program).
		2: RVs are not assigned any value (assignment fails) if distributions for them are not defined
			(same as Davide's DC where RVs' value can be "undefined" in the possible world).
	set_sample_size/1:
		N: number of samples	
	set_time_out/1:
		0: the number of samples is used to stop sampling
		1: time (in secs) is used to stop sampling
	set_combining_rule/1:
		0: no combining rule (same as Davide's DC where the first distribution defined for RV is considered)
		1: noisy-or for boolean, mixture for discrete, mixture for gaussian, none for val, none for uniform, none for poisson
		2: noisy-or for boolean, mixture for discrete, noisy-avg for gaussian, none for val, none for uniform, none for poisson
	set_residual_approach/1:
		bayes_ball: set of all diagnostic evidence is computed using Bayes ball algorithm and then residuals are computed
		full: set of diagnostic evidence is estimated from samples and then residuals are computed
		biased: residuals are not computed. This approach gives biased estimates.
*/
init :- 
	%display_transformed_rules(1),
	\+backward_convert([do(X), X~Y, X::Y, X:=Y]),
	\+backward_convert_evd,
	\+define_rvs([do(X), X~Y, X::Y, X:=Y]),
	connect_sampler(1),
	set_debug(0),
	set_residual_approach(full),
	set_default(0),
	set_time_out(0),
	set_combining_rule(1).

release :- 
	connect_sampler(0),
	abolish_all_tables.

set_seed(S) :- 
	set_sampler_seed(S).

/* Transformation of distributional clauses for backward-chaining */
backward_convert(All_special_predicates) :- 
	member(Pred,All_special_predicates), 
	backward_convert2(Pred).

backward_convert2(L) :- 
	clause(L,_), 
	new_backward_rule(L), 
	fail.

backward_convert_evd :-
	clause(evd(X),Y),
	( Y=true ->
		assert_transformations(evidence(X))
	;	process_evd(Y,Y1), assert_transformations(:-(evidence(X),Y1))
	), fail.

new_backward_rule(do(X)) :- 
	assert_transformations(intervention(X)).

new_backward_rule(H~D) :-
	type_of_rvs(D),
	assert_transformations(:-(distributional(H),add_distribution(H,D))).

new_backward_rule(P::F) :- 
	type_of_rvs(bernoulli(P)),
	assert_transformations(:-(distributional(F),add_distribution(F,bernoulli(P)))).

new_backward_rule(H~D:=R) :- 
	type_of_rvs(D),
	add_at_end(R,add_distribution(H,D),R_distribution),
	assert_transformations(:-(distributional(H),R_distribution)).

new_backward_rule(P::F:=R) :- 
	type_of_rvs(bernoulli(P)),
	add_at_end(R,add_distribution(F,bernoulli(P)),R_distribution),
	assert_transformations(:-(distributional(F),R_distribution)). 

process_evd(X, X1) :-
	\+(X=(_,_)), 
	X=evd(Z),
	X1=evidence(Z),
	!.

process_evd((evd(Z),Y), (evidence(Z),Y1)) :-
	process_evd(Y,Y1).

add_at_end(A,E,(A,E)) :- 
	\+(A=(_,_)), !.

add_at_end((A,B),E,C) :- 
	\+(B=(_,_)), C=(A,(B,E)), !.

add_at_end((A,B),E,(A,C)) :- 
	add_at_end(B,E,C).

type_of_rvs(D) :-
	( D = bernoulli(_) ->
		set_rv(boolean)
	;	( (D = discrete(_); D = finite(_)) ->
			set_rv(discrete)
		;	( D = gaussian(_,_) ->
				set_rv(gaussian)
			;	( D = val(_) ->
					set_rv(constraints)
				;	( D = uniform(_) ->
						set_rv(uniform)
					;	( D = poisson(_) ->
							set_rv(poisson)
						;	throw("unknown RV type encountered")
						)
					)
				)
			)
		)
	).

assert_transformations(X) :-
	( trans_rules(1) ->
		writeln(X)
	;	true
	), assertz(X).

/* Transformation of distributional clauses for forward-chaining */
define_rvs(All_special_predicates) :- 
	member(Pred,All_special_predicates), 
	define_rvs2(Pred).

define_rvs2(L) :- 
	clause(L,_), 
	rv_rule(L), 
	fail.

rv_rule(do(X)) :- 
	rv_format(X,Y),
	assert_transformations(Y).

rv_rule(H~_) :-  
	assert_transformations(rv(H)).

rv_rule(_::F) :- 
	assert_transformations(rv(F)).

rv_rule(H~D:=B) :- 
	term_variables(H~D,V),
	skolemize(V,B,R,_),
	tuple_to_list(R,Rlist), 
	rv_format(R,R1),
	assert_transformations(:-(rv(H),R1)),
	\+assert_children(H,Rlist,R1).

rv_rule(P::F:=B) :- 
	term_variables(P::F,V),
	skolemize(V,B,R,_),
	tuple_to_list(R,Rlist), 
	rv_format(R,R1), 
	assert_transformations(:-(rv(F),R1)),
	\+assert_children(F,Rlist,R1).

assert_children(H,Rlist,R1) :- 
	member(G,Rlist), 
	fetch_hb(G,F),
	assert_children1(F,H,R1),
	fail.

fetch_hb(G,F) :-
	( G = F~=_ ->
		true
	;	( (G = \+(_~=_); G = not(_~=_)) ->
			fetch_hb_neg(G,F)
		;	( is_findall_dc(G,Q) ->
				fetch_hb_findall(Q,F)
			;	fail
			)
		)
	).

fetch_hb_neg(G,F) :- 
	( (G = \+(G1); G = not(G1)) ->
		fetch_hb_neg(G1,F)
	;	G = F~=_
	).

fetch_hb_findall(Q,F) :-
	fetch_hb_findall1(Q,QList), 
	member(F,QList).

fetch_hb_findall1(A,L) :-
	\+(A=(_,_)),
	fetch_hb1(A,L),
	!.
fetch_hb_findall1((A,B),L) :- 
	fetch_hb_findall1(B,L1),
	fetch_hb1(A,L2),
	append(L1,L2,L).

fetch_hb1(A,L) :-
	( fetch_hb(A,A1) ->
		L = [A1]
	;	L = []
	).

assert_children1(F,H,R1) :-
	add_at_end(R1,rv(H),X), 
	assert_transformations(:-(child(F,H),X)).

rv_format(A,B) :-
	rv_format1(A,C),
	rv_format2(C,L),
	( L = [] ->
		B = true
	;	tuple_to_list(B,L)
	).

rv_format1(A,A1) :- 
	\+(A=(_,_)), 
	process_atoms(A,A1), 
	!. 
rv_format1((A,B),C) :- 
	rv_format1(B,B1), 
	process_atoms(A,A1),
	add_at_end(A1,B1,C).

rv_format2(A,L) :-
	\+(A=(_,_)),
	rv_format3(A,[],L).
rv_format2((A,B),L) :-
	rv_format2(B,L1),
	rv_format3(A,L1,L).

rv_format3(A,T,L) :-
	( A = true ->
		L = T
	;	L = [A|T]
	).

process_atoms(A,A1) :-
	( A = R~=_ ->
		A1=rv(R)
	;	( (A = \+(_~=_); A = not(_~=_)) ->
			process_negation(A,1,A1)
		;	( is_findall_dc(A,Q) ->
				process_findall(Q,A1)
			;	( (builtin(A); A = \+(B), builtin(B); A = not(B), builtin(B)) ->
					A1=true
				;	throw("unexpected literal in the body of distributional clause")
				)
			)
		)
	).

process_negation(A,X,A1) :- 
	( (A = \+(A2); A = not(A2)) -> 
		(X=1 -> X1=0; X1=1), process_negation(A2,X1,A1)
	;	A = R~=_,
		( X=1 ->
			A1=rv(R)
		;	A1=true
		)
	).

process_findall(A,A1) :-
	\+(A=(_,_)),
	process_atoms(A,A1),
	!.
process_findall((A,B),(A1,B1)) :- 
	process_findall(B,B1),
	process_atoms(A,A1).

/*Skolemization of the body of distributional clauses*/
skolemize(V,L,R,U) :-
	\+(L = (_,_)),
	renamed_literal(V,L,R,U).
skolemize(V,(A,B),(A1,B1),U) :-
	renamed_literal(V,A,A1,U1),
	skolemize(U1,B,B1,U).

renamed_literal(V,L,R,U) :- 
	( (L = \+(_~=_); is_findall_dc(L,_)) ->
		rename_term_variables(L,V,R),
		U = V
	;	term_variables(L,V1), 
		append(V,V1,U1), 
		list_to_set(U1,U),
		R = L
	).

is_findall_dc(L,Q) :- 
	L = findall_dc(_,Q,_); L = findall_forward(_,Q,_).

rename_term_variables(Term,V,New_Term) :-
	term_variables(Term, V1),
	copy_term(Term, New_Term),
	term_variables(New_Term, V2),
	unify_var(V,V1,V2).

unify_var(_,[],[]) :- !.
unify_var(V,[H1|T1],[H2|T2]) :-
	( needed_var(H1,V) ->
		H2 = H1
	;	true
	), unify_var(V,T1,T2).

needed_var(_,[]) :- fail.
needed_var(H,[HV|TV]) :-
	( H == HV ->
		!
	;	needed_var(H,TV)
	).

/* Backward chaining */
findall_dc(X,Q,L) :- 
	findall(X,Q,L).

findall_forward(X,Q,L) :- 
	findall(X,Q,L).

prove_all(P) :- 
	( combining_rule(0) ->
		not(oncetried(P))
	;	not(alltried(P))
	).

oncetried(P) :-
	once(P),
	fail.

alltried(P) :- 
	call(P), 
	fail.

add_distribution(A,D) :- 
	term_hash(distribution(A),H), 
	recordz(H,distribution(A,D)).

X~=Y :- 
	rv(X), 
	assign(X,Z), 
	Z=Y.

assign(X,Y) :- 	
	( intervention(X~=Y) ->
		true
	; 	( evidence(X~=Y) ->
			( recorded_possible_predictive_evd(X) -> 
				true
			; 	record_possible_predictive_evd(X)
			)
	  	; 	( recorded_top(X) ->
				recorded_assigned(X,Y)
	    		; 	P=distributional(X), 
				prove_all(P),
				record_top(X),
				( recorded_bottom(X) -> 
					true
				; 	record_bottom(X), recordz(agenda,X)
				), sample(X,Y),
				record_assigned(X,Y)
	    		)
	  	)
	).

/* Forward chaining */
forward :- 
	done, 
	!.

forward :- 
	\+todo, 
	forward.

todo :- 
	recorded(agenda,F,R), 
	erase(R), 
	pursuit(F), 
	fail.

pursuit(F) :- 
	child(F,L), 
	infer_child(L).

infer_child(L) :- 
	\+intervention(L~=_),
	( evidence(L~=Y) ->
		\+recorded_top(L), 
		record_top(L), 
		P=distributional(L), 
		prove_all(P), 
		weight(L,Y,W), 
		record_diagnostic_evidence(L,Y,W)
	;	\+recorded_bottom(L), 
		record_bottom(L), 
		recordz(agenda,L)
	).

done :- 
	\+recorded(agenda,_).

/* Bayes Ball to detected all diagnostic evidence */
bayes_ball(Q,ListDiagEvd) :-
	forall(recorded(_,_,R), erase(R)),
	visit_queries(Q),
	findall(X, recorded(diagnostic_evd, X), ListDiagEvd).

visit_queries(V~=_) :-
	rv(V),
	\+visit(V,child).
visit_queries((V~=_,B)) :-
	rv(V),
	\+visit(V,child),
	visit_queries(B).

visit(J,child) :- 
	\+intervention(J~=_),
	\+evidence(J~=_),
	markTop(J),
	child(P,J),
	visit(P,child).
visit(J,child) :-
	\+intervention(J~=_),
	\+evidence(J~=_),
	markBottom(J), 
	child(J,C), 
	visit(C,parent).
visit(J,parent) :-
	evidence(J~=_),
	markTop(J), 
	record_diagnostic(J),
	child(P,J), 
	visit(P,child).
visit(J,parent) :- 
	\+intervention(J~=_),
	\+evidence(J~=_), 
	markBottom(J), 
	child(J,C), 
	visit(C,parent).

markTop(J) :- 
	( recorded_top(J) ->
		fail
	;	record_top(J)
	).

markBottom(J) :-
	( recorded_bottom(J) ->
		fail
	;	record_bottom(J)
	).

record_diagnostic(J) :-
	recordz(diagnostic_evd, J).

/* Simulation of DC programs */
query(Q,P) :- 
	check_sample_size, 
	sample_size(N), 
	findall(Entail, (between(1,N,_), prove(Q,Entail)), L), 
	probability(L,P).

query1(Q,E,P,BB) :- 
	check_sample_size, 
	debug(F), 
	check_debug, 
	sample_size(N), 
	clean, 
	add_evidence(E),
	( BB=1 ->
		findall(Entail, (between(1,N,K), forward_backward(K,Q,Entail)), L)
	;	findall(Entail, (between(1,N,K), backward_naive(K,Q,Entail)), L)
	), 
	( approach(bayes_ball) ->
		bayes_ball(Q,LDE),
		list_of_full_weights_and_residuals(N, LDE, PW, Res),
		expected_lw(PW,Res,EW)
	;	(  approach(full) ->
			list_of_full_weights_and_residuals(N, PW, Res),
			expected_lw(PW,Res,EW)
		;	list_of_partial_weights(N,PW), 
			expected_lw(PW,EW)
		)
	),
	probability(L,EW,P), 
	analyze_evidence(N,PW,EW,L,F), 
	remove_evidence(E).

query(Q,E,P) :-
	query1(Q,E,P,1).

query_naive(Q,E,P) :-
	query1(Q,E,P,0).

query_timeout(Q,E,P,M,Secs,N,BB) :- 
	debug(F), 
	check_debug, 
	clean, 
	add_evidence(E), 
	set_time_out(Secs),
	( BB=1 ->
		findall(Entail, (between(1,M,K), time_out(X), get_time(Y), Y<X, forward_backward(K,Q,Entail)), L)
	;	findall(Entail, (between(1,M,K), time_out(X), get_time(Y), Y<X, backward_naive(K,Q,Entail)), L)
	), length(L,N), 
	( approach(bayes_ball) ->
		bayes_ball(Q,LDE),
		list_of_full_weights_and_residuals(N, LDE, PW, Res),
		expected_lw(PW,Res,EW)
	;	( approach(full) -> 
			list_of_full_weights_and_residuals(N, PW, Res), 
			expected_lw(PW,Res,EW)
		;	list_of_partial_weights(N,PW),
			expected_lw(PW,EW)
		)
	),
	probability(L,EW,P), 
	analyze_evidence(N,PW,EW,L,F), 
	remove_evidence(E).

query_timeout(Q,E,P,M,Secs,N) :-
	query_timeout(Q,E,P,M,Secs,N,1).

query_timeout_naive(Q,E,P,M,Secs,N) :-
	query_timeout(Q,E,P,M,Secs,N,0).

query(Q,E,D,P) :- 
	check_sample_size, 
	debug(F), 
	check_debug, 
	sample_size(N), 
	clean, 
	add_intervention(D), 
	add_evidence(E),
	findall(Entail, (between(1,N,K), forward_backward(K,Q,Entail)), L),
	list_of_partial_weights(N,PW), 
	expected_lw(PW,EW), 
	probability(L,EW,P),
	analyze_evidence(N,PW,EW,L,F), 
	remove_evidence(E), 
	remove_intervention(D).

prove(Q,Entail) :- 
	forall(recorded(_,_,R), erase(R)), 
	( call(Q) -> 
		Entail=1
	; Entail=0
	).

forward_backward(K,Q,Entail) :- 
	forall(recorded(_,_,R), erase(R)),
	(call(Q) -> Entail=1; Entail=0), 
	forward, 
	record(K), 
	debug(F), 
	analyze_sample(K,F).

backward_naive(K,Q,Entail) :- 
	forall(recorded(_,_,R), erase(R)),
	(call(Q) -> Entail=1; Entail=0), 
	\+to_prove_evidence, 
	record(K), 
	debug(F), 
	analyze_sample(K,F).

to_prove_evidence :-
	evidence(X~=_),
	infer_child(X),
	fail.

/* Sampling */
all_distributions(X,D_list) :- 
	term_hash(distribution(X),H), 
	findall(D, recorded(H,distribution(X,D)), D_list).

sample(X,Y) :- all_distributions(X,D_list),
	( D_list=[] -> 
		( default(1) -> 
			sample_default(X,Y)
		;	( default(2) ->
				fail
			;	throw('No distribution to sample')
			)
		)
	; 	combine_distributions(D_list, D_combined), 
		sample_distribution(D_combined,Y)
	).

weight(X,Y,W) :- all_distributions(X,D_list),
	( D_list=[] ->
		( default(1) -> 
			weight_default(X,Y,W)
		;	( default(2) ->
				W is 0.0
			;	throw('No distribution to weight')
			)
		)
	; 	combine_distributions(D_list, D_combined), 
		weight_distribution(D_combined,Y,W)
	).

sample_default(_,Y) :- 
	Y=0.

weight_default(_,Y,W) :- 
	( Y=0 -> 
		W is 1
	; 	W is 0
	).

sample_distribution(bernoulli(P),V) :- 
	sample_bernoulli(P,V).
sample_distribution(discrete(L),V) :- 
	sample_discrete_pl(L,V).
sample_distribution(gaussian(Mean,Variance),V) :- 
	sample_gaussian(Mean,Variance,V).
sample_distribution(val(X),V) :- 
	V=X.
sample_distribution(uniform(X),V) :- 
	length(X,N), sample_uniform(N,Idx), nth1(Idx,X,V).
sample_distribution(gaussian_mixture(Ms,Vs,Ws),V) :-
	sample_gaussian_mixture(Ms,Vs,Ws,V).
sample_distribution(poisson(Mu),V) :- 
	sample_poisson(Mu,V).

weight_distribution(bernoulli(P),V,W) :- 
	weight_bernoulli(P,V,W).
weight_distribution(discrete(L),V,W) :- 
	weight_discrete_pl(L,V,W).
weight_distribution(gaussian(Mean,Variance),V,W) :- 
	weight_gaussian(Mean,Variance,V,W).
weight_distribution(val(X),V,W) :- 
	( number(X) ->
		( X=:=V ->
			W is 1.0
		;	W is 0.0
		)
	;	( X==V ->
			W is 1.0
		;	W is 0.0
		)
	).
weight_distribution(uniform(X),V,W) :-
	( memberchk(V,X) ->
		length(X,N), 
		W is 1/N
	;	W is 0.0
	).
weight_distribution(gaussian_mixture(Ms,Vs,Ws),V,W) :-
	weight_gaussian_mixture(Ms,Vs,Ws,V,W).
weight_distribution(poisson(Mu),V,W) :- 
	weight_poisson(Mu,V,W).

weight_bernoulli(P,V,W) :- 
	( V=1 -> 
		W is P
	; 	( V=0 -> 
			W is 1-P
		; 	fail
		)
	).

discrete_get_probs_vals([],[],[]).
discrete_get_probs_vals([H|T],P,V) :- 
	discrete_get_probs_vals(T,P1,V1), 
	P2:V2=H, 
	P=[P2|P1], 
	V=[V2|V1].

sample_discrete_pl(List,Val) :- 
	discrete_get_probs_vals(List,P,V), 
	sample_discrete(P,X), 
	nth0(X,V,Val).

weight_discrete_pl([H|_],Val,W) :- 
	W:Val=H, 
	!.
weight_discrete_pl([_|T],Val,W) :- 
	weight_discrete_pl(T,Val,W).

/*Combining rules for bernoulli, discrete and gaussian distributions*/
combine_distributions([bernoulli(P)|Tail], D) :- 
	combine_bernoulli([bernoulli(P)|Tail], Q), 
	Q1 is 1-Q, D=bernoulli(Q1).
combine_distributions([discrete(L)|Tail], D) :- 
	combine_discrete_distributions([discrete(L)|Tail],D).
combine_distributions([gaussian(M,V)|Tail], D) :- 
	combine_gaussian_distributions([gaussian(M,V)|Tail],D).
combine_distributions([val(X)|Tail], D) :- 
	combine_vals([val(X)|Tail],Y), list_to_set(Y,Z),
	( Z = [V] ->
		D = val(V)
	;	throw('Error combining vals: a RV cannot have two values in a possible world.')
	).
combine_distributions([poisson(Mu)|Tail], D) :- 
	( Tail = [] ->
		D = poisson(Mu)
	;	throw('The combining rule for Poisson distributions is not written yet.')
	).
combine_distributions([uniform(X)|Tail], D) :- 
	combine_uniform_distributions([uniform(X)|Tail], D).
combine_distributions([finite(L)|Tail], D) :-
	finite_to_discrete([finite(L)|Tail],Ds),
	combine_discrete_distributions(Ds,D).

combine_vals([],[]).
combine_vals([val(X)|Tail],Z) :-
	combine_vals(Tail,Y),
	Z = [X|Y].

finite_to_discrete([],[]).
finite_to_discrete([finite(L)|T1], [discrete(L)|T2]) :-
	finite_to_discrete(T1,T2).

combine_bernoulli([], 1).
combine_bernoulli([bernoulli(P)|T], Q) :- 
	combine_bernoulli(T, Q1),
	Q is Q1*(1-P). 

combine_uniform_distributions([], uniform([])).
combine_uniform_distributions([uniform(X)|Tail], uniform(Z)) :-
	combine_uniform_distributions(Tail, uniform(Y)),
	append(X, Y, Z).

combine_discrete_distributions(Ds,D) :- 
	merge_findn(Ds,Merged,Num), 
	discrete_get_probs_vals(Merged,_,V),
	list_to_set(V,Unique),
	sum_items(Unique,Merged,Num,R), 
	D = discrete(R).

merge_findn([],[],0).
merge_findn([discrete(L)|Tail],Merged,Num) :-
	merge_findn(Tail,M,N), 
	Num is N + 1,
	merge_list(L,M,Merged).

merge_list([],L,L).
merge_list([H|T],L,[H|M]) :-
    merge_list(T,L,M).

sum_items([],_,_,[]).
sum_items([H|T],L,N,R) :-
	sum_items(T,L,N,R1), 
	sum_items1(L,H,Sum), 
	Avg is Sum/N, 
	R=[Avg:H|R1].

sum_items1([],_,0).
sum_items1([P:V|T],Val,S) :-
	sum_items1(T,Val,S1), 
	( V=Val -> 
		S is S1 + P
	;	S is S1 + 0
	).

combine_gaussian_distributions(Ds,D) :-
	combining_rule(X),
	( (X=0; X=1) ->
		combine_gaussian_distributions_rule1(Ds,D)
	;	( (X=0; X=2) ->
			combine_gaussian_distributions_rule2(Ds,D)
		;	throw('Invalid combining rule')
		)
	).

combine_gaussian_distributions_rule1(Ds,D) :-
	length(Ds,N), 
	W is 1/N, 
	combine_gaussian_distributions1(Ds,Ms,Vs,W,Ws), 
	D = gaussian_mixture(Ms,Vs,Ws).

combine_gaussian_distributions1([],[],[],_,[]).
combine_gaussian_distributions1([gaussian(M,V)|T1],[M|T2],[V|T3],W,[W|T4]) :-
	combine_gaussian_distributions1(T1,T2,T3,W,T4).

combine_gaussian_distributions_rule2(Ds,D) :-
	sum_mean_variance(Ds,MSum,VSum,N),
	MAvg is MSum/N,
	VAvg is VSum/(N*N),
	D = gaussian(MAvg,VAvg).

sum_mean_variance([],0,0,0).
sum_mean_variance([gaussian(M,V)|Tail],MSum,VSum,N) :- 
	sum_mean_variance(Tail,MSum1,VSum1,N1),
	MSum is MSum1 + M,
	VSum is VSum1 + V,
	N is N1 + 1.

/* Utilities */
recorded_top(X) :- 
	term_hash(top(X),H), 
	recorded(H, top(X)).

record_top(X) :- 
	term_hash(top(X),H), 
	recordz(H, top(X)).

recorded_bottom(X) :- 
	term_hash(bottom(X),H), 
	recorded(H, bottom(X)).

record_bottom(X) :- 
	term_hash(bottom(X),H), 
	recordz(H, bottom(X)).

recorded_assigned(X,Y) :- 
	term_hash(assigned(X),H), 
	recorded(H, assigned(X,Y)), 
	!.

record_assigned(X,Y) :- 
	term_hash(assigned(X),H), 
	recordz(H, assigned(X,Y)), 
	recordz(assigned, X).

all_assigned(L) :- 
	findall(X~=Y, (recorded(assigned,X), recorded_assigned(X,Y)), L).

recorded_possible_predictive_evd(X) :- 
	term_hash(possible_predictive_evd(X),H), 
	recorded(H,possible_predictive_evd(X)), 
	!.

record_possible_predictive_evd(X) :- 
	term_hash(possible_predictive_evd(X),H),
	recordz(possible_predictive_evd,X), 
	recordz(H,possible_predictive_evd(X)).

all_possible_predictive_evd(L) :- 
	findall(X,(recorded(possible_predictive_evd,X), 
	\+recorded_diagnostic_evidence(X,_,_)),L).

recorded_diagnostic_evidence(X,Y,W) :- 
	term_hash(diagnostic_evidence(X),H),
	recorded(H,diagnostic_evidence(X,[Y,W])), 
	!.

record_diagnostic_evidence(X,Y,W) :- 
	term_hash(diagnostic_evidence(X),H),
	recordz(diagnostic_evidence,X), 
	recordz(H,diagnostic_evidence(X,[Y,W])).

all_diagnostic_evidence(L) :- 
	findall((X,W), (recorded(diagnostic_evidence,X), recorded_diagnostic_evidence(X,_,W)), L).

set_combining_rule(N) :-
	( combining_rule(_) ->
		retract(combining_rule(_)), 
		asserta(combining_rule(N))
	; 	asserta(combining_rule(N)) 
	).

set_sample_size(N) :- 
	( sample_size(_) ->
		retract(sample_size(_)), 
		asserta(sample_size(N))
	; 	asserta(sample_size(N)) 
	).

set_rv(X) :- 
	( has_rv(X) ->
		true
	;	asserta(has_rv(X))
	).

display_transformed_rules(N) :-
	( trans_rules(_) ->
		retract(trans_rules(_)), 
		asserta(trans_rules(N))
	; 	asserta(trans_rules(N)) 
	).
	
set_debug(N) :- 
	( debug(_) ->
		retract(debug(_)), 
		asserta(debug(N))
	; 	asserta(debug(N)) 
	).

set_residual_approach(X) :- 
	( approach(_) ->
		retract(approach(_)), 
		asserta(approach(X))
	; 	asserta(approach(X)) 
	).

set_default(N) :- 
	( ((has_rv(discrete); has_rv(gaussian); has_rv(constraints); has_rv(uniform); has_rv(poisson)), N=1) ->
		throw('Default feature is supported only for Bernoulli distributions')
	;	( default(_) ->
			retract(default(_)), 
			asserta(default(N))
		; 	asserta(default(N)) 
		)
	).

set_time_out(N) :- 
	( time_out(_) ->
		retract(time_out(_))
	; 	true
	),  
	get_time(O), 
	M is O + N, 
	asserta(time_out(M)).

check_sample_size :- 
	\+sample_size(_), 
	writeln('Set sample size first.'), 
	fail.

check_sample_size :- 
	sample_size(N), 
	N<1, 
	writeln('Sample size should be greater than zero.'), 
	fail.

check_sample_size :- 
	sample_size(N), 
	N>0.

check_debug :- 
	\+check_debug_fail.

check_debug_fail :- 
	\+debug(1), 
	\+debug(0).

clean :- 
	forall(recorded(_,_,R), erase(R)),
	forall(nb_current(X,_), nb_delete(X)),
	garbage_collect.

record(N) :- 
	all_diagnostic_evidence(V1),
	atom_concat(d,N,K1), 
	nb_setval(K1,V1),
	all_possible_predictive_evd(V2),
	atom_concat(p,N,K2), 
	nb_setval(K2,V2).

list_of_partial_weights(N, List) :- 
	list_of_diagnostic_evd(N,L1), 
	findall(W, (between(1,N,K), atom_concat(d,K,Key), nb_getval(Key,L2), prepare_list(L1,L2,W)), List).

list_of_full_weights_and_residuals(N, LDE, List, Res) :- 
	list_of_full_weights_and_residuals1(N, LDE, List, Res).

list_of_full_weights_and_residuals(N, List, Res) :-
	list_of_diagnostic_evd(N,LDE),
	list_of_full_weights_and_residuals1(N, LDE, List, Res).

list_of_full_weights_and_residuals1(N, L1, List, Res) :-
	garbage_collect,
	findall((W,R), (between(1,N,K), atom_concat(d,K,Key), nb_getval(Key,L2), forall(recorded(_,_,Ref), erase(Ref)), fill_residuals(L1,L2,W,R,0)), WR), 
	seperate_w_r(WR, List, Res).

seperate_w_r([],[],[]).
seperate_w_r([(W,R)|T], List, Res) :- 
	seperate_w_r(T,List1,Res1), 
	List=[W|List1], 
	Res=[R|Res1].

fill_residuals([],_,[],[],_).
fill_residuals([H|T],Partial,Weight,Res,C) :- 
	C1 is C+1, 
	fill_residuals(T,Partial,Weight1,Res1,C1), 
	(is_member(H,Partial,W) -> 
		Weight=[W|Weight1], 
		Res=Res1
	; 	weight_residual(H,W1), 
		Weight=[W1|Weight1], 
		Res=[C|Res1]
	).

weight_residual(X,W) :- 
	P=distributional(X), 
	prove_all(P), 
	evidence(X~=Y), 
	weight(X,Y,W).


list_of_diagnostic_evd(N,List_out) :- 
	N>0, 
	N1 is N-1, 
	list_of_diagnostic_evd(N1,List_out1), 
	atom_concat(d,N,Key), 
	nb_getval(Key,L1), 
	only_evd(L1,L), 
	union(L,List_out1,List_out).
list_of_diagnostic_evd(0,[]).

only_evd([],[]).
only_evd([(E,_)|T],[E|T1]) :- 
	only_evd(T,T1).


list_of_predictive_evd(N,DEvd,PEvd) :- 
	list_of_predictive_evd1(N,PEvd1), 
	subtract(PEvd1,DEvd,PEvd).

list_of_predictive_evd1(N,List_out) :- 
	N>0, 
	N1 is N-1, 
	list_of_predictive_evd1(N1,List_out1), 
	atom_concat(p,N,Key), 
	nb_getval(Key,L), 
	union(L,List_out1,List_out).

list_of_predictive_evd1(0,[]).


prepare_list([],_,[]).
prepare_list([H|T],Partial,Weight) :- 
	prepare_list(T,Partial,Weight1), 
	( is_member(H,Partial,W) -> 
		Weight=[W|Weight1]
	; 	Weight=[-1|Weight1]
	).

is_member(X,[(X,W)|_],W).
is_member(X,[_|L],W) :- 
	is_member(X,L,W).

tuple_to_list(A,[A]) :- 
	\+(A=(_,_)), 
	!.
tuple_to_list((A,R), [A,B,C|L]) :- 
	R = (B,_), 
	!, 
	tuple_to_list(R,[B,C|L]). 
tuple_to_list((A,B), [A,B]).

add_evidence([]).
add_evidence([H|T]) :- 
	add_evidence(T),
	( evidence(H) -> 
		true
	;	assertz(evidence(H))
	).

add_intervention([]).
add_intervention([H|T]) :- 
	add_intervention(T),
	( intervention(H) -> 
		true
	;	assertz(intervention(H))
	).


remove_evidence([]).
remove_evidence([H|T]) :- 
	retract(evidence(H)), 
	remove_evidence(T).

remove_intervention([]).
remove_intervention([H|T]) :- 
	remove_intervention(T), 
	retract(intervention(H)).


probability(L,P) :- 
	count_true(L,M), 
	sample_size(N), 
	P is M/N.


probability(L,EW,P) :- 
	compute_prob(L,EW,P).

count_true([],0).
count_true([H|T],M1) :- 
	count_true(T, M), 
	( H=1 -> 
		M1 is M+1
	; 	M1 is M
	).

analyze_sample(_,0).
analyze_sample(N,1) :- 
	atom_concat('Sample# ',N,S), 
	writeln(''),
	writeln(S), 
	writeln('-------'), 
	writeln(''),
	writeln('partial assignments:'),
	all_assigned(L1), 
	writeln(L1), 
	writeln(''),
	writeln('diagnostic evidence in this sample:'),
	all_diagnostic_evidence(L3), 
	writeln(L3), 
	writeln(''),
	writeln('predictive evidence in this sample:'),
	all_possible_predictive_evd(L2), 
	writeln(L2), 
	writeln('').

analyze_evidence(N,PW,EW,L,1) :- 
	writeln(''), 
	writeln('-------'), 
	writeln('Summary'), 
	writeln('-------'), 
	writeln('diagnostic evidence:'),
	list_of_diagnostic_evd(N,DEvd), 
	writeln(DEvd), 
	writeln(''),
	writeln('predictive evidence:'),
	list_of_predictive_evd(N,DEvd,PEvd), 
	writeln(PEvd), 
	writeln(''),
	write('partial weights = '), 
	writeln(PW), 
	writeln(''),
	write('expected likelihood weights = '), 
	writeln(EW), 
	writeln(''),
	write('entailments of query = '), 
	writeln(L), 
	writeln('').
analyze_evidence(_,_,_,_,0).


