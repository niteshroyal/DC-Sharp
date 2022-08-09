# DC#
DC# is a hybrid probabilistic logic programming (PLP) language.

It integrates the syntax of distributional clauses ([DCs](https://github.com/davidenitti/DC)) and the ideas of Bayesian Logic Programs ([BLPs](http://people.csail.mit.edu/kersting/papers/srl05chapter.pdf)).

It supports the [combining rules](https://link.springer.com/article/10.1007/s10472-009-9138-5), which are often required to express relational models. Combining rules are essential components of PLPs, which were not supported by the [DC](https://github.com/davidenitti/DC) system.

Compared to the DC system, the inference engine of DC# is much more efficient. The inference engine now properly exploits the following three types of independencies:
1) Conditional independencies ([CIs](https://www.sciencedirect.com/science/article/pii/B978044488738250018X)), which are elegantly modeled in Bayesian networks.
2) Context-specific independencies ([CSIs](https://arxiv.org/pdf/1302.3562.pdf)), which are naturally represented by logical rules.
3) Independence of causal influences ([ICIs](https://www.jair.org/index.php/jair/article/view/10178)), i.e.,  independencies amongst attributes of related objects in relational models succinctly expressed by combining rules.

However, DC# currently does not support writing the following models, which were supported by the [DC](https://github.com/davidenitti/DC) system:
1) [Dynamic probabilistic models](https://en.wikipedia.org/wiki/Dynamic_Bayesian_network) that were expressed using Dynamic Distributional Clauses (DDCs) in the DC system.
2) [Probabilistic models with unknown objects](https://people.eecs.berkeley.edu/~russell/papers/pearlbook10-blog.pdf).

It is possible to write (acyclic) [ProbLog](https://dtai.cs.kuleuven.be/problog/) programs without Annotated Disjunction in DC#. 

More details about DC# can be found in our [submitted JAIR paper](https://arxiv.org/pdf/2201.11165.pdf). 

The inference engine of DC# upgrades context-specific likelihood weighting ([CS-LW](http://proceedings.mlr.press/v130/kumar21b/kumar21b.pdf)) to first-order programs.

The code is in beta, if you need help or find a bug please write an issue or contact me at nitesh (DOT) kr369 (AT) gmail (DOT) com


Installation
============

This installation manual is tested on Ubuntu 18.04.4 LTS and 16.04 LTS.


1. Install dependencies
```
    $ sudo apt install build-essential pkg-config libgsl-dev
```

2. Install latest version of SWI-Prolog (version above 8.1.30 is required)
```
    $ sudo apt-add-repository ppa:swi-prolog/devel
    $ sudo apt-get update
    $ sudo apt-get install swi-prolog
```

3. Build 
```
    $ cd Sampling
    $ make clean
    $ make all
```

Execution 
=========

Go to Examples folder
```
   $ cd ../Examples/
```

You will find examples of DC programs. Notice the syntax of DC programs. Open one example in SWI-Prolog.
```
   $ swipl -s dc#_example.pl
```

SWI-Prolog should now be opened without any error or warnings.
First, set the number of samples.
```
   ?- set_sample_size(10000).
```

Second, turn off the debug mode (1 to turn on the debug mode).
```
   ?- set_debug(0).
```

Now query. The first argument is the query, and second is the list of evidence, and P is the output probability
```
   ?- query((status(l_1)~=X, X==appr), [credit_score(bob)~=600, balance(a_1)~=50010], P).
```

Initialization Options 
======================

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
		1: noisy-or for boolean, mixture for discrete, mixture for gaussian, none for val, none for uniform
		2: noisy-or for boolean, mixture for discrete, noisy-avg for gaussian, none for val, none for uniform
	set_residual_approach/1:
                bayes_ball: set of all diagnostic evidence is computed using Bayes ball algorithm and then residuals are computed
                full: set of diagnostic evidence is estimated from samples and then residuals are computed
                biased: residuals are not computed. This approach gives biased estimates.


Help 
====

If you need help or find a bug please write an [issue](https://github.com/niteshroyal/DC-Plus/issues) or contact me at nitesh.kr (_DOT_) 369 (AT) gmail (_DOT_) com

