# DC+
A hybrid probabilistic logic programming language.

The older version of DC written by Davide Nitti is available here: [DC](https://github.com/davidenitti/DC)

This new version is written from scratch. Now, it supports [combining rules](https://link.springer.com/article/10.1007/s10472-009-9138-5) and the inference engine exploits [context-specific independencies](http://proceedings.mlr.press/v130/kumar21b/kumar21b.pdf) in DC+ programs.

It is possible to write (acyclic) ProbLog programs without Annotated Disjunction in this version. 

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
   $ swipl -s dc+_example.pl
```

SWI-Prolog should now be opened without any error or warnings.
First, set the number of samples.
```
   ?- set_sample_size(1000).
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

Help 
====

If you need help or find a bug please write an [issue](https://github.com/niteshroyal/DC-Plus/issues) or contact me at nitesh.kr (_DOT_) 369 (AT) gmail (_DOT_) com

