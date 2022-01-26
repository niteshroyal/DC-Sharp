/*
 * expectedlw.cpp
 *
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
 *
 */

#include <iostream>
#include <cmath>
#include "SWI-cpp.h"
#include "sampler.h"
#include "stdlib.h"
#include "likelihoodWeighting.h"
#include "UnitPropagation.h"
#include "ClauseProcessor.h"
#include <string>
#include <iomanip>

using namespace std;

sampler* samplerObj;
UnitPropagation* working_prop_obj;
ClauseProcessor* pobj;

struct ListSizeMismatch : public exception {
   const char * what () const throw () {
      return "size of lists are not same!";
   }
};

struct ProbNotSumToOne : public exception {
   const char * what () const throw () {
      return "discrete: probabilities do not sum to one!";
   }
};

struct ItemNotPresentInList : public exception {
   const char * what () const throw () {
      return "discrete: the item is not present in the list!";
   }
};

void print(std::vector <double> const &a) {
   std::cout << "The vector elements are : ";

   for(int i=0; i < a.size(); i++)
   std::cout << a[i] << ' ';
}

PREDICATE(connect_sampler, 1) {
	int flag = (int)A1;
	if (flag == 1) {
		samplerObj = new sampler();
	} else {
		delete(samplerObj);
	}
	return TRUE;
}

PREDICATE(clause_processor, 1) {
	int flag = (int)A1;
	if (flag == 1) {
		pobj = new ClauseProcessor();
	} else {
		delete(pobj);
	}
	return TRUE;
}

PREDICATE(set_sampler_seed, 1) {
	int seed = (int)A1;
	samplerObj->set_seed(seed);
	return TRUE;
}

PREDICATE(sample_gaussian, 3) {
	double mean = (double)A1;
	double variance = (double)A2;
	return A3 = samplerObj->sample_gaussian_dist(mean, sqrt(variance));
}

PREDICATE(sample_gamma, 3) {
	double a = (double)A1;
	double b = (double)A2;
	return A3 = samplerObj->sample_gamma_dist(a, b);
}

PREDICATE(sample_beta, 3) {
	double a = (double)A1;
	double b = (double)A2;
	return A3 = samplerObj->sample_beta_dist(a, b);
}

PREDICATE(sample_uniform, 3) {
	double a = (double)A1;
	double b = (double)A2;
	return A3 = samplerObj->sample_uniform_dist(a, b);
}

PREDICATE(sample_uniform, 2) {
	double a = (double)A1;
	return A2 = samplerObj->sample_uniform_int(a);
}

PREDICATE(sample_poisson, 2) {
	double mu = (double)A1;
	return A2 = samplerObj->sample_poisson_dist(mu);
}

PREDICATE(weight_poisson, 3) {
	return A3 = samplerObj->weight_poisson_dist((double)A1, (int)A2);
}

PREDICATE(weight_gaussian, 4) {
	double mean = (double)A1;
	double variance = (double)A2;
	double x = (double)A3;
	return A4 = samplerObj->weight_gaussian_dist(mean, sqrt(variance), x);
}

PREDICATE(weight_gamma, 4) {
	double a = (double)A1;
	double b = (double)A2;
	double x = (double)A3;
	return A4 = samplerObj->weight_gamma_dist(x, a, b);
}

PREDICATE(weight_beta, 4) {
	double a = (double)A1;
	double b = (double)A2;
	double x = (double)A3;
	return A4 = samplerObj->weight_beta_dist(x, a, b);
}

PREDICATE(weight_uniform, 4) {
	double a = (double)A1;
	double b = (double)A2;
	double x = (double)A3;
	return A4 = samplerObj->weight_uniform_dist(a, b, x);
}

PREDICATE(sample_bernoulli, 2) {
	double prob = (double)A1;
	return A2 = (int)samplerObj->sample_bernoulli_dist(prob);
}

PREDICATE(sample_discrete, 2) {
	PlTail tail(A1);
	PlTerm e;
	vector<double> probs;
	double prob;
	double sum_prob = 0;
	while(tail.next(e)) {
		prob = (double)e;
		probs.push_back(prob);
		sum_prob += prob;
	}
	if (!samplerObj->double_equals(sum_prob, 1.0)) {
		// print(probs);
		throw ProbNotSumToOne();
	}
	int nRows = probs.size();
	double* p = &probs[0];
	int sampled_val = samplerObj->sample_discrete_dist(nRows, p);
	return A2 = sampled_val;
}

PREDICATE(sample_gaussian_mixture, 4) {
	PlTail tail1(A1);
	PlTerm e;
	vector<double> means;
	while(tail1.next(e)) {
		means.push_back((double)e);
	}
	PlTail tail2(A2);
	vector<double> stds;
	while(tail2.next(e)) {
		stds.push_back(sqrt((double)e));
	}
	PlTail tail3(A3);
	vector<double> weights;
	while(tail3.next(e)) {
		weights.push_back((double)e);
	}
	int num = 0;
	if((means.size() == stds.size()) && (means.size() == weights.size())) {
		num = means.size();
	} else {
		throw ListSizeMismatch();
	}
	double sampled_val = samplerObj->sample_mixture_of_gaussians(means, stds, weights, num);
	return A4 = sampled_val;
}

PREDICATE(weight_gaussian_mixture, 5) {
	PlTail tail1(A1);
	PlTerm e;
	vector<double> means;
	while(tail1.next(e)) {
		means.push_back((double)e);
	}
	PlTail tail2(A2);
	vector<double> stds;
	while(tail2.next(e)) {
		stds.push_back(sqrt((double)e));
	}
	PlTail tail3(A3);
	vector<double> weights;
	while(tail3.next(e)) {
		weights.push_back((double)e);
	}
	int num = 0;
	if((means.size() == stds.size()) && (means.size() == weights.size())) {
		num = means.size();
	} else {
		throw ListSizeMismatch();
	}
	return A5 = samplerObj->weight_mixture_of_gaussians(means, stds, weights, (double)A4, num);
}

/*
PREDICATE(sample_discrete, 2) {
	PlTail tail(A1);
	PlTerm e;
	vector<double> probs;
	vector<string> vals;
	double sum_prob = 0;
	while(tail.next(e)) {
		string s = (char*)e;
		vector<string> prob_val = explode(s, ':');
		double this_prob = atof(prob_val[0].c_str());
		sum_prob += this_prob;
		probs.push_back(this_prob);
		vals.push_back(prob_val[1]);
	}
	if (sum_prob != 1.0) {
		throw ProbNotSumToOne();
	}
	int nRows = probs.size();
	double* p = new double[nRows];
	for(int i=0; i<nRows; ++i) {
		p[i] = probs[i];
	}
	unsigned int sampled_val = samplerObj->sample_discrete_dist(nRows, p);
	return A2 = vals[sampled_val].c_str();
}

PREDICATE(weight_discrete, 3) {
	PlTail tail(A1);
	PlTerm e;
	string compare_val = (char*)A2;
	double weight = -1.0;
	double sum_prob = 0;
	while(tail.next(e)) {
		string s = (char*)e;
		vector<string> prob_val = explode(s, ':');
		double this_prob = atof(prob_val[0].c_str());
		if (compare_val == prob_val[1]) {
			weight = this_prob;
		}
		sum_prob += this_prob;
	}
	if (weight == -1.0) {
		throw ItemNotPresentInList();
	}
	if (sum_prob != 1.0) {
		throw ProbNotSumToOne();
	}
	return A3 = weight;
}
*/

PREDICATE(expected_lw, 2) {
	PlTail tail(A1);
	PlTerm e;
	vector<vector<double>> input;
	while(tail.next(e)) {
		vector<double> inner;
		PlTail tailIn(e);
		PlTerm eIn;
		while(tailIn.next(eIn)) {
			inner.push_back((double)eIn);
		}
		input.push_back(inner);
	}
	int rows = input.size();
	double* result = expectedLW(input, 1); // 0: Traditional way, 1: Non-Traditional
	PlTermv av(1);
	PlTail l(av[0]);
	for(int i=0; i<rows; ++i) {
		l.append(result[i]);
	}
	l.close();
	return A2 = av[0];
}

PREDICATE(load_cnf, 2) {
	PlTail tail(A1);
	PlTerm e;
	vector<vector<vector<int>>> input;
	while(tail.next(e)) {
		vector<vector<int>> inner1;
		PlTail tailIn1(e);
		PlTerm eIn1;
		while(tailIn1.next(eIn1)) {
			vector<int> inner2;
			PlTail tailIn2(eIn1);
			PlTerm eIn2;
			while(tailIn2.next(eIn2)) {
				inner2.push_back((int)eIn2);
			}
			inner1.push_back(inner2);
		}
		input.push_back(inner1);
	}
	int unsat = initialize_prop_obj(input);
	return A2 = unsat;
}

PREDICATE(add_clause, 3) {
	PlTail tail1(A1);
	PlTerm e;
	vector<int> head;
	while(tail1.next(e)) {
		head.push_back((int)e);
	}
	PlTail tail2(A2);
	vector<int> body;
	while(tail2.next(e)) {
		body.push_back((int)e);
	}
	int false_id = (int)A3;
	pobj->put_clause(head, body,false_id);
	return TRUE;
}

PREDICATE(add_domain, 1) {
	PlTail tail(A1);
	PlTerm e;
	vector<int> domain;
	while(tail.next(e)) {
		domain.push_back((int)e);
	}
	pobj->put_domain(domain);
	return TRUE;
}

PREDICATE(add_unit_clause, 1) {
	int unit = (int)A1;
	pobj->put_unit_atoms(unit);
	return TRUE;
}

PREDICATE(remove_unit_clause, 1) {
	int unit = (int)A1;
	pobj->remove_unit_atoms(unit);
	return TRUE;
}

PREDICATE(load_cnf, 1) {
	delete_saved_prop_obj();
	pobj->process_clauses();
	int unsat = initialize_prop_obj(pobj->get_cnfs());
	return A1 = unsat;
}

PREDICATE(reload_cnf, 1) {
	int flag = (int)A1;
	if (flag == 1) {
		delete working_prop_obj;
		working_prop_obj = new UnitPropagation();
	} else {
		delete working_prop_obj;
		delete_saved_prop_obj();
	}
	return TRUE;
}

PREDICATE(put_constraint, 3) {
	int id = (int)A1;
	int val = A2;
	int unsat = working_prop_obj->set_assignment(id, val);
	return A3 = unsat;
}

PREDICATE(get_constraint, 2) {
	int id = (int)A1;
	int val = working_prop_obj->get_assignment(id);
	return A2 = val;
}

PREDICATE(expected_lw, 3) {
	PlTail tail(A1);
	PlTerm e;
	vector<vector<double>> input1;
	while(tail.next(e)) {
		vector<double> inner;
		PlTail tailIn(e);
		PlTerm eIn;
		while(tailIn.next(eIn)) {
			inner.push_back((double)eIn);
		}
		input1.push_back(inner);
	}
	PlTail tail2(A2);
	PlTerm e2;
	vector<vector<int>> input2;
	while(tail2.next(e2)) {
		vector<int> inner;
		PlTail tailIn(e2);
		PlTerm eIn;
		while(tailIn.next(eIn)) {
			inner.push_back((int)eIn);
		}
		input2.push_back(inner);
	}
	int rows = input1.size();
	if(rows != input2.size()) {
		throw ListSizeMismatch();
	} else {
	}
	double* result = expectedLW(input1, input2);
	PlTermv av(1);
	PlTail l(av[0]);
	for(int i=0; i<rows; ++i) {
		l.append(result[i]);
	}
	l.close();
	return A3 = av[0];
}

PREDICATE(compute_prob, 3) {
	PlTail tail1(A1);
	PlTerm e;
	vector<int> entail;
	while(tail1.next(e)) {
		entail.push_back((int)e);
	}
	PlTail tail2(A2);
	PlTerm f;
	vector<double> expectedLW;
	while(tail2.next(f)) {
		expectedLW.push_back((double)f);
	}
	if(entail.size() != expectedLW.size()) {
		throw ListSizeMismatch();
	}
	double nume = 0;
	double deno = 0;
	for(int i=0; i<expectedLW.size(); i++) {
		if(entail[i] == 1) {
			nume += expectedLW[i];
			deno += expectedLW[i];
		} else {
			nume += 0;
			deno += expectedLW[i];
		}
	}
	return A3 = nume/deno;
}

/*
PREDICATE(send_list, 1) {
	//double arr[3][4] = {{2.0, 3.9, 9.0, 2.9}, {4.0, 3.6, 3.0, 1.9}, {8.0, 6.9, 8.0, 2.9}};

	double arr[1][1] = {{3.5}};

	PlTermv av(1);
	PlTail l(av[0]);
	for(int i=0; i<1; i++) {
		PlTermv avIn(1);
		PlTail lIn(avIn[0]);
		for(int j=0; j<1; j++) {
			lIn.append(arr[i][j]);
		}
		lIn.close();
		l.append(avIn[0]);
	}
	l.close();
	return A1 = av[0];
}
*/







