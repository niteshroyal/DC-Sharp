/*
 * sampler.cpp
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
 */

#include "sampler.h"
#include <ctime>
#include <cmath>

#define Abs(x)    ((x) < 0 ? -(x) : (x))
#define Max(a, b) ((a) > (b) ? (a) : (b))

struct WeightsNotSumToOne : public exception {
   const char * what () const throw () {
      return "mixture of Gaussians: weights do not sum to one!";
   }
};

struct LessThanOne : public exception {
   const char * what () const throw () {
      return "uniform distribution: input integer less than 1";
   }
};

sampler::sampler() {
	const gsl_rng_type * t;
	gsl_rng_env_setup();
	t = gsl_rng_default;
	generator = gsl_rng_alloc(t);
}

double RelDif(double a, double b)
{
	double c = Abs(a);
	double d = Abs(b);
	d = Max(c, d);
	return d == 0.0 ? 0.0 : Abs(a - b) / d;
}

bool sampler::double_equals(double a, double b)
{
	double tolerance = 0.000001;
	return RelDif(a, b) <= tolerance;
}

void sampler::set_seed(unsigned long int s) {
	gsl_rng_set(generator, s);
}

sampler::~sampler() {
	gsl_rng_free(generator);
}

double sampler::sample_uniform_dist(double a, double b) {
	return gsl_ran_flat(generator, a, b);
}

int sampler::sample_poisson_dist(double mu) {
	return gsl_ran_poisson(generator, mu);
}

double sampler::weight_poisson_dist(double mu, int k) {
	return gsl_ran_poisson_pdf(k, mu);
}

int sampler::sample_uniform_int(int a) {
	int res;
	if(a < 1) {
		LessThanOne();
	} else {
		double k = sampler::sample_uniform_dist(0, a);
		for(int i=1; i<=a; ++i) {
			if(k <= i) {
				res = i;
				break;
			}
		}
	}
	return res;
}

double sampler::weight_uniform_dist(double a, double b, double x) {
	return gsl_ran_flat_pdf(x, a, b);
}

double sampler::sample_gaussian_dist(double m, double std) {
	double s = gsl_ran_gaussian(generator, std);
	return s+m;
}

double sampler::sample_mixture_of_gaussians(vector<double> means, vector<double> stds, vector<double> weights, int num) {
	if(num == 1) {
		if(!sampler::double_equals((double)weights[0], 1.0)) {
			throw WeightsNotSumToOne();
		}
		return sampler::sample_gaussian_dist((double)means[0], (double)stds[0]);
	} else {
		double* acc_weights = new double[num];
		double acc = 0;
		for(int i=0; i<num; ++i) {
			acc = acc + weights[i];
			acc_weights[i] = acc;
		}
		if(!sampler::double_equals(acc, 1.0)) {
			throw WeightsNotSumToOne();
		}
		double r = sampler::sample_uniform_dist(0, 1);
		int k = num-1;
		for(int i=0; i<num; ++i) {
			if(r < acc_weights[i]) {
				k = i;
				break;
			}
		}
		return sampler::sample_gaussian_dist((double)means[k], (double)stds[k]);
	}
}

double sampler::weight_gaussian_dist(double m, double std, double x) {
	x = x-m;
	return gsl_ran_gaussian_pdf(x, std);
}

double sampler::weight_mixture_of_gaussians(vector<double> means, vector<double> stds, vector<double> weights, double x, int num) {
	if(num == 1) {
		if(!sampler::double_equals((double)weights[0], 1.0)) {
			throw WeightsNotSumToOne();
		}
		return sampler::weight_gaussian_dist((double)means[0], (double)stds[0], x);
	} else {
		double acc = 0, res = 0;
		for(int i=0; i<num; ++i) {
			res = res + sampler::weight_gaussian_dist((double)means[i], (double)stds[i], x) * weights[i];
			acc = acc + weights[i];
		}
		if(!sampler::double_equals(acc, 1.0)) {
			throw WeightsNotSumToOne();
		}
		return res;
	}
}

unsigned int sampler::sample_discrete_dist(unsigned int size, double* p) {
	gsl_ran_discrete_t* temp =  gsl_ran_discrete_preproc(size, p);
	return gsl_ran_discrete(generator, temp);
}

unsigned int sampler::sample_bernoulli_dist(double p) {
	return gsl_ran_bernoulli(generator, p);
}

double sampler::weight_bernoulli_dist(unsigned int k, double p) {
	return gsl_ran_bernoulli_pdf(k, p);
}


int test1() {
	int i;
	sampler *samplerObj = new sampler();
	int x;
	clock_t begin = clock();
	for(i=0; i<10; i++) {
		x = (int)samplerObj->sample_bernoulli_dist(0.9);
	}
	clock_t end = clock();
	double elapsed_secs = double(end - begin) / CLOCKS_PER_SEC;
	cout << "Time elapsed in secs: " << elapsed_secs <<"\n";
	return 0;
}
