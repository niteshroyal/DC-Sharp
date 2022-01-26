/*
 * sampler.h
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
#ifndef SRC_SAMPLING_H_
#define SRC_SAMPLING_H_

#include <iostream>
#include <vector>
#include "gsl/gsl_rng.h"
#include "gsl/gsl_randist.h"
#include "gsl/gsl_sf_bessel.h"

using namespace std;

class sampler {
private:
	gsl_rng *generator;

public:
	/**
	 * Initialize random number generator
	 */
	sampler();

	/**
	 * Free random number generator
	 */
	~sampler();

	/**
	 * Change seed
	 */
	void set_seed(unsigned long int s);

	/**
	 * Compares two doubles for equality
	 */
	bool double_equals(double a, double b);

	/**
	 * Sample from a uniform distribution from a to b.
	 */
	double sample_uniform_dist(double a, double b);

	/**
	 * Sample uniform int in range [1, a]
	 */
	int sample_uniform_int(int a);

	/**
	 * Get likelihood weight from uniform distribution.
	 */
	double weight_uniform_dist(double a, double b, double x);

	/**
	 * Sample from a Gaussian distribution with mean m and standard deviation std.
	 */
	double sample_gaussian_dist(double m, double std);

	/**
	 * Sample from a Gamma distribution with parameters a and b.
	 */
	double sample_gamma_dist(double a, double b);

	/**
	 * Sample from a Beta distribution with parameters a and b.
	 */
	double sample_beta_dist(double a, double b);

	/**
	 * Sample from a Poisson distribution with mean mu.
	 */
	int sample_poisson_dist(double mu);

	/**
	 * Get the likelihood at a point from a Poisson distribution.
	 */
	double weight_poisson_dist(double mu, int k);

	/**
	 * Sample from weighted mixture of distributions.
	 */
	double sample_mixture_of_gaussians(vector<double> means, vector<double> stds, vector<double> weights, int num);

	/**
	 * Get the likelihood weight for a point from Gaussian distribution with mean m and standard deviation std.
	 */
	double weight_gaussian_dist(double m, double std, double x);

	/**
	 * Get the likelihood weight for a point from Gamma distribution with parameters a and b.
	 */
	double weight_gamma_dist(double x, double a, double b);

	/**
	 * Get the likelihood weight for a point from Beta distribution with parameters a and b.
	 */
	double weight_beta_dist(double x, double a, double b);

	/**
	 * Get the likelihood at a point from weighted mixture of distributions.
	 */
	double weight_mixture_of_gaussians(vector<double> means, vector<double> stds, vector<double> weights, double x, int num);

	/**
	 * Sample from a bernoulli distribution with parameter p.
	 */
	unsigned int sample_bernoulli_dist(double p);

	/**
	 * Get the likelihood weight for a point from a bernoulli distribution with parameter p.
	 */
	double weight_bernoulli_dist(unsigned int k, double p);

	/**
	 * Sample from a multinomial distribution
	 */
	unsigned int sample_discrete_dist(unsigned int size, double* p);

};

#endif /* SRC_SAMPLING_H_ */
