/*
 * sampler.h
 *
 *  Created on: Feb 5, 2020
 *      Author: nitesh
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
	 * Sample from weighted mixture of distributions.
	 */
	double sample_mixture_of_gaussians(vector<double> means, vector<double> stds, vector<double> weights, int num);

	/**
	 * Get the likelihood weight for a point from Gaussian distribution with mean m and standard deviation std.
	 */
	double weight_gaussian_dist(double m, double std, double x);

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
