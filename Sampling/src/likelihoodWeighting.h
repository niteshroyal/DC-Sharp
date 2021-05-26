/*
 * likelihoodWeighting.h
 *
 *  Created on: Feb 6, 2020
 *      Author: nitesh
 */
#ifndef SRC_LIKELIHOODWEIGHTING_H_
#define SRC_LIKELIHOODWEIGHTING_H_

#include <map>
#include <vector>
#include <exception>
#include <iostream>
#include <string>
#include <algorithm>

using namespace std;

double covariance(vector<int> left, vector<int> right);
double covariance_approach1(vector<int> left, vector<int> right);
double covariance_approach2(vector<int> left, vector<int> right);
double expectation(vector<int> missingCols);
double expectation_unbiased(vector<int> missingCols);
double* expectedWeightOfWorlds();
double* expectedLW(vector<vector<double> > partialWeights, int aprch);
double* expectedLW(vector<vector<double> > weights, vector<vector<int> > missing_cols);
const vector<string> explode(const string& s, const char& c);
void print(std::vector <int> const &a);

#endif /* SRC_LIKELIHOODWEIGHTING_H_ */
