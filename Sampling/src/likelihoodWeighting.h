/*
 * likelihoodWeighting.h
 *
 * Copyright (C) 2020, 2021 Nitesh Kumar
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
