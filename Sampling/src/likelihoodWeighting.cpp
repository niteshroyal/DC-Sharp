/*
 * likelihoodWeighting.cpp
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

/*
 *  A dynamic programming based approach to estimate weights of worlds from the partial weights.
 */
#include "likelihoodWeighting.h"

/*
 * Global variables
 */
double** inputMatrix;
int mRows;
int mCols;
int approach;
map<vector<int>, double> tabledExpectations;

struct DivisionByZero : public exception {
   const char * what () const throw () {
      return "Division by zero! Problem (maybe) with the input matrix, it contains empty column!";
   }
};

struct EmptyMatrix : public exception {
   const char * what () const throw () {
      return "input matrix empty!";
   }
};

void print(std::vector <int> const &a) {
   std::cout << "The vector elements are : ";

   for(int i=0; i < a.size(); i++)
   std::cout << a[i] << ' ';
}

double covariance(vector<int> left, vector<int> right) {
	if(approach == 1) {
		return covariance_approach1(left, right);
	} else {
		return covariance_approach2(left, right);
	}
}

/*
 * Implements the definition of covariance.
 * Slow but (probably) more accurate.
 */
double covariance_approach1(vector<int> left, vector<int> right) {
	double lWeight=1;
	double rWeight=1;
	double expectationOfLeft = expectation(left);
	double expectationOfRight = expectation(right);
	int count = 0;
	double covariance = 0;
	for(int i=0; i<mRows; ++i) {
		if(inputMatrix[i][left[0]] != -1) {
			lWeight = inputMatrix[i][left[0]];
			rWeight = 1;
			vector<int> missingCols;
			for(unsigned int j=0; j<right.size(); ++j) {
				if(inputMatrix[i][right[j]] == -1) {
					missingCols.push_back(right[j]);
				} else {
					rWeight = rWeight * inputMatrix[i][right[j]];
				}
			}
			if (missingCols == right) {
			} else {
				rWeight = rWeight * expectation(missingCols);
				count++;
				covariance += (lWeight - expectationOfLeft)*(rWeight - expectationOfRight);
			}
		} else {
		}
	}
	if(count == 0) {
		return 0;
	} else {
		return covariance/count;
	}
}

/*
 * Implements the definition of covariance.
 * Fast but (probably) less accurate.
 */
double covariance_approach2(vector<int> left, vector<int> right) {
	double lWeight=1;
	double rWeight=1;
	double expectationOfLeft = expectation(left);
	double expectationOfRight = expectation(right);
	int count = 0;
	double covariance = 0;
	for(int i=0; i<mRows; ++i) {
		if(inputMatrix[i][left[0]] != -1) {
			lWeight = inputMatrix[i][left[0]];
			rWeight = 1;
			vector<int> missingCols;
			for(unsigned int j=0; j<right.size(); ++j) {
				if(inputMatrix[i][right[j]] == -1) {
					missingCols.push_back(right[j]);
				} else {
					rWeight = rWeight * inputMatrix[i][right[j]];
				}
			}
			if (!missingCols.empty()) {
			} else {
				count++;
				covariance += (lWeight - expectationOfLeft)*(rWeight - expectationOfRight);
			}
		} else {
		}
	}
	count--; // Important for getting unbiased estimate
	if(count < 1) {
		return 0;
	} else {
		return covariance/count;
	}
}

/*
 * Implements the recursive definition of expectation
 * E(X1...Xn) = cov(X1,X2...Xn) + E(X1)E(X2...Xn)
 */
double expectation(vector<int> missingCols) {
	if(missingCols.empty()) {
		return 1;
	} else {
		map<vector<int>, double>::iterator it = tabledExpectations.find(missingCols);
		if(it != tabledExpectations.end())
		{
			return it->second;
		} else if (missingCols.size() == 1) {
			int col = missingCols[0];
			int count = 0;
			double sum = 0;
			for(int i=0; i<mRows; ++i) {
				if(inputMatrix[i][col] != -1) {
					sum += inputMatrix[i][col];
					count++;
				} else{
				}
			}
		   if( count == 0 ) {
			  throw DivisionByZero();
		   }
		   double e = sum/count;
		   tabledExpectations[missingCols] = e;
		   return e;
		} else {
			vector<int> missingColsLeft{missingCols[0]};
			vector<int> missingColsRight = 	missingCols;
			vector<int>::iterator it = missingColsRight.begin();
			missingColsRight.erase(it);
			double productOfExpectations = expectation(missingColsLeft)*expectation(missingColsRight);
			double covar = covariance(missingColsLeft, missingColsRight);
			double e = productOfExpectations + covar;
			tabledExpectations[missingCols] = e;
			return e;
		}
	}
}


double expectation_unbiased(vector<int> missingCols) {
	if(missingCols.empty()) {
		return 1;
	} else {
		map<vector<int>, double>::iterator it = tabledExpectations.find(missingCols);
		if(it != tabledExpectations.end())
		{
			return it->second;
		} else {
			int count = 0;
			double e = 0;
			for(int i=0; i<mRows; ++i) {
				double pweight = 1;
				bool missing = false;
				for(int j=0; j<missingCols.size(); ++j) {
					double temp = inputMatrix[i][missingCols[j]];
					if(temp == -1) {
						missing = true;
						break;
					}
					pweight = pweight * temp;
				}
				if(!missing) {
					e = e + pweight;
					count++;
				}
			}
			if(count == 0) {
				e = 0;
			} else {
				e = e/(double)count;
			}
			tabledExpectations[missingCols] = e;
			return e;
		}
	}
}


double* expectedWeightOfWorlds() {
	double* expectedWeights = new double[mRows];
	for(int i=0; i<mRows; ++i) {
		double weight = 1;
		vector<int> missingCols;
		for(int j=0; j<mCols; ++j) {
			if(inputMatrix[i][j] == -1) {
				missingCols.push_back(j);
			} else {
				weight = weight * inputMatrix[i][j];
			}
		}
		// weight = weight * expectation_unbiased(missingCols);
		weight = weight * expectation(missingCols);
		expectedWeights[i] = weight;
	}
	return expectedWeights;
}


double* expectedLW(vector<vector<double> > weights, vector<vector<int> > missing_cols) {
	tabledExpectations.clear();
	if(weights.size() == 0) {
		throw EmptyMatrix();
	} else {
		mRows = weights.size();
		mCols = weights[0].size();
		inputMatrix = new double*[mRows];
		for(int i=0; i<mRows; ++i) {
			inputMatrix[i] = new double[mCols];
			for(int j=0; j<mCols; ++j) {
				inputMatrix[i][j] = weights[i][j];
			}
		}
		approach = 4;
		double* result = new double[mRows];
		for(int i=0; i<mRows; ++i) {
			double weight = 1;
			for(int j=0; j<mCols; ++j) {
				if (std::binary_search(missing_cols[i].begin(), missing_cols[i].end(), j)) {
				} else {
					weight = weight * inputMatrix[i][j];
				}
			}
			weight = weight * expectation_unbiased(missing_cols[i]);
			result[i] = weight;
		}
		return result;
	}
}

double* expectedLW(vector<vector<double> > partialWeights, int aprch) {
	tabledExpectations.clear();
	if(partialWeights.size() == 0) {
		throw EmptyMatrix();
	} else {
		mRows = partialWeights.size();
		mCols = partialWeights[0].size();
		inputMatrix = new double*[mRows];
		for(int i=0; i<mRows; ++i) {
			inputMatrix[i] = new double[mCols];
			for(int j=0; j<mCols; ++j) {
				inputMatrix[i][j] = partialWeights[i][j];
			}
		}
		approach = aprch;
		double *result = expectedWeightOfWorlds();
		return result;
	}
}

const vector<string> explode(const string& s, const char& c)
{
	string buff{""};
	vector<string> v;
	for(auto n:s)
	{
		if(n != c) buff+=n; else
		if(n == c && buff != "") { v.push_back(buff); buff = ""; }
	}
	if(buff != "") v.push_back(buff);
	return v;
}

int test() {
    vector<vector<double> > partialWeights {
    	{0.1, -1, -1, 0.7, -1},
	{-1, 0.2, -1, -1, -1},
	{0.3, 0.9, -1, 0.2, -1},
	{-1, -1, 0.5, 0.4, 0.6},
	{-1, 0.1, 0.7, -1, -1},
	{-1, 0.2, 0.8, 0.3, 0.4}
    };
    double* result = expectedLW(partialWeights, 1);
	for (unsigned int i = 0; i < partialWeights.size(); i++) {
		cout << result[i] << ' ';
	}
	cout << endl;
	return 0;
}

