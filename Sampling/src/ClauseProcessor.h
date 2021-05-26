/*
 * ClauseProcessor.h
 *
 *  Created on: Jan 8, 2021
 *      Author: nitesh
 */

#ifndef SRC_CLAUSEPROCESSOR_H_
#define SRC_CLAUSEPROCESSOR_H_

#include <set>
#include <iostream>
#include <vector>
#include <exception>
#include <algorithm>
#include <forward_list>

using namespace std;

struct EmptyBodyOfClause : public exception {
   const char * what () const throw () {
      return "\nBody of a clause can not be empty!\n";
   }
};

struct horn_clause {
	vector<int> head;
	vector<int> body;
	int false_id;
};

class ClauseProcessor {
private:
	forward_list<int> unit_atoms;
	vector<horn_clause> horn_clauses;
	vector<vector<int>> domains;
	vector<vector<vector<int>>> cnfs;
public:
	void put_unit_atoms(int id);
	void remove_unit_atoms(int id);
	void put_clause(vector<int> head, vector<int> body, int false_id);
	void put_domain(vector<int> domain);
	void process_clauses();
	void print_cnfs();
	vector<vector<vector<int>>> &get_cnfs();
};

int test_clause_processor();

#endif /* SRC_CLAUSEPROCESSOR_H_ */
