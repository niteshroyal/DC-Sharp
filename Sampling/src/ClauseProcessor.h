/*
 * ClauseProcessor.h
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
