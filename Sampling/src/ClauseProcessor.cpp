/*
 * ClauseProcessor.cpp
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

#include "ClauseProcessor.h"

void ClauseProcessor::put_unit_atoms(int id) {
	unit_atoms.push_front(id);
}

void ClauseProcessor::remove_unit_atoms(int id) {
	unit_atoms.remove(id);
}

void ClauseProcessor::put_domain(vector<int> domain) {
	domains.push_back(domain);
}

void ClauseProcessor::print_cnfs() {
	cout<< "-- Printing cnfs --" << endl;
	for(auto cnf: cnfs) {
		for(auto l: cnf) {
			if(l[1] == 0) {
				cout << "-";
			}
			cout << l[0] << " ";
		}
		cout << endl;
	}
	cout << "-------------" << endl;
}

void ClauseProcessor::put_clause(vector<int> head, vector<int> body, int false_id) {
	horn_clause c;
	c.head = head;
	c.body = body;
	c.false_id = false_id;
	if(body.size() > 0) {
		horn_clauses.push_back(c);
	} else{
		throw EmptyBodyOfClause();
	}
}

vector<vector<vector<int>>> &ClauseProcessor::get_cnfs() {
	return cnfs;
}

void cartesian_recurse(vector<vector<vector<int>>> &accum, vector<int> &stack,
    vector<horn_clause*> &sequences, int index) {
    for (int i : sequences[index]->body)
    {
        stack.push_back(i);
        if (index == 0) {
        	vector<vector<int>> stack1;
        	set<int> s;
        	unsigned size = stack.size();
        	for(unsigned j = 0; j < size; ++j) {
        		s.insert(stack[j]);
        	}
        	for(int j: s) {
        		vector<int> l = {j, 1};
        		stack1.push_back(l);
        	}
        	accum.push_back(stack1);
        }
        else {
        	cartesian_recurse(accum, stack, sequences, index - 1);
        }
        stack.pop_back();
    }
}

void cartesian_product(vector<vector<vector<int>>> &accum, vector<horn_clause*> &sequences) {
    vector<int> stack;
    if (sequences.size() > 0) {
    	cartesian_recurse(accum, stack, sequences, sequences.size() - 1);
    }
}

void ClauseProcessor::process_clauses() {
	cnfs.clear();
	for(vector<int> &domain: domains) {
		for(int id: domain) {
			vector<horn_clause*> match;
			for(horn_clause &c: horn_clauses) {
				if(find(c.head.begin(), c.head.end(), id) != c.head.end()) {
					match.push_back(&c);
				}
			}
			vector<vector<vector<int>>> cartP;
			cartesian_product(cartP, match);
			for(vector<vector<int>> cnf: cartP) {
				vector<int> l = {id, 0};
				cnf.push_back(l);
				cnfs.push_back(cnf);
			}
		}
	}
	for(horn_clause &c: horn_clauses) {
		vector<vector<int>> cnf;
		for(int id: c.body) {
			vector<int> l = {id, 0};
			cnf.push_back(l);
		}
		for(int id: c.head) {
			vector<int> l = {id, 1};
			cnf.push_back(l);
			if(c.false_id != -1) {
				vector<int> l = {c.false_id, 1};
				cnf.push_back(l);
			}
		}
		cnfs.push_back(cnf);
	}
	for(vector<int> &domain: domains) {
		unsigned size = domain.size();
		for(unsigned i=0; i<size; ++i) {
			for(unsigned j=i; j<size; ++j) {
				if(i!=j) {
					vector<vector<int>> cnf = {{domain[i], 0}, {domain[j], 0}};
					cnfs.push_back(cnf);
				}
			}
		}
	}
	for(int id: unit_atoms) {
		vector<vector<int>> cnf = {{id, 1}};
		cnfs.push_back(cnf);
	}
	print_cnfs();
}

int test_clause_processor() {
	ClauseProcessor *pobj = new ClauseProcessor();

	vector<int> head = {6};
	vector<int> body = {1};
	pobj->put_clause(head, body, 5);
	head = {6};
	body = {2};
	pobj->put_clause(head, body, -1);
	head = {8};
	body = {1};
	pobj->put_clause(head, body, -1);
	head = {7, 9};
	body = {2};
	pobj->put_clause(head, body, -1);
	head = {4};
	body = {1, 6};
	pobj->put_clause(head, body, 3);
	head = {10};
	body = {3, 9};
	pobj->put_clause(head, body, -1);
	head = {10, 11, 12, 13};
	body = {5, 8};
	pobj->put_clause(head, body, -1);
	head = {12, 13};
	body = {3, 7, 6};
	pobj->put_clause(head, body, -1);
	head = {13};
	body = {4};
	pobj->put_clause(head, body, -1);

	vector<int> domain = {1, 2};
	pobj->put_domain(domain);
	domain = {3,4};
	pobj->put_domain(domain);
	domain = {5,6};
	pobj->put_domain(domain);
	domain = {7,8,9};
	pobj->put_domain(domain);
	domain = {10,11,12,13};
	pobj->put_domain(domain);

	pobj->put_unit_atoms(12);

	pobj->process_clauses();
	pobj->print_cnfs();

	return 0;
}
