/*
 * unitPropagation.cpp
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

#include "UnitPropagation.h"

int unsat_on_save;
UnitPropagation* saved_prop_obj;

Literal::Literal() {
	id = -1;
	val = false;
	prev = NULL;
	next = NULL;
}

Atom::Atom(int id) {
	this->id = id;
	this->val = -1;
}

Atom::Atom(int id, int val) {
	this->id = id;
	this->val = val;
}

UnitPropagation::UnitPropagation(set<int> &unique_ids, vector<vector<atom_val>> &cnfs) {
	int numOfClauses = cnfs.size();
	for(int id: unique_ids) {
		Atom* a = new Atom(id);
		atoms[id] = a;
	}
	for(int i=0; i<numOfClauses; ++i) {
		Literal* c = new Literal();
		int numOfLiterals = cnfs[i].size();
		Literal* prevl;
		for(int j=0; j<numOfLiterals; ++j) {
			atom_val av = cnfs[i][j];
			Literal* l = new Literal();
			Atom* a = atoms[av.id];
			l->id = av.id;
			l->val = av.val;
			a->link.push_front(l);
			if (j==0) {
				c->next = l;
				l->prev = c;
			} else {
				prevl->next = l;
				l->prev = prevl;
			}
			prevl = l;
		}
		clauses.push_front(c);
	}
	unsat_on_save = iterate_clauses();
}

UnitPropagation::UnitPropagation() {
	map<Literal*, Literal*> address_map;
	forward_list<Literal*> &saved_clauses = saved_prop_obj->get_clauses();
	for(auto literal1: saved_clauses) {
		Literal* literal = new Literal();
		literal->id = literal1->id;
		literal->val = literal1->val;
		address_map[literal1] = literal;
		clauses.push_front(literal);
		while(literal1 != NULL) {
			if(literal1->next != NULL) {
				Literal *new_literal = new Literal();
				literal->next = new_literal;
				new_literal->id = literal1->next->id;
				new_literal->val = literal1->next->val;
				new_literal->prev = literal;
				literal = new_literal;
				address_map[literal1->next] = new_literal;
				literal1 = literal1->next;
			} else {
				literal1 = literal1->next;
			}
		}
	}
	map<int, Atom*> &saved_atoms = saved_prop_obj->get_atoms();
	for(auto x: saved_atoms) {
		Atom *a = new Atom(x.second->id, x.second->val);
		for(Literal* l: x.second->link) {
			a->link.push_front(address_map[l]);
		}
		atoms[x.first] = a;
	}
}

UnitPropagation::~UnitPropagation() {
	for(Literal* nodePtr: clauses) {
		while(nodePtr != NULL) {
			Literal* tempPtr = nodePtr->next;
			delete nodePtr;
			nodePtr = tempPtr;
		}
	}
	clauses.clear();
	for(auto x: atoms) {
		delete x.second;
	}
	atoms.clear();
}

int UnitPropagation::get_assignment(int id) {
	Atom* a = atoms[id];
	return a->val;
}

int UnitPropagation::set_assignment(int id, bool val) {
	int unsat = propagate(id, val);
	if(unsat) {
		return 1;
	} else {
		if(atoms[id]->val == -1) {
			atoms[id]->val = val;
			atoms[id]->link.clear();
		}
		return iterate_clauses();
	}
}

int UnitPropagation::iterate_clauses() {
	bool change = true;
	while(change) {
		change = false;
		forward_list<Literal*> to_remove;
		for(Literal* root: clauses) {
			if(root->next == NULL) {
				to_remove.push_front(root);
			} else if(root->next->next == NULL) {
				Literal* l = root->next;
				bool val = l->val;
				int id = l->id;
				int unsat = propagate(id, val);
				if (unsat) {
					return 1;
				} else {
					if(atoms[id]->val == -1) {
						atoms[id]->val = val;
						atoms[id]->link.clear();
					}
				}
				to_remove.push_front(root);
				change = true;
			} else {
			}
		}
		for(Literal* empty_clause: to_remove) {
			delete empty_clause;
			clauses.remove(empty_clause);
		}
	}
	return 0;
}

int UnitPropagation::propagate(int id, bool val) {
	Atom* a = atoms[id];
	if(a->val == -1) {
		for(Literal* l: a->link) {
			if(l->val == val) {
				Literal* lr = l;
				Literal* ll = l->prev;
				bool first = true;
				while(lr != NULL) {
					Literal* l1 = lr->next;
					Atom* b = atoms[lr->id];
					if(!first) {
						b->link.remove(lr);
					}
					first = false;
					delete lr;
					lr = l1;
				}
				while(ll->prev != NULL) {
					Literal* l1 = ll->prev;
					Atom* b = atoms[ll->id];
					b->link.remove(ll);
					delete ll;
					ll = l1;
				}
				ll->next = NULL;
			} else {
				if(l->next == NULL && l->prev->prev == NULL) {
					return 1; // UNSAT
				} else {
					Literal* prev = l->prev;
					prev->next = l->next;
					if(l->next != NULL) {
						l->next->prev = prev;
					}
					delete l;
				}
			}
		}
	} else if (a->val != val) {
		return 1; // UNSAT
	} else {
	}
	return 0;
}

map<int, Atom*> &UnitPropagation::get_atoms() {
	return atoms;
}

forward_list<Literal*> &UnitPropagation::get_clauses() {
	return clauses;
}

int initialize_prop_obj(vector<vector<vector<int>>> &input_cnfs) {
	set<int> unique_ids;
	vector<vector<atom_val>> cnfs;
	for(unsigned int i=0; i<input_cnfs.size(); ++i) {
		vector<atom_val> cnf;
		set<int> temp_set;
		for(unsigned int j=0; j<input_cnfs[i].size(); ++j) {
			atom_val l;
			l.id = input_cnfs[i][j][0];
			unique_ids.insert(l.id);
			l.val = (bool)input_cnfs[i][j][1];
			cnf.push_back(l);
			temp_set.insert(l.id);
		}
		if(temp_set.size()!=input_cnfs[i].size()) {
			throw SameAtomInCNF();
		}
		cnfs.push_back(cnf);
	}
	saved_prop_obj = new UnitPropagation(unique_ids, cnfs);
	return unsat_on_save;
}

void delete_saved_prop_obj() {
	delete saved_prop_obj;
}

int test_unit_propagation() {
	vector<vector<vector<int>>> input_cnfs{
		{{1,1}, {2,0}, {3,0}},
		{{4,1}, {3,0}, {1,1}},
		{{2,1}, {1,0}, {3,1}},
		{{2,0}, {1,0}},
		{{3,0}, {5,1}},
		{{5,0}, {4,1}, {6,0}}
	};

	cout << "UNSAT on save?: " << initialize_prop_obj(input_cnfs) << endl;

	UnitPropagation* working_prop_obj = new UnitPropagation();

	cout << "Before unit propagation, assignment of 1: " << working_prop_obj->get_assignment(1) << endl;

	cout << "UNSAT?: " << working_prop_obj->set_assignment(1,1) << endl;

	cout << "After unit propagation, assignment of 1: " << working_prop_obj->get_assignment(1) << endl;
	cout << "After unit propagation, assignment of 2: " << working_prop_obj->get_assignment(2) << endl;
	cout << "After unit propagation, assignment of 3: " << working_prop_obj->get_assignment(3) << endl;
	cout << "After unit propagation, assignment of 4: " << working_prop_obj->get_assignment(4) << endl;
	cout << "After unit propagation, assignment of 5: " << working_prop_obj->get_assignment(5) << endl;
	cout << "After unit propagation, assignment of 6: " << working_prop_obj->get_assignment(6) << endl;

	delete working_prop_obj;
	delete saved_prop_obj;
	return 0;
}


