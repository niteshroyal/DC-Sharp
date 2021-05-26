/*
 * unitPropagation.h
 *
 *  Created on: Dec 29, 2020
 *      Author: nitesh
 */

#ifndef SRC_UNITPROPAGATION_H_
#define SRC_UNITPROPAGATION_H_

#include <set>
#include <map>
#include <iostream>
#include <vector>
#include <exception>
#include <algorithm>
#include <forward_list>

using namespace std;

struct SameAtomInCNF : public exception {
   const char * what () const throw () {
      return "\nRepeated atoms in individual CNFs are not supported by unit propagation module!\n";
   }
};

struct atom_val {
	int id;
	bool val;
};

class Literal {
public:
	int id;
	bool val;
	Literal* prev;
	Literal* next;
	Literal();
};


class Atom {
public:
	int id;
	int val;
	forward_list<Literal*> link;
	Atom(int id);
	Atom(int id, int val);
};


class UnitPropagation {
private:
	map<int, Atom*> atoms;
	forward_list<Literal*> clauses;
	int iterate_clauses();
	int propagate(int atom, bool val);

public:
	/**
	 * The constructor that performs unit propagation on input CNFs
	 * and saves the data structure in the main memory.
	 */
	UnitPropagation(set<int> &unique_ids, vector<vector<atom_val>> &cnfs);

	/**
	 * The default constructor that creates object using the saved data structure.
	 */
	UnitPropagation();

	/**
	 * Destroys the object.
	 */
	~UnitPropagation();

	/**
	 * Fetch constraints.
	 *
	 * @param id Atom id.
	 * @return 0 if True, 1 is False, -1 if not known.
	 */
	int get_assignment(int id);

	/**
	 * Add constraints.
	 *
	 * @param id Atom id.
	 * @param val Assignment of the atom.
	 * @return 1 if UNSAT; otherwise 0.
	 */
	int set_assignment(int id, bool val);

	/**
	 * Returns Atoms.
	 *
	 * @return atoms.
	 */
	map<int, Atom*> &get_atoms();

	/**
	 * Returns Clauses.
	 *
	 * @return clauses.
	 */
	forward_list<Literal*> &get_clauses();
};


/**
 * Performs unit propagation on input CNFs and
 * saves the data structure in the main memory.
 * Repeated atoms in individual CNFs are not supported.
 *
 * @param input_cnfs Input CNFs.
 * @return 1 if UNSAT; otherwise 0.
 */
int initialize_prop_obj(vector<vector<vector<int>>> &input_cnfs);

/**
 * Deletes the saved data structure from the main memory.
 */
void delete_saved_prop_obj();

/**
 * Unit Test
 */
int test_unit_propagation();

#endif /* SRC_UNITPROPAGATION_H_ */
