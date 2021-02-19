#include<vector> 
#include<map> 
#include<set> 
#include<string> 

using namespace std; 

#ifndef __SIMULATOR
#define __SIMULATOR

#define ERRNAME -1
#define ERRSIZE -2
#define ERRMINSIZE -3

class simulator { 

public:

	/* Memory map */
	vector<bool> mem; 
	
	/* Map from names to memory positions and sizes */ 
	map < string, pair<int, int> > names; 

	/* Set of initial positions of each block with occupied memory.
	 * It is used to avoid linear search while searching for free memory.
	 */
	std::set< int > bounds; 

	/* Total size of memory */
	int size; 


	/* Always receive the amount of memory to simulate */ 
	simulator( int mmax ) ;  


	/* Checks if insertion is valid: 
	 * 	There are no names asociated already with passed name.
	 * 	There is enough memory.
	 * And proceeds to "allocate".
	 */
	int insert (string name, int request);


	/* Checks if deletion is valid (there is an existing name with
	 * an associated block of memory), and proceeds to "de-allocate".
	 */
	bool erase(string name); 

	
	/* Shows the current memory layout */
	void display_memory();


private : 

	/* Performs memory allocation */ 
	void reserve(string name, int request ,int ind) ; 

	/* Given a index, a size and an element, fill said size from said index 
	 * memory position with given element 
	 */
	void fill_segment ( int ind, int ub , int el) ; 

	/* Returns best fit size ( upper bound for a power of 2 ) of a given amount of 
	 * memory in request
	 */
	int best_fit_size (int el ); 

	
	/* Returns either the index for the next memory allocation to carry out, or -1 
	 * if not possible
	 */	
	int find_best_fit(int request); 

}; 

#endif
