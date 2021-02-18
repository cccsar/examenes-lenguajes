#include "simulator.hpp"
#include <math.h> 

simulator::simulator( int mmax ) {  

	mem.resize( mmax ) ; 

	for(int i=0 ;i<mmax ; ++i)
		mem[i] = true; 

	size = mmax; 
}


void simulator :: fill_segment ( int ind, int ub , int el) {

	for(int i=ind ; i<ub; i++) 
		mem[i] = el ;
}


int simulator :: best_fit_size (int el) { 
	int cnt , chk;


	cnt = chk = 0 ;

	while ( el != 0 )  {
		if ( (el & 1) == 1 ) 
			chk +=1 ;
		el >>= 1; 
		cnt +=1; 
	}
		
	if (chk == 1 ) 
		return ( int ) pow (2,cnt-1) ;
 	return (int)  pow (2, cnt);
} 


void simulator :: reserve( string name, int request, int ind) { 
	int size = best_fit_size( request ) ; 

	// Update memory state
	fill_segment(ind, ind+size, false); 

	// Update mapping with name, start position and size
	names[name] = { ind, request } ; 

	// insert lower bound for further checking
	bounds.insert( ind ) ; 
}


int simulator :: find_best_fit(int request) { 
	int step = best_fit_size (request) ; 
	int i; 
	
	/* For each multiple of best fit whithin mmax constraint, check with bounds set that
	 * there's available space for allocation. If so, return index. 
	 */
	for (i=0; i<size; i+= step ) { 
		auto it = bounds.upper_bound(i); 

		printf("mem[%d]: %d | ub(%d): %d | %d\n",i,(int) mem[i],i,*it,request); 

		if ( mem[i] && (it == bounds.end() || *it > i + request - 1) && i + request - 1 < size )
			return i ; 
	} 

	return -1; 
}


int simulator :: insert ( string name, int request){ 
	int fit; 

	if (request <= 0) 
		return ERRMINSIZE; 

	if (names.count(name) > 0) 
		return ERRNAME; 

	fit = find_best_fit(request); 

	if ( fit != -1)  { 
		reserve(name, request, fit) ;
		return 1; 
	}

	return ERRSIZE; 
}


void simulator :: display_memory() { 
	for(auto el : mem) 
		printf("%d",(int)el) ; 
	printf("\n"); 
}


bool simulator :: erase(string name) { 
	if (names.count(name) == 0) 
		return false; 

	auto pp = names[name];  
	int ub = pp.first + best_fit_size( pp.second ) ; 

	fill_segment(pp.first, ub, true) ; //erase memory segment
	bounds.erase( pp.first ) ;  // erase from set
	names.erase(name); // erase from map 

	return true; 
}
