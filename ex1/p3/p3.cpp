#include<stdio.h> 
#include<stdlib.h> 
#include<set> 
#include<map> 
#include<string> 
#include<vector> 
#include<iostream> 
#include<math.h> 

#define ERRNAME -1
#define ERRSIZE -2

#define RESERVAR "RESERVAR"
#define LIBERAR "LIBERAR"
#define MOSTRAR "MOSTRAR"
#define SALIR "SALIR"


using namespace std; 

int best_fit_size (int el) { 
	int cnt = 0, chk;

	while ( el != 0 )  {
		el >>= 1; 
		chk += el & 1; 
		cnt +=1; 
	}
		
	if (chk == 1 && el != 1) 
		return ( int ) pow (2,cnt-1) ;
 	return (int)  pow (2, cnt); 	 // por ahora
} //mejorar

void fill_segment(vector<bool> &memo, int ind, bool el, int ub) { 
	for(int i=ind ; i<ub; i++) 
		memo[i] = el ;
}

void reserve(vector<bool> &memo, map<string, pair<int, int> > &ids,set<int> &bounds, string name, int request, int ind) { 
	int size = best_fit_size( request ) ; 

	// Update memory state
	fill_segment(memo, ind, false, ind+size); 

	// Update mapping with name, start position and size
	ids[name] = { ind, request } ; 

	// insert lower bound for further checking
	bounds.insert( ind ) ; 
}

int find_best_fit(vector<bool> &memo, set<int> &bounds, int mmax, int request) { 
	int step = best_fit_size (request) ; 
	int i; 
	
	/* For each multiple of best fit whithin mmax constraint, check with bounds set that
	 * there's available space for allocation. If so, return index. 
	 */
	for (i=0; i<mmax; i+= step ) { 
		auto it = bounds.upper_bound(i); 
		if ( memo[i] && (it == bounds.end() || *it > i + request - 1) && i + request - 1 < mmax )
			return i ; // reserve(memo,bounds,ids, name, request,i); 

	} 

	return -1; 
}


int insert (vector<bool> &memo, set<int> &bounds, map<string, pair<int, int>> &ids, string name, int mmax, int request){ 
	int fit; 
	if (ids.count(name) > 0) 
		return ERRNAME; 

	fit = find_best_fit(memo,bounds, mmax,request); 


	if ( fit != -1)  { 
		reserve(memo, ids, bounds, name, request, fit) ;
		return 1; 
	}

	return ERRSIZE; 
}

void display_memory(vector<bool> &memo) { 
	for(auto el : memo) 
		printf("%d",(int)el) ; 
	printf("\n"); 
}

bool erase(vector<bool> &memo, set<int> &bounds, map<string, pair<int, int>> &names, string name) { 
	
	if (names.count(name) == 0) 
		return false; 
	// vector, indice, elemento y cota superior

	auto pp = names[name];  
	int ub = pp.first + best_fit_size( pp.second ) ; 

	fill_segment(memo, pp.first, true, ub) ; //erase memory segment
	bounds.erase( pp.first ) ;  // erase from set
	names.erase(name); // erase from map 

	return true; 
}


void dbg(map<string, pair<int, int> > &names,vector<bool> &mem, set<int> &bounds ) { 

	printf("set of bounds:\n"); 
	for(auto el : bounds)
		printf("%d ",el); 
	printf("\n"); 

	printf("memory vector:\n"); 
	for(auto el : mem)
		printf("%d",(int)el); 
	printf("\n"); 

	printf("map of names:\n"); 
	for(auto el : names){ 
		cout<<el.first<<" "; 
		printf("%d %d\n", el.second.first, el.second.second); 
	}
}

bool parse_input(string ss) { 

	unsigned int ind ;
	string help ; 

	
	if ( (ind = ss.find(RESERVAR)) != string::npos ) {  //aaaaaaaaaaaaaaaaaaaaaaaaaaa
		help = ss.substr(ind); 
	}
	else if ( ss.find(LIBERAR) != string::npos ) {

	}
	else if ( ss.find(MOSTRAR) != string::npos ) { 

	}
	else if ( ss.find(SALIR) != string::npos )  { 

	}
	else 
		return false; 
	
	return true; 
}

int main(int argc, char **argv) { 

}

void testing(int argc, char **argv) {
	map<string, pair<int, int> > names; 
	set<int> bounds; 
	vector<bool> mem; 

	int mmax = atoi(argv[1]); 

	mem.resize( mmax ) ;  // not pretty
	for(int i=0; i<mmax; i++) mem[i] = true;  // init

	int blocks, num, chk; 
	string test; 


	printf("Ingrese el numero de inserciones que hara: "); 
	scanf("%d",&num); 

	//RESERVAR
	
	for(int i=0; i<num ;i++) { 
		cin >> test >> blocks; 

		chk = insert(mem, bounds, names, test, mmax, blocks); 
		switch ( chk ) { 
			case ERRNAME: 
			        printf("Existing name\n"); 
			        break; 
		       	case ERRSIZE:
			        printf("Insuficient size\n"); 
				break;	
			default:
				printf("Successfull insertion\n"); 
		}
			
	}
	printf("\n"); 

	dbg(names, mem, bounds); 

	printf("Ingrese el numbero de elementos a eliminar: "); 
	scanf("%d",&num); 

	bool gg; 
	for(int i=0 ;i<num; i++)  {
		cin >> test; 

		gg = erase(mem,bounds, names, test); 
		printf("%s", gg? "Eliminacion exitosa\n" : "Nombre no presente\n"); 

	}
	printf("\n"); 

	dbg(names, mem, bounds); 
}

/*
 *  program num_blocks
 *
 *  RESERVAR <nombre> <bloques> 
 *  	no hay espacio contiguo suficiente
 *  	nombre ya tiene memoria reservada
 *
 *  LIBERAR  <nombre>
 *  	no existe ese nombre con espacio asignado
 *
 *  MOSTRAR 
 *
 *  SALIR
 *
 */


