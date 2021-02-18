#include<stdio.h> 
#include<iostream> 

#include "simulator.cpp"
#include "parser.cpp"


#define INTRO "Simulador de manejo de memoria"
#define DIALOG "Que deseas hacer?: \n * RESERVAR <nombre> <cantidad>\n * LIBERAR <nombre>\n * MOSTRAR \n * SALIR\n"


using namespace std; 


void dbg(simulator &sim) { 

	printf("set of bounds:\n"); 
	for(auto el : sim.bounds)
		printf("%d ",el); 
	printf("\n"); 

	printf("memory vector:\n"); 
	for(auto el : sim.mem)
		printf("%d",(int)el); 
	printf("\n"); 

	printf("map of names:\n"); 
	for(auto el : sim.names){ 
		cout<<el.first<<" "; 
		printf("%d %d\n", el.second.first, el.second.second); 
	}
}

int main(int argc, char **argv) { 
	int mmax  ; 
	bool chk; 
	string line; 

	if ( argc < 1) { 
		printf("Es necesario 1 argumento que represente la memoria total. Saliendo.\n"); 
		exit(-1);
	}

	mmax = atoi ( argv[1] ) ; 
	simulator sim ( mmax ) ; 


	printf("%s\n",INTRO); 

	while ( true ) {
		chk = false; 
		
		printf(DIALOG); 

		printf("Siguiente eleccion: "); 
		getline(cin,line); 

		chk = parse_input(line, sim) ; 

		if ( !chk ) 
			printf("Opcion no reconocida. Ingrese una opcion correcta\n"); 
	} 
}
