#include<stdio.h> 
#include<iostream> 

#include "parser.hpp"
#include "simulator.hpp"

#define INTRO "Simulador de manejo de memoria"
#define DIALOG "Que deseas hacer?: \n * RESERVAR <nombre> <cantidad>\n * LIBERAR <nombre>\n * MOSTRAR \n * SALIR\n"


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
