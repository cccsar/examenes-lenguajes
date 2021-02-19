#include<vector> 
#include<string> 
#include<string> 

#include "simulator.hpp"

using namespace std; 

#define RESERVAR "RESERVAR"
#define LIBERAR "LIBERAR"
#define MOSTRAR "MOSTRAR"
#define SALIR "SALIR"

#ifndef __PARSER_HELPER
#define __PARSER_HELPER

/*
 * Checks if a string of characters is an integer.
 */
bool isInt (const std::string & ss); 

/* 
 * Breaks a string terminated on '\n' into a vector of space separated words.
 */
void words(std::vector<std::string> &wd, std::string &ss); 

/*
 * Given a string and a simulator, parses the string and acts on behalf
 * of the parsed request.
 */

bool parse_input(string &ss, simulator &sim) ; 
#endif
