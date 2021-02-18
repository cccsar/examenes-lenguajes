#include "simulator.hpp"

#ifndef __PARSER_HELPER
#define __PARSER_HELPER

/*
 * Checks if a string of characters is an integer.
 */
bool isInt (const string & ss); 

/* 
 * Breaks a string terminated on '\n' into a vector of space separated words.
 */
void words(vector<string> &wd, string &ss); 

/*
 * Given a string and a simulator, parses the string and acts on behalf
 * of the parsed request.
 */
bool parse_input(string &ss, simulator &sim); 

#endif
