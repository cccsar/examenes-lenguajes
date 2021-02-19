all: buddy test

buddy: main.cpp parser.o simulator.o
	g++ -O2 -Wall -std=c++11 -o buddy main.cpp parser.o simulator.o

parser.o: parser.cpp parser.hpp 
	g++ -O2 -Wall -std=c++11 -c parser.cpp

simulator.o: simulator.cpp simulator.hpp
	g++ -O2 -Wall -std=c++11 -c simulator.cpp

test: catch_amalgamated.cpp catch_amalgamated.hpp
	g++ -O2 -Wall -std=c++17 -fprofile-arcs -ftest-coverage -o test test.cpp

clear: 
	@echo "cleaning"
	rm *.o buddy test *.gcda *.gcno *.gcov

# gcov ./test
