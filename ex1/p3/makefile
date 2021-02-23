buddy: main.cpp parser.o simulator.o
	g++ -O2 -Wall -std=c++17 -o buddy main.cpp parser.o simulator.o

parser.o: parser.cpp parser.hpp 
	g++ -O2 -Wall -std=c++17 -c parser.cpp

simulator.o: simulator.cpp simulator.hpp
	g++ -O2 -Wall -std=c++17 -c simulator.cpp

clean: 
	@echo "cleaning"
	rm *.o buddy 
