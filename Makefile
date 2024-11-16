CC     = gcc
RACKET = racket
RM     = rm

runtime.o: runtime.cc runtime.h
	$(CC) -x c++ -c -std=c++17 -g -arch x86_64 runtime.cc

test: runtime.o
	$(RACKET) run-tests.rkt

clean:
	$(RM) -f *.o *.out *.exe *.s *~
