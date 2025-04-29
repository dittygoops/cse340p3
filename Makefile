CXX = g++
CXXFLAGS = -Wall -g

all: a.out

a.out: demo.cc execute.cc inputbuf.cc lexer.cc
	$(CXX) $(CXXFLAGS) -o a.out demo.cc execute.cc inputbuf.cc lexer.cc

clean:
	rm -f a.out *.o
