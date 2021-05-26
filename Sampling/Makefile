CXXFLAGS=--std=c++11 -fPIC
CXXFLAGS+=$(shell pkg-config swipl --cflags)
CXXFLAGS+=$(shell gsl-config --cflags)
LD=swipl-ld
LDFLAGS=--std=c++11 -shared -ld g++
LDFLAGS+=$(shell gsl-config --libs)
SRC=$(wildcard ./src/*.cpp)
OBJ=$(SRC:.cpp=.o)
EXEC=sampling.so
#DESTDIR=..

all	: $(EXEC)

$(EXEC)	: $(OBJ)
	$(LD) $(LDFLAGS) -o $@ $^
#	mv $(EXEC) $(DESTDIR)

sampling.o	: sampling.cpp sampler.cpp sampler.h
# ... a lot more of dependencies specification here

.PHONY 	: clean

clean	:
	rm -rf ./src/*.o
	rm -f $(EXEC)

mrproper: clean
	rm -f $(EXEC)