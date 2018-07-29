CC := g++

CFLAGS := -std=c++14 -Wall -c -g3 $(shell mysql_config --cflags) \
-I/usr/local/include -fopenmp

LDFLAGS := $(shell mysql_config --libs) $(shell pkg-config --libs gsl) \
-L/usr/local/lib -fopenmp

## src := aggregate.cpp
%.o: %.cpp
	$(CC) $(CFLAGS) $^ -o $@

test: Neon.o
	$(CC) $^ $(LDFLAGS) -o $@

.PHONY: clean
clean:
	rm -f *.o test

