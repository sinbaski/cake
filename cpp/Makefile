CC := g++

CFLAGS := -std=c++11 -c -g3 $(shell mysql_config --cflags)
LDFLAGS := $(shell mysql_config --libs) ## $(shell pkg-config --libs gsl)

src := aggregate.cpp

%.o: %.cpp
	$(CC) $(CFLAGS) $^ -o $@

trader: aggregate.o
	$(CC) $< $(LDFLAGS) -o $@

.PHONY: clean
clean:
	rm -f *.o trader

