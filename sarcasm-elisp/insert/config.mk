EXEC = 

DESTDIR = .

# Compiler
# Compilation with Clang++ could be done with:
# CXX=clang++ / CC=clang make [options]
CXX ?= g++
CC ?= gcc

SRCS = $(wildcard *.cpp)

OBJS = $(SRCS:%.cpp=%.o)

INCLUDES =
# Add optimisation flags (-O2) ?
CXXFLAGS = -Wall -Wextra -Wshadow -Wpointer-arith $(INCLUDES)
CFLAGS = -Wall -Wextra -Wshadow -ansi -pedantic $(INCLUDES)
LDFLAGS =
DEBUG = -ggdb3

# Archiver
AR = ar
ARFLAGS = rcs

# Others
RM = rm -f
CP = cp

all: $(OBJS)
	$(CXX) -o $(DESTDIR)/$(EXEC) $(OBJS) $(LDFLAGS)

clean:
	$(RM) $(OBJS)

cleanall: clean
	$(RM) $(DESTDIR)/$(EXEC)

debug: CXXFLAGS += $(DEBUG)
debug: all

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -o $@ -c $<

%.o: %.c
	$(CC) $(CFLAGS) -o $@ -c $<

gprof: CXXFLAGS += -pg
gprof: LDFLAGS += -pg
gprof: all

check-syntax:
	g++ $(CXXFLAGS) -fsyntax-only $(CHK_SOURCES)

code-helpers:
	gtags
	cscope-indexer
	find . -name "*.cpp" -o -name "*.hh" -o -name "*.hpp" | ebrowse
