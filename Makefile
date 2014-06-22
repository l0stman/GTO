CC		= c++ -std=c++11
CXXFLAGS	= -O3 -Wall -pedantic -DNDEBUG			# Production use
#CXXFLAGS	= -O0 -Wall -pedantic -g3			# Debug mode
#CXXFLAGS	= -O3 -Wall -pedantic -DNDEBUG -g3		# Profiling mode
PREF		= ${HOME}/hacks/poker/pokerstove/src
LIBDIRS		= -L /usr/local/lib -L $(PREF)/build/lib/pokerstove/peval
#PERFTOOLS_LIBS = -lprofiler	# Uncomment for profiling mode.
STATIC_LIBS	= -lpeval
DYNAM_LIBS	= $(PERFTOOLS_LIBS)
LDFLAGS		= -Wl,-Bstatic $(STATIC_LIBS) -Wl,-Bdynamic $(DYNAM_LIBS)

INCLUDES	= -I /usr/local/include -I $(PREF)/lib
3BET_OBJS	= 3bet.o range.o equi_dist.o cfr.o err.o
5BET_OBJS	= 5bet.o preflop_range.o preflop_equi_dist.o cfr.o err.o
PROGNAMES	= 3bet 5bet

all : $(PROGNAMES)
3bet : $(3BET_OBJS)
	$(CC) $(LIBDIRS) -o $@ $(3BET_OBJS) $(LDFLAGS)

5bet : $(5BET_OBJS)
	$(CC) $(LIBDIRS) -o $@ $(5BET_OBJS) $(LDFLAGS)

.SUFFIXES : .o .cc
.cc.o :
	$(CC) $(CXXFLAGS) $(INCLUDES) -c $<

depend:
	$(CC) $(INCLUDES) -E -MM *.cc > .depend
clean:
	rm -f *.o *.core *~ $(PROGNAMES)
