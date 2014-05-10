CC		= g++47 -std=c++11
CXXFLAGS	= -O0 -Wall -pedantic -g3
#CXXFLAGS	= -O3 -Wall -pedantic
PREF		= ${HOME}/hacks/poker/pokerstove/src
LDFLAGS		= -static $(PREF)/build/lib/pokerstove/peval/libpeval.a

INCLUDES	= -I /usr/local/include -I $(PREF)/lib
OBJS		= main.o range.o equi_dist.o
PROGNAME	= test

$(PROGNAME) : $(OBJS)
	$(CC) -o $(.TARGET) $(.ALLSRC) $(LDFLAGS)

.SUFFIXES : .o .cc
.cc.o :
	$(CC) $(CXXFLAGS) $(INCLUDES) -c $(.IMPSRC)

depend:
	$(CC) $(INCLUDES) -E -MM *.cc > .depend
clean:
	rm -f *.o *.core *~ $(PROGNAME)
