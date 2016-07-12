PROGS  = hslines
SOURCES = Main.hs GameLogic.hs MainWindow.hs

all : $(PROGS)

hslines : $(SOURCES)
	  $(HC_RULE)

HC_RULE = $(HC) --make $< -o $@ $(HCFLAGS)

clean:
	rm -f $(SOURCES:.hs=.hi) $(SOURCES:.hs=.o) $(PROGS)

HC=ghc
