VPATH = ../

FC = gfortran -c			#fortran compiler
LK = gfortran -o ../flow_v3.7		#program linker

OP = -fopenmp -g -O2 -fbacktrace	#compiling options

COMP = $(FC) $(OP)			#Compile Command
LINK = $(LK) $(OP)			#Link Command

MODS = parameter_module.o profile_module.o map_module.o

OBJS = ocean.o drop_search.o level_drain.o cell_drain.o\
       output.o grad_search.o rate_flow.o initialize.o\
       solve_status.o elev_drop.o elev_drain.o grad_drain.o

PROG = rivercode.o

.f.o:
	$(COMP) $<

%.o : %.f90
	$(COMP) $<

flow_path_v1: $(MODS) $(OBJS) $(PROG)
	$(LINK) $(MODS) $(OBJS) $(PROG)

clean: 
	rm -f *.o *.mod

cleanrun:
	rm -f ../../rundir/riverun ../../rundir/*.log ../../rundir/*.txt
