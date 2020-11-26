VPATH = ../

FC = gfortran -c			#fortran compiler
LK = gfortran -o ../flow_v2.8 		#program linker

OP = -fopenmp -g -O2 -fbacktrace	#compiling options

COMP = $(FC) $(OP)			#Compile Command
LINK = $(LK) $(OP)			#Link Command

MODS = parameter_module.o profile_module.o map_module.o

OBJS = cell_sink.o cell_flow.o connect_rad_flow.o output.o

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
