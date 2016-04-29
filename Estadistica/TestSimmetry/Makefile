VERSION=10.0
MLINKDIR = /opt/Mathematica/SystemFiles/Links/MathLink/DeveloperKit/Linux-x86-64
SYS = Linux-x86-64
CADDSDIR = ${MLINKDIR}/CompilerAdditions
DEFINES = MATHEMATICA
EXTRA_CFLAGS=-m64 -D ${DEFINES}

INCDIR = ${CADDSDIR}
LIBDIR = ${CADDSDIR}
OBJDIR = obj
BINDIR = bin

MPREP = ${CADDSDIR}/mprep
RM = rm

CC = /usr/bin/cc
CXX = /usr/bin/c++ -std=c++11 -pthread -lgsl -lgslcblas -lm


# Shared
clean: 
	@echo "##########################"
	@echo "#                        #"
	@echo "#      Cleaning up       #"
	@echo "#                        #"
	@echo "##########################"
	@echo ""
	@rm MathematicaTestSimmetry test_simmetrytm.o test_simmetry.o test_simmetrytm.cpp
	

# Mathematica

before_mathematica: 
	@echo ""
	@echo "#######################"
	@echo "#                     #"
	@echo "#      Building       #"
	@echo "#                     #"
	@echo "######################"
	@echo ""

after_mathematica: 
	@echo ""
	@echo "#############################"
	@echo "#                           #"
	@echo "#            Done.          #"
	@echo "#                           #"
	@echo "#############################"
	@echo ""

mathematica : before_mathematica MathematicaTestSimmetry after_mathematica

MathematicaTestSimmetry : test_simmetrytm.o test_simmetry.o
	${CXX} ${EXTRA_CFLAGS} -I${INCDIR} test_simmetrytm.o test_simmetry.o -L${LIBDIR} -lML64i4 -lm -lpthread -lrt -lstdc++ -ldl -luuid -o $@
	
test_simmetrytm.o : test_simmetrytm.cpp
	${CXX} -c ${EXTRA_CFLAGS} -I${INCDIR} test_simmetrytm.cpp -o $@
	
test_simmetry.o: test_simmetry.cpp
	$(CXX) -c $(EXTRA_CFLAGS) -I${INCDIR} test_simmetry.cpp -o test_simmetry.o
	

test_simmetrytm.cpp : test_simmetry.tm
	${MPREP} $? -o $@
	
	
.DEFAULT_GOAL := mathematica