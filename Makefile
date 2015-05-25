# ffourati@student.42.fr
# Created 03/25/2015 02:35

BIN = mySolver

SRCD = src
SRC_ = Data/MyPolynomial.hs \
       Data/MyPolynomial/Print.hs \
       Data/MyPolynomial/Parser.hs \
       Data/MyPolynomial/Type.hs \
       Main.hs \
       Solve.hs

SRC = $(addprefix $(SRCD)/, $(SRC_))
SRCSUBD = $(addprefix $(SRCD)/, $(SUBDIRS))

SPACE = $(eval) $(eval)
IMPORT_LIST = $(subst $(SPACE),:,$(SRCSUBD))

SUBDIRS := Data Data/MyPolynomial 

OBJD = obj
OBJSUBD = $(addprefix $(OBJD)/, $(SUBDIRS))
OBJ_ = $(SRC_:.hs=.o)
OBJ = $(addprefix $(OBJD)/, $(OBJ_))

IFACED = iface
IFACESUBD = $(addprefix $(IFACED)/, $(SUBDIRS))
IFACE_ = $(SRC_:.hs=.hi)
IFACE = $(addprefix $(IFACED)/, $(IFACE_))


all: $(OBJD) $(BIN)


$(BIN): $(SRC)
	ghc --make -o $(BIN) -O2 -odir$(OBJD) -hidir$(IFACED) -i$(IMPORT_LIST) $^


$(OBJD):
	mkdir -p $(OBJSUBD) $(IFACESUBD)

clean:
	rm -rf $(OBJD) $(IFACED)

fclean: clean
	rm -f $(BIN)

re: fclean all


.PHONY: all clean fclean re
