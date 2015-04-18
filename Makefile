# ffourati@student.42.fr
# Created 03/25/2015 02:35

.PHONY: all clean fclean re depend

BIN = mySolver

SRCD = src
SRC_ = MyPolynomial.hs MyPolynomial/Print.hs Main.hs 
SRC = $(addprefix $(SRCD)/, $(SRC_))
SRCSUBD = $(addprefix $(SRCD)/, $(SUBDIRS))

SPACE = $(eval) $(eval)
IMPORT_LIST = $(subst $(SPACE),:,$(SRCSUBD))

SUBDIRS := MyPolynomial

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
	ghc --make -o $(BIN) -O -odir$(OBJD) -hidir$(IFACED) -i$(IMPORT_LIST) $^


$(OBJD):
	mkdir -p $(OBJSUBD) $(IFACESUBD)

clean:
	rm -rf $(OBJD) $(IFACED)

fclean: clean
	rm -f $(BIN)

re: fclean all

