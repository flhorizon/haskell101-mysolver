# ffourati@student.42.fr
# Created 03/25/2015 02:35

.PHONY: all clean fclean re

BIN = mySolver

SRCD = src
SRC_ = Main.hs MyPolynome.hs
SRC = $(addprefix $(SRCD)/, $(SRC_))

OBJD = obj
OBJ_ = $(SRC_: .hs=.o)
OBJ = $(addprefix $(OBJD)/, $(OBJ_))
IFACE_ = $(SRC_: .hs=.hi)
IFACE = $(addprefix $(OBJD)/, $(IFACE_))

GHCFLAGS = -O -odir $(OBJD) -hidir $(OBJD)

all: $(OBJD) $(BIN)

$(BIN): $(OBJ)


$(OBJD)/%.o: $(SRCD)/%.hs
	ghc $(GHCFLAGS) $< -o $@


$(OBJD):
	mkdir -p $(OBJD)

clean:
	rm -rf $(OBJD)

fclean: clean
	rm -f $(BIN)

re: fclean all

