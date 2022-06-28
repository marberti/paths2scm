FC = gfortran
FLAGS = -g -cpp -O2 -Wall -Wunused -Wpedantic -Wno-maybe-uninitialized -std=f2008

SRCDIR = src
OBJDIR = obj
MODDIR = mod

SOURCES = mod_error.f90                   \
          mod_get_field.f90               \
          mod_scm_data.f90                \
          mod_scm_compute.f90             \
          mod_scm_input.f90               \
          mod_input.f90                   \
          main.f90

SRC = $(addprefix $(SRCDIR)/, $(SOURCES))
OBJ = $(addprefix $(OBJDIR)/, $(SOURCES:%.f90=%.o))
EXE = paths2scm.x

# main compilation options --------------------------------
.PHONY: default
default: $(EXE)

.PHONY: fresh
fresh: clean $(EXE)

# utility -------------------------------------------------
.PHONY: clean
clean:
	@printf "Cleaning... "
	@rm -f $(OBJDIR)/*.o $(MODDIR)/*.mod
	@printf "DONE\n"

# core ----------------------------------------------------
$(EXE): $(OBJ)
	$(FC) $(FLAGS) -J$(MODDIR) -o $(EXE) $(OBJ)

$(OBJ): | $(OBJDIR)
$(OBJ): | $(MODDIR)
$(OBJ): $(OBJDIR)/%.o: $(SRCDIR)/%.f90
	$(FC) $(FLAGS) -J$(MODDIR) -c -o $@ $<

$(OBJDIR):
	mkdir -p $(OBJDIR)

$(MODDIR):
	mkdir -p $(MODDIR)

