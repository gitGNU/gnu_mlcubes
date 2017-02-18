########################################################################
# This file is part of mlcubes.                                        #
#                                                                      #
# mlcubes is free software: you can redistribute it and/or modify it   #
# under the terms of the GNU General Public License as published by    #
# the Free Software Foundation, either version 3 of the License, or    #
# (at your option) any later version.                                  #
#                                                                      #
# mlcubes is distributed in the hope that it will be useful, but       #
# WITHOUT ANY WARRANTY; without even the implied warranty of           #
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU     #
# General Public License for more details.                             #
#                                                                      #
# You should have received a copy of the GNU General Public License    #
# along with mlcubes. If not, see <http://www.gnu.org/licenses/>.      #
########################################################################

include .config.mk
include Config-$(OS).mk

OCAMLUSERFLAGS=
OCAMLDEVFLAGS=-safe-string -strict-sequence -w A -warn-error A
OCAMLFLAGS=$(OCAML$(MODE)FLAGS)
OCAMLCCMD=ocamlc.opt$(EXEEXT)
OCAMLOPTCMD=ocamlopt.opt$(EXEEXT)
OCAMLC=$(OCAMLCCMD) -annot $(OCAMLFLAGS)
OCAMLOPT=$(OCAMLOPTCMD) $(OCAMLFLAGS)
OCAMLDEP=ocamldep$(EXEEXT)
RM=rm$(EXEEXT) -f
CHMOD=chmod$(EXEEXT)

include Objs.mk

.PHONY: all clean depend

all: byt bin

clean:
	$(RM) $(EXES) $(OBJS) $(INTERFACES) $(SPURIOUS)

depend: $(CAMLFILES)
	@$(RM) .depend.mk
	$(OCAMLDEP) $(CAMLFILES) >.depend.mk
	@$(CHMOD) 0000 .depend.mk
	@$(CHMOD) +rw .depend.mk
	@$(CHMOD) -w .depend.mk

.PHONY: bin binobjs byt bytobjs interfaces objs

bin: $(BINEXE)

binobjs: $(BINOBJS)

byt: $(BYTEXE)

bytobjs: $(BYTOBJS)

interfaces: $(INTERFACES)

objs: $(OBJS)

$(BYTEXE): $(BYTOBJS)
	$(OCAMLC) -o $(BYTEXE) $(BYTLIBS) $(BYTOBJS)

$(BINEXE): $(BINOBJS)
	$(OCAMLOPT) -o $(BINEXE) $(BINLIBS) $(BINOBJS)

.SUFFIXES: .cmi .cmo .cmx .ml .mli

.ml.cmo:
	$(OCAMLC) -c $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.mli.cmi:
	$(OCAMLC) -c $<

include .depend.mk
