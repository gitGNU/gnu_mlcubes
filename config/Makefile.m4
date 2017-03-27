ifelse(true,false,
########################################################################
# Copyright (C) 2017 Clément Franchini                                 #
#                                                                      #
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
)dnl
include(.config.m4)dnl
include(OS.m4)dnl
include(MODE.m4)dnl
SETPREFIX`'dnl
SETOCAMLFLAGS`'dnl
OCAMLC=ocamlc`'EXEEXT`'USEANNOT`'USEOCAMLFLAGS
OCAMLOPT=ocamlopt`'EXEEXT`'USEOCAMLFLAGS
OCAMLDEP=ocamldep`'EXEEXT
RM=rm`'EXEEXT -f
CHMOD=chmod`'EXEEXT
MKDIR=mkdir`'EXEEXT -p
CP=cp`'EXEEXT

include Objs.mk

.PHONY: all clean depend`'INSTALLPHONY

all: byt bin

clean:
	$(RM) $(EXES) $(OBJS) $(INTERFACES) $(SPURIOUS)

depend: $(CAMLFILES)
	@$(RM) .depend.mk
	$(OCAMLDEP) $(CAMLFILES) >.depend.mk
CHMODHACK(.depend.mk)dnl
	@$(CHMOD) -w .depend.mk

INSTALL`'dnl
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
