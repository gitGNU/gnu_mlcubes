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

BYTLIBS=\
 graphics.cma

BINLIBS=$(BYTLIBS:.cma=.cmxa)

MLIFILES=\
 geometry.mli\
 graph.mli\
 main.mli

MLFILES=\
 geometry.ml\
 graph.ml\
 main.ml

CAMLFILES=\
 $(MLIFILES)\
 $(MLFILES)

INTERFACES=$(MLIFILES:.mli=.cmi)\

BYTOBJS=$(MLFILES:.ml=.cmo)

BINOBJS=$(BYTOBJS:.cmo=.cmx)

OBJS=\
 $(BYTOBJS)\
 $(BINOBJS)

SPURIOUS=\
 $(MLFILES:.ml=.annot)\
 $(MLFILES:.ml=$(OBJEXT))

EXENAME=mlcubes

BYTEXE=$(EXENAME).byt$(EXEEXT)

BINEXE=$(EXENAME).bin$(EXEEXT)

EXES=\
 $(BYTEXE)\
 $(BINEXE)