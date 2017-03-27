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
define(`SETPREFIX',`dnl
`PREFIX'=PREFIX

')dnl
define(`SETOCAMLFLAGS',)dnl
define(`USEOCAMLFLAGS',)dnl
define(`USEANNOT',)dnl
define(INSTALLPHONY,` install')dnl
define(INSTALL,`dnl
install:
	$(MKDIR) USEDESTDIR$(`PREFIX')`'SEP`'bin
	$(CP) $(BINEXE) USEDESTDIR$(`PREFIX')`'SEP`'bin`'SEP`'dnl
$(EXENAME)`'EXEEXT

')dnl