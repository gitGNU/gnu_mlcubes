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
define(OCAMLFLAGS,)dnl
define(SETPREFIX,)dnl
define(SETOCAMLFLAGS,`dnl
`OCAMLFLAGS'=-safe-string -strict-sequence -w A -warn-error A
')dnl
define(USEOCAMLFLAGS,` $(`OCAMLFLAGS')')dnl
define(USEANNOT,` -annot')dnl
define(INSTALLPHONY,)dnl
define(INSTALL,)dnl
