# makefile for isl.

# Author: Thierry Volpiatto.
# Copyright (C) 2018, Thierry Volpiatto, all rights reserved.

## This file is NOT part of GNU Emacs
##
## License
##
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 3, or (at your option)
## any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with this program; see the file COPYING.  If not, write to
## the Free Software Foundation, Inc., 51 Franklin Street, Fifth
## Floor, Boston, MA 02110-1301, USA.

# Emacs invocation
EMACS_COMMAND   := emacs

EMACS		:= $(EMACS_COMMAND) -q -batch

EVAL := $(EMACS) --eval

PKGDIR := .

# Additional emacs loadpath
LOADPATH	:= -L $(PKGDIR)
ELPA_DIR        =  $(HOME)/.emacs.d/elpa

# Files to compile
EL			:= isl.el

# Compiled files
ELC			:= $(EL:.el=.elc)


.PHONY: clean autoloads batch-compile install uninstall

all: clean autoloads batch-compile

$(ELC): %.elc: %.el
	$(EMACS) $(LOADPATH) -f batch-byte-compile $<

# Compile needed files
compile: $(ELC)

# Compile all files at once
batch-compile:
	$(EMACS) $(LOADPATH) -f batch-byte-compile $(EL)

# Remove all generated files
clean:
	rm -f $(ELC)

# Make autoloads file
autoloads:
	$(EVAL) "(progn (setq generated-autoload-file (expand-file-name \"isl-autoloads.el\" \"$(PKGDIR)\")) \
(setq backup-inhibited t) (update-directory-autoloads \"$(PKGDIR)\"))"

PREFIX=/usr/local/share/
DESTDIR=${PREFIX}emacs/site-lisp/isl/
install:
	test -d ${DESTDIR} || mkdir ${DESTDIR}
	cp -vf *.el $(DESTDIR)
	cp -vf *.elc $(DESTDIR)
	cp -vf isl-autoloads.el $(DESTDIR)

uninstall:
	rm -vf ${DESTDIR}*.elc
	rm -vf ${DESTDIR}*.el
