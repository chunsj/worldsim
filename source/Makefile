# File: world-sim/Makefile
# SCCS Version: %W%
# Contains: Source code maintenance and development makefile
# Author: Jeff Dalton <J.Dalton@ed.ac.uk>
# Created: November 1994
# Updated: Mon Oct  9 18:33:05 1995 by Jeff Dalton
# Release Version: %Y%
# Copyright: (c) 1993, 1994 AIAI, University of Edinburgh
# This material may be reproduced by or for the U.S. Government pursuant
# to the copyright license under the clause at DFARS 252.227-7032
# (June 1975) -- Rights in Technical Data and Computer Software (Foreign).

# This Makefile can be used to perform various operations on the
# "world-sim"  source directory.

# "make rdate" writes a file that makes *world-release-date* have
# the current date as its value.  The file is loaded when the world
# code is compiled and when a executable world image is built.

# "make tar" makes a gzipped tar file of the sources.

# The code can be compiled by using one of the "-compile" targets.
# E.g. "make lucid-compile".

# The "-build" targets build an executable image.  E.g "make lucid-build".

# The "-world" targets both compile and build by calling "make"
# recursively.  So "make lucid-world" does "make lucid-compile" and
# then "make lucid-build".

# The Lucid targets (such as lucid-build) use Lucid-specific command-line
# arguments; the KCL targets are much less KCL-specific and are therefore
# easier to generalize.  The "generic-" targets are one such generalization.


# Edit the following line(s) to define commands for running Lisp.
Lucid   = lucid4.1+clos
KCL     = gcl-1.1
Allegro = allegro-4.2

# This is the default lisp for the "generic" make targets:
Lisp = ${Allegro}

# Here's the default image name:
World-image = world-image

# A command that runs WorldSim:
WorldSim = worldsim

# What to make by default:
Target = lucid-world


all:
	${MAKE} ${Target}
	${MAKE} WorldSim=./test-world compile-known-worlds


# Install

Base-dir  = ..
Lib-dir   = ${Base-dir}/lib
Bin-dir   = ${Base-dir}/bin

Scripts   = worldsim worldsim-remote

install:
	mv ${World-image} ${Lib-dir}

${Lib-dir}:
	mkdir ${Lib-dir}

${Bin-dir}:
	mkdir ${Bin-dir}


# Release date
#
# Writes something like (defvar *world-release-date* "29-Oct-94")
# to a new world-release-date.lsp.
#
# %h works for the month with both SunOS 4.1.3 and FreeBSD.
#
rdate:
	rm -f world-release-date.lsp
	echo '(in-package :user)' > world-release-date.lsp
	echo '(defvar *world-release-date* "'`date +%d-%h-%y`'")' \
		>> world-release-date.lsp


# For gcc, when /tmp is too small, setenv TMPDIR = .
# This can happen when using KCL.


# Compile and build combinations

lucid-world:
	${MAKE} lucid-compile
	${MAKE} lucid-build

kcl-world:
	${MAKE} kcl-compile
	${MAKE} kcl-build

allegro-world:
	${MAKE} Lisp=${Allegro} generic-compile
	${MAKE} Lisp=${Allegro} generic-build


# Image-building

lucid-build:
	${Lucid} -load world-prep.lsp \
		 -eval "(load-system 'world)" \
		 -eval "(world:save-world \"${World-image}\")" \
		 -quit

kcl-build:
	echo "(load \"world-prep.lsp\") \
	      (load-system 'world) \
	      (world:save-world \"${World-image}\") \
	      (bye)" \
	  | ${KCL}

generic-build:
	echo "(load \"world-prep.lsp\") \
	      (load-system 'world) \
	      (world:save-world \"${World-image}\") \
	      (util:exit-lisp)" \
	  | ${Lisp}


# Compilation

lucid-compile:
	${Lucid} -load world-prep.lsp \
		 -eval "(compile-system 'world)" \
		 -quit

lucid-compile-warnings:
	${Lucid} -load world-prep.lsp \
		 -eval "(setq *break-on-warnings* t)" \
		 -eval "(compile-system 'world)" \
		 -quit

kcl-compile:
	echo "(load \"world-prep.lsp\") \
	      (use-fast-links t) \
	      (compile-system 'world) \
	      (bye)" \
	  | ${KCL}

generic-compile:
	echo "(load \"world-prep.lsp\") \
	      (compile-system 'world) \
	      (util:exit-lisp)" \
	  | ${Lisp}


# World compilation

compile-known-worlds:
	${WorldSim} -eval "(world:compile-known-worlds)" \
		    -eval "(exit-lisp)"


# Clean

lucid-clean:
	${Lucid} -load world-prep.lsp \
		 -eval "(clean-system 'world)" \
		 -quit

kcl-clean:
	echo "(load \"world-prep.lsp\") \
	      (clean-system 'world) \
	      (bye)" \
	  | ${KCL}

generic-clean:
	# Loads dependent-support to get util:exit-lisp.
	echo "(load \"world-prep.lsp\") \
	      (load-system 'dependent-support) \
	      (clean-system 'world) \
	      (util:exit-lisp)" \
	  | ${Lisp}


# World cleaning

clean-known-worlds:
	${WorldSim} -eval "(world:clean-known-worlds)" \
		    -eval "(exit-lisp)"


# Release

Instructions = INSTALL README ../../GNU_General_Public_License

ExtraSource = Makefile world-prep.lsp lisp-prep.lsp world-release-date.lsp \
	      world-systems.lsp self-copier.lsp world-init test-world

ConfigFiles = config/*.config ../config/monitor.def

Documentation = ../../world-docs/manual/worldsim.ps

ReleaseDir = ../../worldsim-test

# "make release" makes a copy of WorldSim which can then be made into
# a tar file.  "make release" has to be done in the "world-sim" subdirectory
# of the O-plan "source" directory, and the result is not quite a copy,
# because the directory structure and file locations are somewhat different.

release: copy-source
	rm -rf ${ReleaseDir}
	mkdir ${ReleaseDir}
	mkdir ${ReleaseDir}/lib
	mkdir ${ReleaseDir}/bin
	mkdir ${ReleaseDir}/source
	mkdir ${ReleaseDir}/source/support
	mkdir ${ReleaseDir}/source/support/parser-kit
	mkdir ${ReleaseDir}/documentation
	cp -p ${Instructions} ${ReleaseDir}
	cp -p ${ExtraSource} ${ReleaseDir}/source
	cp -p ${Scripts} ${ReleaseDir}/bin
	cp -p ${ConfigFiles} ${ReleaseDir}/lib
	cp -p ${Documentation} ${ReleaseDir}/documentation
	./copy-source ${ReleaseDir}/source
	${MAKE} copy-worlds-for-release
	mkdir ${ReleaseDir}/xmenu
	cp -p ../xmenu/* ${ReleaseDir}/xmenu
	cd ${ReleaseDir}/xmenu; make clean

# N.B. remake copy-source if you change which Lisp you're using.

# You may want to use: make WorldSim=./test-world copy-source

copy-source: world-systems.lsp self-copier.lsp
	rm -f copy-source
	${WorldSim} -load "self-copier.lsp" \
	            -eval "(world::write-copy-script)" \
		    -eval "(exit-lisp)"
	chmod a+x copy-source

copy-worlds-for-release:
	(cd ..; tar chf - worlds) | (cd ${ReleaseDir}/lib; tar xpf -)
	# Clean out object and tilde files
	cd ${ReleaseDir}/lib/worlds; \
	    find . \( -name "*.o" -o -name "*.sbin" -o -name "*.s2bin" \
                      -o -name "*.fasl" -o -name "gazonk*" -o -name "*~" \) \
	       -print | xargs rm


# Tarfile

# N.B. Dereferences symbolic links

# Need up to */*/*.lsp because of support/parser-kit.

TarDir = . 

tar:
	gnutar zcvhf ${TarDir}/world-sim.`date +%d%h%y`.tar.gz \
		README INSTALL Makefile \
		world-init test-world worldsim worldsim-remote
		*.lsp */*.lsp */*/*.lsp config/*.config


# End
