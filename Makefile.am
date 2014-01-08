#use recursive make for programs, but just but documentation rules here
SUBDIRS = src
info_TEXINFOS = doc/SciLisp.texi
ACLOCAL_AMFLAGS=-I m4
all:
	[ -h SciLisp ] && unlink SciLisp || true
	[ ! -e SciLisp ] && ln -s src/SciLisp SciLisp
