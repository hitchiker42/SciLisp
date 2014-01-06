SHELL=/bin/bash
#variables for use in Makefile.am
QUIET_FLAGS = -DHERE_OFF -DQUIET_LEXING -DNDEBUG
SCILISP_INCLUDES =common.h prim.h types.h cons.h lex.yy.h print.h array.h cffi.h sequence.h
COMMON_INCLUDES =common.h debug.h types.h env.h
#FRONTEND_SRC =array.c bignum.c ccall.c cffi.c cons.c env.c frontend.c \
#	hash.c hash_fn.c lex.yy.c lisp_macros.c lisp_math.c lisp_system.c \
#	lisp_types.c parser.c print.c regex.c sequence.c SFMT.a tree.c unicode.c #setf.c
FRONTEND_SRC = frontend.c
STD_LIB_SRC = array.c bignum.c ccall.c cffi.c cons.c env.c emacs_regex.c eval.c \
	format.c hash.c hash_fn.c lex.yy.c lisp_macros.c lisp_math.c lisp_system.c \
	lisp_types.c parser.c print.c prim.c regex.c sequence.c tree.c unicode.c #setf.c
MOSTLYCLEANFILES=lex.yy.c lex.yy.h
CLEANFILES=$(MOSTLYCLEANFILES) prim.c prim.h
DISTCLEANFILES=$(CLEAN_FILES) fnv_hash
BACKEND_SRC = c_codegen.c
INCLUDE_FLAGS = -I$$PWD/gc/include/gc -I$$PWD/include
#Automake variablesA
AM_CPPFLAGS=$(INCLUDE_FLAGS) -D_GNU_SOURCE
#AM_CFLAGS=-rdynamic -fPIC -Wl,-rpath="$$PWD/gc/lib $$PWD/bignum/lib" 
AM_CFLAGS=-rdynamic -L$$PWD/gc/lib #-L$$PWD/bignum/lib
BUILT_SOURCES = prim.c prim.h lex.yy.c lex.yy.h
ACLOCAL_AMFLAGS=-I m4
SUBDIRS = SFMT
#Things to be built
bin_PROGRAMS = SciLisp
SciLisp_LDADD = SFMT/libSFMT.la libSciLisp.la
SciLisp_SOURCES = $(FRONTEND_SRC) $(BACKEND_SRC) $(SCILISP_INCLUDES)
#SciLisp_CFLAGS= $(AM_CFLAGS)
lib_LTLIBRARIES = libSciLisp.la
libSciLisp_la_SOURCES = $(STD_LIB_SRC)
libSciLisp_la_LIBADD = SFMT/libSFMT.la
info_TEXINFOS = doc/SciLisp.texi
test: SciLisp_test
SciLisp_llvm: $(FRONTEND) $(BACKEND) $(SCILISP_INCLUDES) llvm_codegen.o
	$(CXX) $(LLVM_FLAGS) -DUSE_LLVM $(FRONTEND) $(BACKEND) \
	$(XLDFLAGS) $(COMMON_CFLAGS) llvm_codegen.o -o $@
llvm_test: llvm_codegen.o llvm_test.o libSciLisp.so prim.bc
	 $(CXX)	llvm_codegen.o llvm_test.o -DUSE_LLVM \
	`$(LLVM_CONFIG) --cflags --ldflags --libs all` $(INCLUDE_FLAGS) \
	 $(XLDFLAGS) $(COMMON_CFLAGS) -L$(shell pwd) \
	 libSciLisp.so -Wl,-rpath=$(shell pwd) -g -o llvm-test
SciLisp_test: SciLisp
	./SciLisp -r
#compiled files
lex.yy.c lex.yy.h: lisp.lex $(COMMON_HEADERS)
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS_NOWARN) -w -c lex.yy.c -o lex.yy.o
fnv_hash: fnv_hash.c
	$(CC) $(CFLAGS) -O3 fnv_hash.c -o fnv_hash
prim.h: prim.c
prim.c: extra/generate_prims.el extra/primc_header.c extra/primh_header.h fnv_hash.c
	./extra/generate_prims.el
llvm_codegen.o:llvm_codegen.c codegen.h $(COMMON_HEADERS) prim.h cons.h llvm_c.h
	$(compile_llvm)
llvm_test.o: llvm_test.c llvm_c.h
	$(compile_llvm)
codegen.o: codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
	$(CC) $(XCFLAGS) -c c_codegen.c -o codegen.o
prim.bc: prim.c eval.c print.c env.c cons.c array.c bignum.c
	clang $(QUIET_FLAGS) $(LIBPRIM_FLAGS) -w -S -emit-llvm -fno-asm $^;\
	llvm-link prim.s cons.s eval.s array.s env.s print.s -o prim.bc;\
	rm prim.s cons.s eval.s array.s env.s print.s
	opt -O3 prim.bc -o prim.bc
	llvm-dis prim.bc -o prim.ll
quiet:
	CFLAGS=$(QUIET_FLAGS) $(MAKE) -B all
force:
	$(MAKE) -B all
gen_cffi: gen_cffi.c gen_cffi.h
	gcc -o gen_cffi $(XLDFLAGS) -lclang -O2 -g gen_cffi.c -std=gnu99
