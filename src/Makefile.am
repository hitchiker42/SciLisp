SHELL=/bin/bash
#variables for use in Makefile.am
QUIET_FLAGS = -DNDEBUG
SCILISP_INCLUDES = array.h bignum.h cffi.h codegen.h common.h cons.h emacs_regex.h \
	env.h hash.h lisp_utf8.h lisp_math.h lisp_strings.h prim.h regex.h \
	read.h tree.h types.h unicode.h
COMMON_INCLUDES = common.h debug.h types.h env.h frames.h print.h
#FRONTEND_SRC =array.c bignum.c ccall.c cffi.c cons.c env.c frontend.c \
#	hash.c hash_fn.c lex.yy.c lisp_macros.c lisp_math.c lisp_system.c \
#	lisp_types.c parser.c print.c regex.c sequence.c SFMT.a tree.c unicode.c #setf.c
FRONTEND_SRC = frontend.c repl.c
STD_LIB_SRC = array.c bignum.c c_codegen.c ccall.c cffi.c cons.c cord.c env.c \
	emacs_regex.c eval.c format.c hash.c interpreter.c lisp_math.c lisp_utf8.c \
	lisp_system.c lisp_types.c parser.c print.c prim.c regex.c read.c sequence.c \
	tree.c unicode.c #setf.c
SIMPLE_SRC = frontend.c repl.c read.c env.c print.c cord.c frames.c\
	lisp_utf8.c prim.c extra/memcpy_stride.S cons.c lisp_math.c\
	bignum.c lisp_types.c interpreter.c
CLEANFILES=$(MOSTLYCLEANFILES) prim.c prim.h
DISTCLEANFILES=$(CLEAN_FILES) get_hash
#BACKEND_SRC = c_codegen.c
INCLUDE_FLAGS = -I$$PWD/include
#Automake variablesA
AM_CPPFLAGS=$(INCLUDE_FLAGS) -D_GNU_SOURCE
#AM_CFLAGS=-rdynamic -fPIC -Wl,-rpath="$$PWD/gc/lib $$PWD/bignum/lib" 
AM_CFLAGS=-rdynamic -L$$PWD/lib
BUILT_SOURCES =  get_hash prim.c prim.h
#libffi and gc are subpackages
SUBDIRS = SFMT gc/bdwgc libffi
#Things to be built
bin_PROGRAMS = SciLisp read_test SciLisp_test
read_test_SOURCES = read_test.c env.c print.c cord.c frames.c \
		lisp_utf8.c read.c prim.c extra/memcpy_stride.S\
		cons.c lisp_math.c bignum.c lisp_types.c\
		interpreter.c
read_test_CFLAGS = -std=gnu99 -Og -ggdb -rdynamic
read_test_LDFLAGS = -lreadline -lmpfr -lgmp -lpthread -ldl -lgc
read_test_LDADD = SFMT/libSFMT.la 

SciLisp_LDADD = SFMT/libSFMT.la libSciLisp.la
SciLisp_SOURCES = $(FRONTEND_SRC) $(BACKEND_SRC) $(SCILISP_INCLUDES)
SciLisp_simple_SOURCES = $(SIMPLE_SRC)
SciLisp_simple_LDFLAGS = -lreadline -lmpfr -lgmp -lpthread -ldl -lgc
SciLisp_simple_LDADD = SFMT/libSFMT.la 

lib_LTLIBRARIES = libSciLisp.la
libSciLisp_la_SOURCES = $(STD_LIB_SRC)
libSciLisp_la_LIBADD = SFMT/libSFMT.la gc/bdwgc/libgc_convenience.la \
	gc/bdwgc/libcord_convenience.la libffi/libffi_convenience.la
#libSciLisp_la_includedir=$(includedir)/SciLisp
#libSciLisp_la_HEADERS = $(SCILISP_INCLUDES)

#Explicit targets
#make assembly files
p%.s : %.c 
	$(CC) $(INCLUDE_FLAGS) -Og -fverbose-asm $<
test: SciLisp_test
#gc/bdwgc/libcord.la gc/bdwgc/libgc.la: gc/bdwgc/configure
#	cd gc/bdwgc && ./configure $(GC_OPTIONS) --disable-gcj-support && $(MAKE)
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
get_hash: get_hash.c
	$(CC) $(CFLAGS) -O3 get_hash.c -o get_hash
prim.h: prim.c
prim.c: extra/generate_prims.el extra/primc_header.c extra/primh_header.h get_hash
	./extra/generate_prims.el
llvm_codegen.o:llvm_codegen.c codegen.h $(COMMON_HEADERS) prim.h cons.h llvm_c.h
	$(compile_llvm)
llvm_test.o: llvm_test.c llvm_c.h
	$(compile_llvm)
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
