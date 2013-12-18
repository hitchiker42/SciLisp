SHELL:=/bin/bash
.SUFFIXES:
.SUFFIXES: .c .o .y .a .so
#gcc has a nice -Og flag for optimizations w/debugging so use that if possible
#right now any other compiler fails to produce a correct executable anyway
ifeq ($(CC),cc)
CC:=gcc
endif
ifeq ($(CXX),c++)
CXX:=g++
endif
ifneq ($(CC),gcc)
OPT_FLAGS:=-O1 -fno-lto
WARNING_FLAGS:=-w
NOT_GCC=1
endif
ifneq ($(CXX),g++)
OPT_FLAGS:=-O1 -fno-lto
WARNING_FLAGS:=-w
NOT_GCC=1
endif
ifndef NOT_GCC
OPT_FLAGS=-Og -flto
endif
CC:=$(CC) -fPIC #position independent code, for everything
OPT_FLAGS:=$(OPT_FLAGS) -g
QUIET_FLAGS:= -DHERE_OFF -DQUIET_LEXING -DNDEBUG
WARNING_FLAGS:=$(WARNING_FLAGS) -Wparentheses -Wsequence-point -Warray-bounds -Wenum-compare \
	 -Wmissing-field-initializers -Wimplicit -Wstrict-aliasing -fmax-errors=30 -Wmissing-braces -Wcomment
COMMON_CFLAGS=-std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums	\
	-rdynamic #-fstrict-aliasing
#-I$(shell pwd)/llvm/llvm/include
INCLUDE_FLAGS:=-I$(shell pwd)/gc/include/gc -I$(shell pwd)/bignum/include
#-Wl,-rpath=$(shell pwd)/readline/lib	-I$(shell pwd)/readline/include
XLDFLAGS:=-Wl,-rpath=$(shell pwd)/gc/lib \
	-Wl,-rpath=$(shell pwd)/bignum/lib \
	-lgc -lm -lreadline -lcord -rdynamic -lpthread -lgmp -lmpfr -ldl
XCFLAGS=$(WARNING_FLAGS) $(XLDFLAGS) $(COMMON_CFLAGS) $(INCLUDE_FLAGS) $(OPT_FLAGS)
XCFLAGS_NOWARN=-g $(COMMON_CFLAGS) $(XLDFLAGS) $(INCLUDE_FLAGS) $(OPT_FLAGS)
LEX:=flex
SCILISP_HEADERS:=common.h prim.h types.h cons.h lex.yy.h print.h array.h cffi.h sequence.h
COMMON_HEADERS:=common.h debug.h types.h env.h
FRONTEND_SRC:=lex.yy.c parser.c cons.c print.c frontend.c env.c array.c bignum.c \
	hash_fn.c lisp_math.c cffi.c ccall.c regex.c lisp_system.c unicode.c \
	tree.c sequence.c hash.c lisp_types.c #setf.c
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o env.o array.o bignum.o \
	hash_fn.o lisp_math.o cffi.o ccall.o emacs_regex.o regex.o lisp_system.o unicode.o \
	tree.o sequence.o hash.o lisp_types.o #setf.o
STD_LIB:= cons.o array.o bignum.o lisp_math.o cffi.o ccall.o regex.o emacs_regex.o \
	lisp_system.o unicode.o hash.o lisp_types.o #setf.o
STD_LIB_SRC:=cons.c array.c bignum.c lisp_math.c cffi.c ccall.c regex.c emacs_regex.c \
	lisp_system.c unicode.c hash.c lisp_type.c #setf.c
BACKEND_SRC:=eval.c codegen.c prim.c
BACKEND:=eval.o codegen.o prim.o
CFLAGS:=$(CFLAGS) $(XCFLAGS) $(OPT_FLAGS)
LLVM_CONFIG:=$(shell pwd)/llvm/llvm/bin/llvm-config
LLVM_FLAGS:=$(OPT_FLAG) -Wl,-rpath=$(shell pwd)/llvm/Release/lib \
	-L$$PWD/llvm/Release/lib -lLLVM-3.{4,5}svn
define compile_llvm =
	$(CC) $(CFLAGS) `$(LLVM_CONFIG) --cppflags` -c $< -o $@
endef
.PHONY: clean all quiet asm optimized set_quiet set_optimized mpfr\
	doc info pdf clean_doc libSciLisp_reqs readline llvm gc gmp \
	libs test SciLisp_test
#Programs to be built
test: SciLisp_test
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(CFLAGS) $(XCFLAGS) $(FRONTEND) $(BACKEND) -o $@
SciLisp_llvm: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS) llvm_codegen.o
	$(CXX) $(LLVM_FLAGS) -DUSE_LLVM $(FRONTEND) $(BACKEND) \
	$(XLDFLAGS) $(COMMON_CFLAGS) llvm_codegen.o -o $@
llvm_test: llvm_codegen.o llvm_test.o libSciLisp.so prim.bc
	 $(CXX)	llvm_codegen.o llvm_test.o -DUSE_LLVM \
	`$(LLVM_CONFIG) --cflags --ldflags --libs all` $(INCLUDE_FLAGS) \
	 $(XLDFLAGS) $(COMMON_CFLAGS) -L$(shell pwd) \
	 libSciLisp.so -Wl,-rpath=$(shell pwd) -g -o llvm-test
SciLisp_test: SciLisp
	./SciLisp -r
all: SciLisp libs test
#compiled files
lex.yy.c: lisp.lex $(COMMON_HEADERS)
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS_NOWARN) -w -c lex.yy.c -o lex.yy.o
array.o: array.c $(COMMON_HEADERS) array.h
bignum.o: bignum.c $(COMMON_HEADERS) prim.h
c_codegen.o:codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
ccall.o:cffi.h $(COMMON_HEADERS)
cffi.o:cffi.h $(COMMON_HEADERS)
cons.o: cons.c $(COMMON_HEADERS) cons.h
emacs_regex.o: emacs_regex.c emacs_regex.h
setf.o: setf.c $(COMMON_HEADERS) prim.h
env.o: env.c $(COMMON_HEADERS)
eval.o: eval.c $(COMMON_HEADERS) cons.h array.h
frontend.o: frontend.c $(COMMON_HEADERS) prim.h
hash.o: hash.c hash.h $(COMMON_HEADERS) hash_fn.h
hash_fn.o: hash_fn.c hash_fn.h
lisp_math.o: lisp_math.c $(COMMON_HEADERS) bignum.h
lisp_system.o: lisp_system.c $(COMMON_HEADERS)
lisp_types.o: lisp_types.c $(COMMON_HEADERS) prim.h
parser.o: parser.c $(COMMON_HEADERS) cons.h
prim.o: prim.c $(COMMOM_HEADERS) array.h cons.h
print.o: print.c $(COMMON_HEADERS) cons.h prim.h
regex.o: regex.c regex.h $(COMMON_HEADERS) emacs_regex.o
sequence.o: sequence.c $(COMMON_HEADERS) sequence.h
tree.o: tree.c $(COMMON_HEADERS) cons.h array.h
unicode.o: unicode.c
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
# or $(CXX) $(XCFLAGS) $(LLVM_FLAGS) c_codegen.o llvm_codegen.o -o codegen.o
#making libraries
LIBSCILISP_FLAGS:=$(COMMON_CFLAGS) $(INCLUDE_FLAGS) -O3
LIBSCILISP_CC:=$(CC) $(LIBSCILISP_FLAGS)
lib_files:
	mkdir -p lib_files
compile_lib_file = $(CC_TEMP) -o lib_files/libSciLisp_$(file:.c=.o) -c $(file)
libSciLisp_reqs := prim.c eval.c print.c env.c cons.c array.c bignum.c hash_fn.c \
	lisp_math.c ccall.c cffi.c lisp_system.c sequence.c regex.c lisp_types.c \
	parser.c lex.yy.c hash.c tree.c unicode.c
libSciLisp_files := $(addprefix lib_files/libSciLisp_,$(libSciLisp_reqs:.c=.o))
start_libSciLisp: $(libSciLisp_files)
$(libSciLisp_files): lib_files/libSciLisp_%.o: %.c
	$(LIBSCILISP_CC) -o $@ -c 
LD_SHARED_FLAGS:= -Wl,-R$(shell pwd) -Wl,-shared -Wl,-soname=libSciLisp.so
libSciLisp.o: start_libSciLisp
	$(CC) -o libSciLisp.o $(LIBSCILISP_FLAGS) -lm -lgc -lcord
libSciLisp.a: start_libSciLisp
	ar rcs $@ $(libSciLisp_files)
libSciLisp.so: start_libSciLisp
	$(CC) $(XCFLAGS) -shared $(LD_SHARED_FLAGS) \
	$(libSciLisp_files) -o $@
libs: libSciLisp.a libSciLisp.so
prim.bc: prim.c eval.c print.c env.c cons.c array.c bignum.c
	$(eval CC:=clang $(QUIET_FLAGS) $(LIBPRIM_FLAGS) -w)
	$(CC) -S -emit-llvm -fno-asm $^;\
	llvm-link prim.s cons.s eval.s array.s env.s print.s -o prim.bc;\
	rm prim.s cons.s eval.s array.s env.s print.s
	opt -O3 prim.bc -o prim.bc
	llvm-dis prim.bc -o prim.ll
set_quiet:
	$(eval CFLAGS=$(CFLAGS) $(QUIET_FLAGS) )
quiet:set_quiet all
force:
	$(MAKE) -B all
set_optimized:
	$(eval OPT_FLAGS:=-O3 -march=native)
	$(eval CFLAGS:=$(COMMON_CFLAGS) $(QUIET_FLAGS) $(OPT_FLAGS))
optimized:set_optimized all
clean:
	rm -f *.o
	rm -f prim.bc;rm -f prim.ll;rm -f libSciLisp.so;rm -f libSciLisp.a
	rm -f SciLisp;rm -f llvm_test
#documentation
doc:
	cd doc && $(MAKE) all
info:
	cd doc && $(MAKE) info
pdf: doc/manual.texi
	cd doc && $(MAKE) pdf
clean_doc:
	cd doc && rm $$(find '!' '('-name "*.texi" '-o' -name "*.m4" ')' -type f)
#external dependencys
# the dependency basically just makes sure the readline dir isn't empty
PYTHON2:=$(shell if [ $$(python --version 2>&1  | awk '{ print(substr($$2,1,1)) }') -ge 3 ];then echo '/usr/bin/python2';else echo '/usr/bin/python';fi)
readline: readline/configure
	cd readline && ./configure --prefix=$$PWD --enable-multibyte \
	&& $(MAKE) install
llvm:  llvm/configure
	cd llvm && mkdir -p llvm && ./configure --prefix=$$PWD/llvm \
	--enable-optimized --enable-shared\
	 --with-python=$(PYTHON2) && $(MAKE) -k install
	mv llvm/Release+Asserts llvm/Release
clang: llvm/tools/clang
llvm/tools/clang:
	cd llvm/tools && git clone http://llvm.org/git/clang.git
gc/gc_config.stamp:
	if [ ! -f gc/bdwgc/configure ];then cd gc/bdwgc && ./autogen.sh; fi
	if [ ! -f gc/libatomic_ops/configure ];then cd gc/libatomic_ops && autogen.sh;fi
	if [ ! -h gc/bdwgc/libatomic_ops ]; then ln -s $$PWD/gc/libatomic_ops\
	$$PWD/gc/bdwgc/libatomic_ops; fi
	touch gc/gc_config.stamp
gc: gc/gc_config.stamp gc/bdwgc/autogen.sh gc/libatomic_ops/autogen.sh
	cd gc/bdwgc && ./configure --with-threads=posix --disable-java-finalization \
	CFLAGS='-DGC_LINUX_THREADS -DPARALLEL_MARK -DTHREAD_LOCAL_ALLOC -DALL_INTERIOR_POINTERS'	\
	--enable-parallel-mark --prefix=$$PWD/.. && $(MAKE) install cord
	cp gc/bdwgc/include/cord_pos.h gc/include/gc
bignum/gmp/configure: bignum/gmp.tar.xz
	mkdir -p bignum/gmp
	cd bignum && tar -xf gmp.tar.xz
bignum/mpfr/configure: bignum/mpfr.tar.xz
	mkdir -p bignum/mprf
	cd bignum && tar -xf mpfr.tar.xz
gmp: bignum/gmp/configure
	cd bignum/gmp && ./configure --enable-alloca=alloca --prefix=$$PWD/.. \
	&& $(MAKE) install
mpfr: bignum/mpfr/configure
	cd bignum/mpfr && ./configure --prefix=$$PWD/.. \
	--with-gmp-build=$$PWD/../gmp/ && $(MAKE) install
bignum/gmp.tar.xz:
	cd bignum && wget ftp://ftp.gnu.org/gnu/gmp/gmp-5.1.3.tar.xz \
	&& mv gmp-5.1.3.tar.xz gmp.tar.xz
bignum/mpfr.tar.xz:
	cd bignum && http://www.mpfr.org/mpfr-current/mpfr-3.1.2.tar.xz \
	&& mv mpfr-3.1.2.tar.xz mpfr.tar.xz
gen_cffi: gen_cffi.c gen_cffi.h
	gcc -o gen_cffi $(XLDFLAGS) -lclang -O2 -g gen_cffi.c -std=gnu99
#http://physics.nist.gov/cuu/Constants/Table/allascii.txt
