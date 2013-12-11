SHELL:=/bin/bash
.SUFFIXES:
.SUFFIXES: .c .o .y .a .so
#gcc has a nice -Og flag for optimizations w/debugging, clang does not
ifeq ($(CC),clang)
OPT_FLAGS:=-O2 -fno-lto
CXX:=clang++
WARNING_FLAGS:=-w
else ifeq ($(CXX),clang++)
OPT_FLAGS:=-O2 -fno-lto
CC:=clang
WARNING_FLAGS:=-w
else
CC:=gcc
CXX:=g++
OPT_FLAGS:=-Og
endif
CC:=$(CC) -fPIC #position independent code, for everything
OPT_FLAGS:=$(OPT_FLAGS) -g
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING -DNDEBUG
WARNING_FLAGS:=$(WARNING_FLAGS) -Wparentheses -Wsequence-point -Warray-bounds -Wenum-compare -Wmissing-field-initializers -Wimplicit -Wstrict-aliasing -fmax-errors=30 -Wmissing-braces -Wcomment
COMMON_CFLAGS=-std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums\
	-flto -rdynamic #-fstrict-aliasing
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
	tree.c sequence.c hash.c lisp_types.c
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o env.o array.o bignum.o \
	hash_fn.o lisp_math.o cffi.o ccall.o emacs_regex.o regex.o lisp_system.o unicode.o \
	tree.o sequence.o hash.o lisp_types.o
STD_LIB:= cons.o array.o bignum.o lisp_math.o cffi.o ccall.o regex.o emacs_regex.o \
	lisp_system.o unicode.o hash.o lisp_types.o
STD_LIB_SRC:=cons.c array.c bignum.c lisp_math.c cffi.c ccall.c regex.c emacs_regex.c \
	lisp_system.c unicode.c hash.c lisp_type.c
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
	doc info pdf clean_doc libprim_reqs readline llvm gc gmp gmp.tar.xz\
	mpfr.tar.xz libs test SciLisp_test
#Programs to be built
test: SciLisp_test
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(CFLAGS) $(XCFLAGS) $(FRONTEND) $(BACKEND) -fno-lto -o $@
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
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS_NOWARN) -w -c lex.yy.c -o lex.yy.o
unicode.o: unicode.c
parser.o: parser.c $(COMMON_HEADERS) cons.h
cons.o: cons.c $(COMMON_HEADERS) cons.h
print.o: print.c $(COMMON_HEADERS) cons.h prim.h
sequence.o: sequence.c $(COMMON_HEADERS) sequence.h
frontend.o: frontend.c $(COMMON_HEADERS) prim.h
	$(CC) $(CFLAGS) -c frontend.c -o frontend.o -fno-var-tracking-assignments
eval.o: eval.c $(COMMON_HEADERS) cons.h array.h
codegen.o: codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
	$(CC) $(XCFLAGS) -c c_codegen.c -o codegen.o
# or $(CXX) $(XCFLAGS) $(LLVM_FLAGS) c_codegen.o llvm_codegen.o -o codegen.o
c_codegen.o:codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
cffi.o:cffi.h $(COMMON_HEADERS)
ccall.o:cffi.h $(COMMON_HEADERS)
llvm_codegen.o:llvm_codegen.c codegen.h $(COMMON_HEADERS) prim.h cons.h llvm_c.h
	$(compile_llvm)
llvm_test.o: llvm_test.c llvm_c.h
	$(compile_llvm)
env.o: env.c $(COMMON_HEADERS)
array.o: array.c $(COMMON_HEADERS) array.h
prim.o: prim.c $(COMMOM_HEADERS) array.h cons.h
bignum.o: bignum.c $(COMMON_HEADERS) prim.h
hash_fn.o: hash_fn.c hash_fn.h
tree.o: tree.c $(COMMON_HEADERS) cons.h array.h
lisp_math.o: lisp_math.c $(COMMON_HEADERS) bignum.h
lisp_types.o: lisp_types.c $(COMMON_HEADERS) prim.h
fnv_hash: fnv_hash.c
	$(CC) $(CFLAGS) -O3 fnv_hash.c -o fnv_hash
emacs_regex.o: emacs_regex.c emacs_regex.h
regex.o: regex.c regex.h $(COMMON_HEADERS) emacs_regex.o
prim.c prim.h: extra/generate_prims.el extra/primc_header.c extra/primh_header.h fnv_hash
	./extra/generate_prims.el
lisp_system.o: lisp_system.c $(COMMON_HEADERS)
hash.o: hash.c hash.h $(COMMON_HEADERS) hash_fn.h
#making libraries
LIBSCILISP_FLAGS:=$(COMMON_CFLAGS) $(INCLUDE_FLAGS) -O3
lib_files:
	mkdir -p lib_files
#should be a way to do this in less lines
define start_libSciLisp =
	$(eval CC_TEMP:=$(CC) $(LIBSCILISP_FLAGS))#$(QUIET_FLAGS)
	$(CC_TEMP) -o lib_files/libSciLisp_prim.o -c prim.c
	$(CC_TEMP) -o lib_files/libSciLisp_cons.o -c  cons.c
	$(CC_TEMP) -o lib_files/libSciLisp_array.o -c array.c
	$(CC_TEMP) -o lib_files/libSciLisp_eval.o -c eval.c
	$(CC_TEMP) -o lib_files/libSciLisp_print.o -c print.c
	$(CC_TEMP) -o lib_files/libSciLisp_env.o -c env.c
	$(CC_TEMP) -o lib_files/libSciLisp_bignum.o -c bignum.c
	$(CC_TEMP) -o lib_files/libSciLisp_hash_fn.o -c hash_fn.c
	$(CC_TEMP) -o lib_files/libSciLisp_math.o -c lisp_math.c
	$(CC_TEMP) -o lib_files/libSciLisp_ccall.o -c ccall.c
	$(CC_TEMP) -o lib_files/libSciLisp_cffi.o -c cffi.c
	$(CC_TEMP) -o lib_files/libSciLisp_system.o -c lisp_system.c
endef
libSciLisp_reqs: prim.c eval.c print.c env.c cons.c array.c bignum.c hash_fn.c lisp_math.c lib_files ccall.c cffi.c lisp_system.c
define libSciLisp_files :=
lib_files/libSciLisp_prim.o lib_files/libSciLisp_env.o lib_files/libSciLisp_cons.o \
	lib_files/libSciLisp_array.o lib_files/libSciLisp_eval.o  \
	lib_files/libSciLisp_print.o lib_files/libSciLisp_bignum.o \
	lib_files/libSciLisp_hash_fn.o lib_files/libSciLisp_math.o \
	lib_files/libSciLisp_ccall.o lib_files/libSciLisp_cffi.o \
	lib_files/libSciLisp_system.o
endef
LD_SHARED_FLAGS:= -Wl,-R$(shell pwd) -Wl,-shared -Wl,-soname=libSciLisp.so
libSciLisp.o: libSciLisp_reqs
	$(start_libSciLisp)
	$(CC) -o libSciLisp.o $(LIBSCILISP_FLAGS) -lm -lgc -lcord
libSciLisp.a: libSciLisp_reqs
	$(start_libSciLisp)
	ar rcs $@ $(libSciLisp_files)
	rm $(libSciLisp_files)
libSciLisp.so: libSciLisp_reqs
	$(start_libSciLisp)
	$(CC) $(XCFLAGS) -shared -lcord -lm -lgc $(LD_SHARED_FLAGS) \
	$(libSciLisp_files) -o $@
libs: libSciLisp.a libSciLisp.so
prim.bc: prim.c eval.c print.c env.c cons.c array.c bignum.c
	$(eval CC:=clang $(QUIET_FLAGS) $(LIBPRIM_FLAGS) -w)
	$(CC) -S -emit-llvm -fno-asm $^;\
	llvm-link prim.s cons.s eval.s array.s env.s print.s -o prim.bc;\
	rm prim.s cons.s eval.s array.s env.s print.s
	opt -O3 prim.bc -o prim.bc
	llvm-dis prim.bc -o prim.ll
#.PHONY targets(set flags, clean etc..)
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
gmp: bignum/gmp/configure
	cd bignum/gmp && ./configure --enable-alloca=alloca --prefix=$$PWD/.. \
	&& $(MAKE) install
mpfr: bignum/mpfr/configure
	cd bignum/mpfr && ./configure --prefix=$$PWD/.. \
	--with-gmp-build=$$PWD/../gmp/ && $(MAKE) install
gmp.tar.xz:
	cd bignum && wget ftp://ftp.gnu.org/gnu/gmp/gmp-5.1.3.tar.xz \
	&& mv gmp-5.1.3.tar.xz gmp.tar.xz
mpfr.tar.xz:
	cd bignum && http://www.mpfr.org/mpfr-current/mpfr-3.1.2.tar.xz \
	&& mv mpfr-3.1.2.tar.xz mpfr.tar.xz
write_prims: write_prims.c libSciLisp.so
	gcc -o write_prims -std=gnu99 write_prims.c -lgc -lcord -lgmp libSciLisp.so -Wl,-rpath=$$PWD -L$$PWD -lm
gen_cffi: gen_cffi.c gen_cffi.h
	gcc -o gen_cffi $(XLDFLAGS) -lclang -O2 -g gen_cffi.c -std=gnu99
#http://physics.nist.gov/cuu/Constants/Table/allascii.txt
