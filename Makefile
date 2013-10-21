SHELL:=/bin/bash
.SUFFIXES:
.SUFFIXES: .c .o .y .a .so
#gcc has a nice -Og flag for optimizations w/debugging, clang does not
ifeq ($(CC),clang)
OPT_FLAGS:=-O2
CXX:=clang++
WARNING_FLAGS:=-w
else ifeq ($(CXX),clang++)
OPT_FLAGS:=-O2
CC:=clang
WARNING_FLAGS:=-w
else
CC:=gcc
CXX:=g++
OPT_FLAGS:=-Og
endif
CC:=$(CC) -fPIC #position independent code, for everything
OPT_FLAGS:=$(OPT_FLAGS) -ggdb
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING -DNDEBUG
WARNING_FLAGS:=$(WARNING_FLAGS) -Wparentheses -Wsequence-point -Warray-bounds -Wenum-compare -Wmissing-field-initializers -Wimplicit
COMMON_CFLAGS=-std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums\
	 -flto -rdynamic -fno-strict-aliasing
COMMON_CXXFLAGS:= -D_GNU_SOURCE -fno-strict-aliasing -fno-strict-enums -Wno-write-strings -I$(shell pwd)/gc/include/gc
INCLUDE_FLAGS:=-I$(shell pwd)/gc/include/gc -I$(shell pwd)/llvm/llvm/include
#-Wl,-rpath=$(shell pwd)/readline/lib 	-I$(shell pwd)/readline/include
XLDFLAGS:=-Wl,-rpath=$(shell pwd)/gc/lib \
	-Wl,-rpath=$(shell pwd)/llvm/llvm/lib \
	-lgc -lm -lreadline -lcord -rdynamic
XCFLAGS=$(WARNING_FLAGS) $(XLDFLAGS) $(COMMON_CFLAGS) $(INCLUDE_FLAGS)
XCFLAGS_NOWARN=-g $(OPT_FLAGS) $(COMMON_CFLAGS) $(XLDFLAGS) $(INCLUDE_FLAGS)
LEX:=flex
SCILISP_HEADERS:=common.h prim.h types.h cons.h lex.yy.h print.h array.h
COMMON_HEADERS:=common.h debug.h types.h
FRONTEND_SRC:=lex.yy.c parser.c cons.c print.c frontend.c env.c array.c
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o env.o array.o
BACKEND_SRC:=eval.c codegen.c
BACKEND:=eval.o codegen.o prim.o
CFLAGS:=$(CFLAGS) $(XCFLAGS) $(OPT_FLAGS)
LLVM_CONFIG:=$(shell pwd)/llvm/llvm/bin/llvm-config
LLVM_FLAGS:=`$(LLVM_CONFIG) --ldflags --cxxflags --libs core engine` $(OPT_FLAG)
CXXFLAGS:=$(CXXFLAGS) `$(LLVM_CONFIG) --cppflags` -flto -ggdb
define compile_llvm =
	$(CC) $(CFLAGS) `$(LLVM_CONFIG) --cppflags` -c $< -o $@
endef
.PHONY: clean all quiet asm optimized set_quiet set_optimized\
	doc info pdf clean_doc libprim_reqs readline llvm gc
#Programs to be built
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(CFLAGS) $(XCFLAGS) $(FRONTEND) $(BACKEND) -o $@
SciLisp_llvm: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS) llvm_codegen.o
	$(CXX) $(CXXFLAGS) $(LLVM_FLAGS) $^ -o $@
llvm_test: llvm_codegen.o llvm_test.o libSciLisp.so prim.bc
	 $(CXX)	llvm_codegen.o llvm_test.o \
	`$(LLVM_CONFIG) --cflags --ldflags --libs all` $(INCLUDE_FLAGS) \
	 $(XLDFLAGS) $(COMMON_CFLAGS) -L$(shell pwd) \
	 libSciLisp.so -Wl,-rpath=$(shell pwd) -ggdb -o llvm-test
all: SciLisp llvm_test
#compiled files
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS_NOWARN) -w -c lex.yy.c -o lex.yy.o
parser.o: parser.c $(COMMON_HEADERS) cons.h
cons.o: cons.c $(COMMON_HEADERS) cons.h
print.o: print.c $(COMMON_HEADERS) cons.h
frontend.o: frontend.c $(COMMON_HEADERS) prim.h
eval.o: eval.c $(COMMON_HEADERS) cons.h array.h
codegen.o: codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
	$(CC) $(XCFLAGS) -c c_codegen.c -o codegen.o
# or $(CXX) $(XCFLAGS) $(LLVM_FLAGS) c_codegen.o llvm_codegen.o -o codegen.o
c_codegen.o:codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
llvm_codegen.o:llvm_codegen.c codegen.h $(COMMON_HEADERS) prim.h cons.h llvm_c.h
	$(compile_llvm)
llvm_test.o: llvm_test.c llvm_c.h
	$(compile_llvm)
env.o: env.c $(COMMON_HEADERS)
array.o: array.c $(COMMON_HEADERS) array.h
prim.o: prim.c $(COMMOM_HEADERS) array.h cons.h
#making libraries
LIBPRIM_FLAGS:=$(COMMON_CFLAGS) $(INCLUDE_FLAGS) -O3
#should be a way to do this in less lines
define start_libprim =
	$(eval CC_TEMP:=$(CC) $(QUIET_FLAGS) $(LIBPRIM_FLAGS))
	$(CC_TEMP) -o libprim_prim.o -c prim.c
	$(CC_TEMP) -o libprim_cons.o -c  cons.c
	$(CC_TEMP) -o libprim_array.o -c array.c
	$(CC_TEMP) -o libprim_eval.o -c eval.c
	$(CC_TEMP) -o libprim_print.o -c print.c
	$(CC_TEMP) -o libprim_env.o -c env.c
endef
libprim_reqs: prim.c eval.c print.c env.c cons.c array.c
define libprim_files :=
libprim_prim.o libprim_env.o libprim_cons.o libprim_array.o libprim_eval.o libprim_print.o
endef
LD_SHARED_FLAGS:= -Wl,-R$(shell pwd) -Wl,-shared -Wl,-soname=libSciLisp.so
libprim.o: libprim_reqs
	$(start_libprim)
	$(CC) -o libprim.o $(LIBPRIM_FLAGS) -lm -lgc -lcord
libSciLisp.a: libprim_reqs
	$(start_libprim)
	ar rcs $@ $(libprim_files)
	rm $(libprim_files)
libSciLisp.so: libprim_reqs
	$(start_libprim)
	$(CC) $(XCFLAGS) -shared -lcord -lm -lgc $(LD_SHARED_FLAGS) $(libprim_files) -o $@
prim.bc: prim.c eval.c print.c env.c cons.c array.c
	$(eval CC:=clang $(QUIET_FLAGS) $(LIBPRIM_FLAGS))
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
	rm *.o
	rm prim.bc;rm prim.ll;rm libSciLisp.so
	rm SciLisp;rm llvm_test
#documentation
doc: info pdf
info: doc/manual.texi
	(cd doc && makeinfo manual.texi)
pdf: doc/manual.texi
	(cd doc && makeinfo --pdf manual.texi)
clean_doc:
	cd doc && rm $$(find '!' -name "*.texi" -type f)
#external dependencys
# the dependency basically just makes sure the readline dir isn't empty
PYTHON2:=$(shell if [ $$(python --version 2>&1  | awk '{ print(substr($$2,1,1)) }') -ge 3 ];then echo '/usr/bin/python2';else echo '/usr/bin/python';fi)
readline: readline/configure
	cd readline && ./configure --prefix=$$PWD --enable-multibyte \
	&& $(MAKE) install
llvm:  llvm/configure
	cd llvm && mkdir -p llvm && ./configure --prefix=$$PWD/llvm \
	--enable-optimized --enable-shared\
	 --with-python=$(PYTHON2) && $(MAKE) install
gc/gc_config.stamp:
	if [ ! -f gc/bdwgc/configure ];then cd gc/bdwgc && ./autogen.sh; fi
	if [ ! -f gc/libatomic_ops/configure ];then cd gc/libatomic_ops && autogen.sh;fi
	if [ ! -h gc/bdwgc/libatomic_ops ]; then ln -s $$PWD/gc/libatomic_ops\
	$$PWD/gc/bdwgc/libatomic_ops; fi
	touch gc/gc_config.stamp
gc: gc/gc_config.stamp gc/bdwgc/autogen.sh gc/libatomic_ops/autogen.sh
	cd gc/bdwgc && ./configure --with-threads=posix --disable-java-finalization \
	--enable-parallel-mark --enable-cplusplus --prefix=$$PWD/.. && $(MAKE) install
