SHELL:=/bin/bash
ifeq ($(CC),clang)
OPT_FLAGS:=-O2
WARNING_FLAGS:=-w
else ifeq ($(CXX),clang++)
OPT_FLAGS:=-O1
WARNING_FLAGS:=-w
else
CC:=gcc
CXX:=g++
OPT_FLAGS:=-Og
endif
CC:=$(CC) -fPIC
OPT_FLAGS:=$(OPT_FLAGS) -ggdb
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING -DNDEBUG
# -Wsuggest-attribute=pure|const|noreturn maybe add this warning for looking for optimizations
WARNING_FLAGS:=$(WARNING_FLAGS) -Wparentheses -Wsequence-point -Warray-bounds -Wenum-compare -Wmissing-field-initializers -Wimplicit
COMMON_CFLAGS=-std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums\
	 -flto -rdynamic -fno-strict-aliasing -I$(shell pwd)/gc/include/gc\
	 -I$(shell pwd)/llvm/llvm/include
COMMON_CXXFLAGS:= -D_GNU_SOURCE -fno-strict-aliasing -fno-strict-enums -Wno-write-strings -I$(shell pwd)/gc/include/gc 
#pretty sure I violate strict-aliasing, even if not, better safe than sorry
XLDFLAGS:=-Wl,-rpath=$(shell pwd)/gc/lib  -lgc -lm -lreadline -lcord -rdynamic\
	 -Wl,-rpath=$(shell pwd)/llvm/llvm/lib
XCFLAGS=$(WARNING_FLAGS) $(XLDFLAGS) $(COMMON_CFLAGS)
XCFLAGS_NOWARN=-g $(OPT_FLAGS) $(COMMON_CFLAGS) $(XLDFLAGS)
LEX:=flex
SCILISP_HEADERS:=common.h prim.h types.h cons.h lex.yy.h print.h array.h
COMMON_HEADERS:=common.h debug.h types.h
FRONTEND_SRC:=lex.yy.c parser.c cons.c print.c frontend.c env.c array.c
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o env.o array.o
BACKEND_SRC:=eval.c codegen.c
BACKEND:=eval.o codegen.o prim.o
ASM_FILES :=$(ASM_FILES) eval.c
CFLAGS:=$(CFLAGS) $(XCFLAGS) $(OPT_FLAGS)
.phony: clean all quiet asm optimized set_quiet set_optimized\
	doc info pdf clean_doc libprim_reqs
llvm_config:=$(shell pwd)/llvm/release+asserts/bin/llvm-config
LLVM_FLAGS:=`$(LLVM_CONFIG) --ldflags --cxxflags --libs core engine` $(OPT_FLAG)
CXXFLAGS:=$(CXXFLAGS) $(shell llvm-config --cppflags) -flto -ggdb 
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(CFLAGS) $(XCFLAGS) $(FRONTEND) $(BACKEND) -o $@
SciLisp_llvm: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS) llvm_codegen.o
	$(CXX) $(CXXFLAGS) $(LLVM_FLAGS) $(FRONTEND) $(BACKEND) llvm_codegen.o -o $@
all: SciLisp llvm_test.o
llvm_test.o: llvm_codegen.c libSciLisp.so prim.bc
	$(CC) -o llvm_temp.o $(COMMON_CFLAGS) \
	`$(LLVM_CONFIG) --cppflags --cflags` -D_LLVM_TEST_ llvm_codegen.c -c -O2
	$(CXX) -flto llvm_temp.o -ggdb\
	 `$(LLVM_CONFIG) --cflags --ldflags --libs all` \
	 -lcord -lgc -lm -o llvm-test.o $(COMMON_CFLAGS) -L$(shell pwd) \
	 libSciLisp.so -Wl,-rpath=$(shell pwd) -ggdb
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS_NOWARN) -w -c lex.yy.c -o lex.yy.o
set_quiet:
	$(eval CFLAGS=$(CFLAGS) $(QUIET_FLAGS) )
quiet:set_quiet all
force:
	$(MAKE) -B all
set_optimized:
	$(eval OPT_FLAGS:=-O3 -march=native)
	$(eval CFLAGS:=$(COMMON_CFLAGS) $(QUIET_FLAGS) $(OPT_FLAGS))
optimized:set_optimized all
parser.o: parser.c $(COMMON_HEADERS) cons.h
cons.o: cons.c $(COMMON_HEADERS) cons.h
print.o: print.c $(COMMON_HEADERS) cons.h
frontend.o: frontend.c $(COMMON_HEADERS) prim.h
eval.o: eval.c $(COMMON_HEADERS) cons.h array.h
codegen.o: codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
	$(CC) $(XCFLAGS) -c c_codegen.c -o codegen.o
# or $(CXX) $(XCFLAGS) $(LLVM_FLAGS) c_codegen.o llvm_codegen.o -o codegen.o
c_codegen.o:codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
llvm_codegen.o:codegen.h $(COMMON_HEADERS) prim.h llvm_codegen.c cons.h
	$(CC) `llvm-config --cppflags` $(CFLAGS) -c llvm_codegen.c -o llvm_codegen.o
env.o: env.c $(COMMON_HEADERS)
array.o: array.c $(COMMON_HEADERS) array.h
prim.o: prim.c $(COMMOM_HEADERS) array.h cons.h
#make object file of primitives, no debugging, and optimize
LIBPRIM_FLAGS:=$(COMMON_CFLAGS) -O3
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
libprim.o: libprim_reqs
	$(start_libprim)
	$(CC) -o prim.o $(LIBPRIM_FLAGS) -lm -lgc -lcord	
libSciLisp.a: libprim_reqs
	$(start_libprim)
	ar rcs $@ $(libprim_files)
	rm $(libprim_files)
LD_SHARED_FLAGS:= -Wl,-R$(shell pwd) -Wl,-shared -Wl,-soname=libSciLisp.so
libSciLisp.so: libprim_reqs
	$(start_libprim)
	$(CC) $(XCFLAGS) -shared -lcord -lm -lgc $(LD_SHARED_FLAGS) $(libprim_files) -o $@
prim.bc: prim.c eval.c print.c env.c cons.c array.c
	$(eval CC:=clang $(QUIET_FLAGS) $(LIBPRIM_FLAGS))
	$(CC) -S -emit-llvm -fno-asm $^;\
	llvm-link prim.s cons.s eval.s array.s env.s print.s -o prim.bc;\
	rm prim.s cons.s eval.s array.s env.s print.s
	llvm-dis prim.bc -o prim.ll
clean:
	rm *.o
	rm prim.bc;rm prim.ll;rm libSciLisp.so
asm: $(ASM_FILES)
	mkdir -p SciLisp_asm
	(cd SciLisp_asm; \
	for i in $(ASM_FILES);do \
	$(CC) -std=gnu99 $(OPT_FLAGS) -fverbose-asm ../$$i -S;done)
doc: info pdf
info: doc/manual.texi
	(cd doc && makeinfo manual.texi)
pdf: doc/manual.texi
	(cd doc && makeinfo --pdf manual.texi)
clean_doc:
	cd doc && rm $$(find '!' -name "*.texi" -type f)
