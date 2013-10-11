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
OPT_FLAGS:=$(OPT_FLAGS) -ggdb
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING -DNDEBUG
# -Wsuggest-attribute=pure|const|noreturn maybe add this warning for looking for optimizations
WARNING_FLAGS:=$(WARNING_FLAGS) -Wparentheses -Wsequence-point -Warray-bounds -Wenum-compare -Wmissing-field-initializers -Wimplicit
COMMON_CFLAGS=-std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums -flto
XLDFLAGS:=-lgc -lm -lreadline -lcord -rdynamic
XCFLAGS=$(WARNING_FLAGS) $(XLDFLAGS) $(COMMON_CFLAGS)
XCFLAGS_NOWARN=-g $(OPT_FLAGS) -std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums -flto $(XLDFLAGS)
LEX:=flex
SCILISP_HEADERS:=common.h prim.h types.h cons.h lex.yy.h print.h array.h
COMMON_HEADERS:=common.h debug.h types.h
FRONTEND_SRC:=lex.yy.c parser.c cons.c print.c frontend.c env.c #array.c
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o env.o #array.o
BACKEND_SRC:=eval.c codegen.c
BACKEND:=eval.o codegen.o
ASM_FILES :=$(ASM_FILES) eval.c
CFLAGS:=$(CFLAGS) $(XCFLAGS) $(OPT_FLAGS)
.PHONY: clean all quiet asm optimized set_quiet set_optimized
LLVM_FLAGS:=$(shell llvm-config --ldflags --cxxflags --libs core engine)$(OPT_FLAG)
CXXFLAGS:=$(CXXFLAGS)$(shell llvm-config --cppflags) -lto -ggdb
.PHONY: clean all quiet asm
all: SciLisp
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(CFLAGS) $(XCFLAGS) $(FRONTEND) $(BACKEND) -fwhole-program -o $@
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
llvm_codegen.o:codegen.h $(COMMON_HEADERS) prim.h llvm_codegen.cpp cons.h
env.o: env.c $(COMMON_HEADERS)
#make object file of primitives, no debugging, and optimize
prim.o: prim.c
	gcc -o prim.o -c -std=gnu99 -O3 $^
#array.o: array.c $(COMMON_HEADERS)
clean:
	rm *.o
asm: $(ASM_FILES)
	mkdir -p SciLisp_asm
	(cd SciLisp_asm; \
	for i in $(ASM_FILES);do \
	$(CC) -std=gnu99 $(OPT_FLAGS) -fverbose-asm ../$$i -S;done)
llvm_ir: $(ASM_FILES)
	$(eval CC := clang -emit-llvm)
	$(eval OPT_FLAG := -O2)
