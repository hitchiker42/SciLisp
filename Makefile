ifeq ($(CC),clang)
OPT_FLAG:=-O2
WARNING_FLAGS:=-w
else ifeq ($(CXX),clang++)
OPT_FLAG:=-O1
WARNING_FLAGS:=-w
else
CC:=gcc
CXX:=g++
OPT_FLAG:=-Og
endif
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING
# -Wsuggest-attribute=pure|const|noreturn maybe add this warning for looking for optimizations
WARNING_FLAGS:=$(WARNING_FLAGS) -Wparentheses -Wsequence-point -Warray-bounds -Wenum-compare -Wmissing-field-initializers -Wimplicit
COMMON_CFLAGS=-ggdb $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums -flto 
XLDFLAGS:=-lgc -lm -lreadline -lcord -rdynamic
XCFLAGS:=$(WARNING_FLAGS) $(XLDFLAGS) $(COMMON_CFLAGS) 
XCFLAGS_NOWARN=-g $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums -flto $(XLDFLAGS)
LEX:=flex
SCILISP_HEADERS:=common.h prim.h types.h cons.h lex.yy.h print.h
COMMON_HEADERS:=common.h debug.h types.h
FRONTEND_SRC:=lex.yy.o parser.o cons.o print.o frontend.o env.o
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o env.o
BACKEND_SRC:=eval.o codegen.o
BACKEND:=eval.o codegen.o
ASM_FILES :=$(ASM_FILES) eval.c
CFLAGS:=$(CFLAGS) $(XCFLAGS)
LLVM_FLAGS:=$(shell llvm-config --ldflags --cxxflags --libs core engine)$(OPT_FLAG)
.PHONY: clean all quiet asm
all: SciLisp
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(XCFLAGS) $(FRONTEND) $(BACKEND) -fwhole-program -o $@
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS_NOWARN) -w -c lex.yy.c -o lex.yy.o
quiet: 
	CFLAGS="$(CFLAGS)$(QUIET_FLAGS)" $(MAKE) all
force:
	$(MAKE) -B all
optimized:
	CFLAGS="$(CFLAGS) -O3 -march=native" $(MAKE) -B all
parser.o: parser.c $(COMMON_HEADERS) cons.h
cons.o: cons.c $(COMMON_HEADERS) cons.h
print.o: print.c $(COMMON_HEADERS) cons.h
frontend.o: frontend.c $(COMMON_HEADERS) prim.h
eval.o: eval.c $(COMMON_HEADERS) cons.h
codegen.o: codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
	$(CC) $(XCFLAGS) -c c_codegen.c -o codegen.o
# or $(CXX) $(XCFLAGS) $(LLVM_FLAGS) c_codegen.o llvm_codegen.o -o codegen.o
c_codegen.o:codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
llvm_codegen.o:codegen.h $(COMMON_HEADERS) prim.h llvm_codegen.c cons.h
env.o: env.c $(COMMON_HEADERS)
clean:
	rm *.o
asm: $(ASM_FILES)
	mkdir -p SciLisp_asm
	(cd SciLisp_asm; \
	for i in $(ASM_FILES);do \
	$(CC) -std=gnu99 $(OPT_FLAG) -fverbose-asm ../$$i -S;done)
llvm_ir: $(ASM_FILES)
	$(eval $CC := clang -emit-llvm)
	$(eval $OPT_FLAG := -O2)
