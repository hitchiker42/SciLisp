CC=gcc $(CFLAGS)
ifeq ($(CC),clang)
OPT_FLAG:=-O1
else
OPT_FLAG:=-Og
endif
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING
# -Wsuggest-attribute=pure  maybe add this warning for looking for optimizations
WARNING_FLAGS=-Wparentheses -Wsequence-point -Warray-bounds -Wenum-compare\
-Wmissing-field-initializers 
XCFLAGS=-ggdb $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums -flto $(WARNING_FLAGS) $(XLDFLAGS)
XCFLAGS_NOWARN=-g $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums -flto $(XLDFLAGS)
XLDFLAGS:=-lgc -lm -lreadline -lcord -rdynamic
LEX:=flex
YACC:=bison
SCILISP_HEADERS:=common.h prim.h types.h cons.h lex.yy.h
COMMON_HEADERS:=common.h debug.h types.h
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o env.o
BACKEND:=eval.o codegen.o
.PHONY: clean all quiet
all: SciLisp
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(XCFLAGS) $(FRONTEND) $(BACKEND) -o $@
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS_NOWARN) -c lex.yy.c -o lex.yy.o
quiet: 
	CFLAGS="$(CFLAGS)$(QUIET_FLAGS)" $(MAKE) all
force:
	$(MAKE) -B all
optimized:
	CFLAGS="$(CFLAGS) -O3 -march=native -flto" $(MAKE) -B all
parser.o: parser.c $(COMMON_HEADERS) cons.h
	$(CC) $(XCFLAGS) -c parser.c -o parser.o
cons.o: cons.c $(COMMON_HEADERS) cons.h
	$(CC) $(XCFLAGS) -c cons.c -o cons.o
print.o: print.c $(COMMON_HEADERS) cons.h
	$(CC) $(XCFLAGS) -c print.c -o print.o
frontend.o: frontend.c $(COMMON_HEADERS) prim.h
	$(CC) $(XCFLAGS) -c frontend.c -o frontend.o
eval.o: eval.c $(COMMON_HEADERS) cons.h
	$(CC) $(XCFLAGS) -c eval.c -o eval.o
codegen.o: codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
	$(CC) $(XCFLAGS) -c c_codegen.c -o codegen.o
# or $(CXX) $(XCFLAGS) $(LLVM_FLAGS) c_codegen.o llvm_codegen.o -o codegen.o
c_codegen.o:codegen.h $(COMMON_HEADERS) prim.h c_codegen.c cons.h
	$(CC) $(XCFLAGS) -c c_codegen.c -o c_codegen.o
llvm_codegen.o:codegen.h $(COMMON_HEADERS) prim.h llvm_codegen.c cons.h
	$(CC) $(XCFLAGS) -c llvm_codegen.c -o llvm_codegen.o
env.o: env.c $(COMMON_HEADERS)
	$(CC) $(XCFLAGS) -c env.c -o env.o
clean:
	rm *.o
