CC=gcc $(CFLAGS)
ifeq ($(CC),clang)
OPT_FLAG:=-O1
else
OPT_FLAG:=-Og
endif
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING
XCFLAGS=-g $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -foptimize-sibling-calls -fshort-enums -flto $(XLDFLAGS)
XLDFLAGS:=-lgc -lm -lreadline -lcord
LEX:=flex
YACC:=bison
SCILISP_HEADERS:=common.h prim.h types.h cons.h lex.yy.h
FRONTEND:=lex.yy.o parser.o cons.o print.o frontend.o
BACKEND:=eval.o codegen.o
.PHONY: clean all quiet
all: SciLisp
SciLisp: $(FRONTEND) $(BACKEND) $(SCILISP_HEADERS)
	$(CC) $(XCFLAGS) $(FRONTEND) $(BACKEND) -o $@
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lex.yy.o: lex.yy.c
	$(CC) $(XCFLAGS) -c lex.yy.c -o lex.yy.o
quiet: 
	CFLAGS="$(CFLAGS)$(QUIET_FLAGS)" $(MAKE) all
force:
	$(MAKE) -B all
optimized:
	CFLAGS="$(CFLAGS) -O3 -march=native -flto" $(MAKE) -B all
parser.o:
	$(CC) $(XCFLAGS) -c parser.c -o parser.o
cons.o:
	$(CC) $(XCFLAGS) -c cons.c -o cons.o
print.o:
	$(CC) $(XCFLAGS) -c print.c -o print.o
frontend.o:
	$(CC) $(XCFLAGS) -c frontend.c -o frontend.o
eval.o:
	$(CC) $(XCFLAGS) -c eval.c -o eval.o
codegen.o:
	$(CC) $(XCFLAGS) -c codegen.c -o codegen.o
clean:
	rm *.o
