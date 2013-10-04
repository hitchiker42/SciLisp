CC=gcc $(CFLAGS)
ifeq ($(CC),clang)
OPT_FLAG:=-O1
else
OPT_FLAG:=-Og
endif
QUIET_FLAGS:=-DHERE_OFF -DQUIET_LEXING
XCFLAGS:=-g $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -lgc -lm -lcord -foptimize-sibling-calls
LEX:=flex
YACC:=bison
FRONTEND:=frontend.c lex.yy.c common.h prim.h parser.c cons.c print.c\
prim.h types.h cons.h
BACKEND:=eval.c codegen.c
.PHONY: clean all quiet
all: test_interpreter
test_interpreter: $(FRONTEND) $(BACKEND)
	$(CC) $(XCFLAGS) -lreadline $^ -o $@
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lisp.tab.h: lisp.y cons.c
	$(YACC) -d lisp.y
lisp.tab.c: lisp.tab.h 
quiet: 
	CFLAGS="$(CFLAGS)$(QUIET_FLAGS)" $(MAKE) all
force:
	$(MAKE) -B all
