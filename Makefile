CC=gcc 
ifeq ($(CC),clang)
OPT_FLAG:=-O1
else
OPT_FLAG:=-Og
endif
XCFLAGS:=-g $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -lgc -lm
LEX:=flex
YACC:=bison
FRONTEND:=frontend.c lex.yy.c common.h prim.h parser.c cons.c
BACKEND:=eval.c codegen.c
.PHONY: clean all
all: test_interpreter
test_interpreter: $(FRONTEND) $(BACKEND)
	$(CC) $(XCFLAGS) -lreadline $^ -o $@
lex.yy.c: lisp.lex common.h
	$(LEX) lisp.lex
lisp.tab.h: lisp.y cons.c
	$(YACC) -d lisp.y
lisp.tab.c: lisp.tab.h 
