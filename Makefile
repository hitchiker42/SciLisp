CC ?=gcc 
ifeq ($(CC),clang)
OPT_FLAG:=-O1
else
OPT_FLAG:=-Og
endif
XCFLAGS:=-g $(OPT_FLAG) -std=gnu99 -D_GNU_SOURCE -lgc
LEX:=flex
YACC:=bison
.PHONY: clean all
all: test_interpreter
test_interpreter: scanner_test.c lex.yy.c common.h prim.h
	$(CC) $(XCFLAGS) -lreadline $^ -o $@
lex.yy.c: lisp.lex lisp.tab.h
	$(LEX) lisp.lex
lisp.tab.h: lisp.y
	$(YACC) -d lisp.y
