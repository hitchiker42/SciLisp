# @file  Makefile
# @brief Makefile
#
# @author Mutsuo Saito (Hiroshima University)
# @author Makoto Matsumoto (Hiroshima University)
#
# Copyright (C) 2007, 2013 Mutsuo Saito, Makoto Matsumoto and Hiroshima
# University. All rights reserved.
#
# The new BSD License is applied to this software.
# see LICENSE.txt

WARN = -Wall -Wno-unused
OPTI = -O3 -finline-functions -fomit-frame-pointer -DNDEBUG \
-fno-strict-aliasing --param max-inline-insns-single=1800 -flto \
-fkeep-inline-functions
STD = -std=c99 -D_GNU_SOURCE
CC = gcc
CCFLAGS = $(OPTI) $(WARN) $(STD) -fPIC -rdynamic
SSE2FLAGS = -msse2 -DHAVE_SSE2
CFLAGS:= $(CFLAGS) $(CCFLAGS)
#STD_TARGET = test-std-M19937
#CCFLAGS += --param inline-unit-growth=500 \
#--param large-function-growth=900
all: SFMT.so
test: SFMT_test
SFMT_lib.o: SFMT_lib.c SFMT_lib.h SFMT-sse2.h
SFMT_r.o: SFMT_r.c SFMT_r.h SFMT_lib.h
SFMT_static.o: SFMT_static.c SFMT_lib.h SFMT_static.h
SFMT.so: SFMT_r.o SFMT_static.o SFMT_lib.o
	$(CC) $(CFLAGS) SFMT_r.o SFMT_static.o SFMT_lib.o -shared -o SFMT.so
SFMT.a: SFMT_r.o SFMT_static.o SFMT_lib.o
	ar rcs $@ SFMT_r.o SFMT_static.o SFMT_lib.o
SFMT_test: SFMT_test.c SFMT.so SFMT.h SFMT.a
	$(CC) $(CFLAGS) -o SFMT_test SFMT_test.c SFMT.a
std: $(STD_TARGET)
sse2:$(SSE2_TARGET)
std-check: $(ALL_STD_TARGET)
	./check.sh 32 test-std
sse2-check: $(ALL_SSE2_TARGET)
	./check.sh 32 test-sse2
test-std-M19937: test.c SFMT.c SFMT.h SFMT-params19937.h
	$(CC) $(CCFLAGS) -DSFMT_MEXP=19937 -o $@ test.c SFMT.c

test-alti-M19937: test.c SFMT.c SFMT.h SFMT-alti.h \
	SFMT-params19937.h
	$(CC) $(CCFLAGS) $(ALTIFLAGS) -DSFMT_MEXP=19937 -o $@ test.c SFMT.c

test-sse2-M19937: test.c SFMT.c SFMT.h SFMT-sse2.h \
	SFMT-params19937.h
	$(CC) $(CCFLAGS) $(SSE2FLAGS) -DSFMT_MEXP=19937 -o $@ test.c SFMT.c
clean:
	rm -f *.o *~ SFMT.so
