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
CCFLAGS = $(OPTI) $(WARN) $(STD) -fPIC -rdynamic -msse2 -DHAVE_SSE2
SSE2FLAGS = -msse2 -DHAVE_SSE2
AM_CFLAGS = $(CFLAGS) $(CCFLAGS)
#STD_TARGET = test-std-M19937
#CCFLAGS += --param inline-unit-growth=500 \
#--param large-function-growth=900
noinst_LTLIBRARIES = libSFMT.la
libSFMT_la_SOURCES = SFMT.c SFMT_r.h SFMT_lib.h SFMT_lib.c
