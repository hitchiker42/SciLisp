#!/bin/bash
if [ ! -L src/gc/bdwgc/libatomic_ops ]; then
    ln -s `pwd`/src/gc/libatomic_ops `pwd`/src/gc/bdwgc/libatomic_ops
fi
autoreconf -i
