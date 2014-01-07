#!/bin/bash
autoreconf -i
if [ ! -L src/gc/bdwgc/libatomic_ops ]; then
    ln -s `pwd`/src/gc/libatomic_ops `pwd`/gc/bdwgc/libatomic_ops
fi
