#!/bin/bash
SCRIPT_PATH=$(cd $(dirname $0);pwd)
if [ ! -L $SCRIPT_PATH/src/gc/bdwgc/libatomic_ops ]; then
    ln -s $SCRIPT_PATH/src/gc/libatomic_ops $SCRIPT_PATH/src/gc/bdwgc/libatomic_ops
fi
pushd $SCRIPT_PATH
autoreconf -iv &&
CC='ccache gcc' $SCRIPT_PATH/configure --prefix=$HOME/usr "$@"
popd 
