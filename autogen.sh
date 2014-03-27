#!/bin/bash
CC='ccache gcc' ./bootstrap.sh && ./configure --prefix=$HOME/usr "$@"
