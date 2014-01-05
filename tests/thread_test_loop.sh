#!/bin/bash
: #clear $?
#if we get SIGALRM that's good, it means we didn't fail, so exit with 0
trap 'exit 0' SIGALRM
while( [ $? -eq 0 ]);do
    SciLisp -t test.lisp
done
#if we got here SciLisp failed, so exit with whatever exit code we got
exit $?
