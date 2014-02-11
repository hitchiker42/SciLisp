#include "x86_asm.h"

#define RETVAL	(-8)
#define SAVE0	(RETVAL - 8)
#define SAVE1	(SAVE0	- 8)
#define SAVE2	(SAVE1	- 8)
#define SAVE3	(SAVE2	- 8)

#define save_rbx()                      \
        movq	%rbx, SAVE0(%rsp)       \
        cfi_rel_offset (%rbx, SAVE0)
#define restore_rbx()
        movq	SAVE0(%rsp), %rbx       \
        cfi_restore (%rbx)
#define save_registers()                \
        movq	%r14, SAVE0(%rsp)       \
        cfi_rel_offset (%r14, SAVE0)    \
        movq	%r13, SAVE1(%rsp)       \
        cfi_rel_offset (%r13, SAVE1)    \
        movq	%r12, SAVE2(%rsp)       \
        cfi_rel_offset (%r12, SAVE2)    \
        movq	%rbx, SAVE3(%rsp)       \
        cfi_rel_offset (%rbx, SAVE3)
#define restore_registers()             \
        movq	SAVE3(%rsp), %rbx       \
        cfi_restore (%rbx)              \
        movq	SAVE2(%rsp), %r12       \
        cfi_restore (%r12)              \
        movq	SAVE1(%rsp), %r13       \
        cfi_restore (%r13)              \
        movq	SAVE0(%rsp), %r14       \
        cfi_restore (%r14)

ENTRY(memcpy_stride) /*uint64_t*(uint64_t*,uint64_t*,size_t,size_t)*/
/* memcpy handles blocks >32B specially, we don't*/
/*rdi=dest,rsi=src,rdx==size,rcx=stride*/
/*temp registers rax,r8-r11, maybe rcx*/

L(1):
        testq $2, %rcx
        movq %rdi,RETVAL(%rsp)
        je L(stride2)
L(strideN):
        xorq %rax,%rax
L(1a):
        testq $1, %rdx
        jz L(2b)

        movq (%rsi,%rax,8),%r8
        addq %rcx,%rax
        movq %r8,0(%rdi)

        addq $8,%rdi
        decl %rdx
L(1b):
        testq $2, %rdx
        jz L(2c)

        movq (%rsi,%rax,8),%r8
        addq %rcx,%rax
        movq (%rsi,%rax,8),%r9
        addq %rcx,%rax
        movq %r8,0(%rdi)
        movq %r9,8(%rdi)

        addq $16,%rdi
        subq $2,%rdx
L(1c):
        andq   $-4,%rdx
        jz L(1exit)
L(1loop):
        movq (%rsi,%rax,8),%r8
        addq %rcx,%rax
        movq (%rsi,%rax,8),%r9
        addq %rcx,%rax
        movq (%rsi,%rax,8),%r10
        addq %rcx,%rax
        movq (%rsi,%rax,8),%r11
        addq %rcx,%rax

        movq %r8,0(%rdi)
        movq %r9,8(%rdi)
        movq %r10,16(%rdi)
        movq %r11,32(%rdi)

        leaq $40(%rdi),%rdi
        subq $4,$rdx
        jnz L(1loop)
L(1exit):
        restore_rbx()
        ret
L(stride2):
L(2a):
        testq $1, %rdx
        jz L(2b)

        movq 0(%rsi),%r8
        movq %r8,0(%rdi)

        addq $16,%rsi
        addq $8,%rdi
        decl %rdx
L(2b):
        testq $2, %rdx
        jz L(2c)

        movq 0(%rsi),%r8
        movq 16(%rsi),%r9
        movq %r8,0(%rdi)
        movq %r9,8(%rdi)

        addq $32,%rsi
        addq $16,%rdi
        subq $2,%rdx
L(2c):
        andq   $-4,%rdx
        jz L(2exit)
L(2loop):
        movq 0(%rsi),%r8
        movq 16(%rsi),%r9
        movq 32(%rsi),%r10
        movq 64(%rsi),%r11

        movq %r8,0(%rdi)
        movq %r9,8(%rdi)
        movq %r10,16(%rdi)
        movq %r11,32(%rdi)

        leaq $80(%rsi),%rsi
        leaq $40(%rdi),%rdi 
        
        subq $4,$rdx
        jnz L(2loop)
L(2exit):
        movq RETVAL(%rsp),%rax
        retq

        .p2align 4

/*
save and restore callee saved registers in the red zone
(the 128 bits between (%rsp) and -128(%rsp) that's reserved
   as stack space)

#define RETVAL	(0)
#define SAVE0	(RETVAL - 8)
#define SAVE1	(SAVE0	- 8)
#define SAVE2	(SAVE1	- 8)
#define SAVE3	(SAVE2	- 8)

        movq	%r14, SAVE0(%rsp)
        cfi_rel_offset (%r14, SAVE0)
        movq	%r13, SAVE1(%rsp)
        cfi_rel_offset (%r13, SAVE1)
        movq	%r12, SAVE2(%rsp)
        cfi_rel_offset (%r12, SAVE2)
        movq	%rbx, SAVE3(%rsp)
        cfi_rel_offset (%rbx, SAVE3)

        movq	SAVE3(%rsp), %rbx
        cfi_restore (%rbx)
        movq	SAVE2(%rsp), %r12
        cfi_restore (%r12)
        movq	SAVE1(%rsp), %r13
        cfi_restore (%r13)
        movq	SAVE0(%rsp), %r14
        cfi_restore (%r14)

*/
