/*#define __ASSEMBLER__*/
/*#include "x86_asm.h"*/
#define RETVAL	-8
#define SAVE0   -16
#define SAVE1	-24
#define SAVE2	-32
#define SAVE3	-40
#define L(name)	.L##name
#define l2_cache_size 1024*1024
#define l2_cache_size_half 1024*1024/2

.macro save_registers
        movq	%r14, SAVE0(%rsp)
        .cfi_rel_offset %r14, SAVE0
        movq    %r13, SAVE1(%rsp)
        .cfi_rel_offset %r13, SAVE1
        movq	%r12, SAVE2(%rsp)
        .cfi_rel_offset %r12, SAVE2
        movq	%rbx, SAVE3(%rsp)
        .cfi_rel_offset %rbx, SAVE3
.endm
.macro restore_registers
        movq	SAVE3(%rsp), %rbx
        .cfi_restore %rbx
        movq	SAVE2(%rsp), %r12
        .cfi_restore %r12
        movq	SAVE1(%rsp), %r13
        .cfi_restore %r13
        movq	SAVE0(%rsp), %r14
        .cfi_restore %r14
.endm
.macro save_rbx
        movq  %rbx, SAVE0(%rsp)
        .cfi_rel_offset %rbx, $SAVE0
.endm
.macro restore_rbx
        movq  SAVE0(%rsp),%rbx
        .cfi_restore %rbx
.endm
.macro CALL_MCOUNT
        pushq %rbp;
        .cfi_adjust_cfa_offset 8;
        movq %rsp, %rbp;
        .cfi_def_cfa_register %rbp;
.endm
.macro ENTRY name
        .globl \name;
        .type \name,@function;
        .p2align 4
\name\():
        .cfi_startproc
.endm

.macro END name
.cfi_endproc
.size \name, .-\name
.endm
/* uint64_t *memcpy_stride
        (uint64_t *dest,const uint64_t *src,size_t size,size_t stride)

   copy size 8 byte blocks from src to dest, memory from src is obatined
   at 8*stride byte intervals and stored in dest contiguously.

   if stride is 2 a much more optimized routine is used than if stride is
   some other number

   This is mostly copied from the x86_64 memcpy.S in libc so it should be
   nearly as fast as memcpy (at least for stride 2). For stride two
   register + displacement adressing is used to load memory from src
   since we know the displacement increases by 16 each time. For an unknown
   stride we use register+(index*scale) adressing with index being incremented
   by stride after each memory access
*/

ENTRY memcpy_stride

/* memcpy handles blocks >32B specially, we don't*/
/*rdi=dest,rsi=src,rdx==size,rcx=stride*/
/*temp registers rax,r8-r11, maybe rcx*/

L(1):
/*if stride = 2 then jmp to optimized vesion */
        movq %rdi,RETVAL(%rsp)
        cmpq $2, %rcx
        je L(stride2)
L(strideN):
        xorq %rax,%rax
/*copy up to 3 blocks to make size a multiple of 4*/
L(1a):
        testq $1, %rdx
        jz L(1b)

        movq (%rsi,%rax,8),%r8
        addq %rcx,%rax
        movq %r8,0(%rdi)

        addq $8,%rdi
        decq %rdx
L(1b):
        testq $2, %rdx
        jz L(1c)

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
        shrq $2,%rdx
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
        movq %r11,24(%rdi)

        leaq 32(%rdi),%rdi

        decq %rdx
        jz L(1exit)

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
        movq %r11,24(%rdi)

        leaq 32(%rdi),%rdi

        decq %rdx
        jnz L(1loop)
L(1exit):
        restore_rbx
        movq RETVAL(%rsp),%rax
        ret
        .p2align 4
L(stride2):
/* deal with excess bytes, so we can copy 64 byte chunks in the main loop*/
L(2a):
        testq $1, %rdx
        jz L(2b)

        movq 0(%rsi),%r8
        movq %r8,0(%rdi)

        addq $16,%rsi
        addq $8,%rdi
        decq %rdx
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
/*zero out the last 2 bits from the size parameter (we just dealt with them)*/
        andq   $-4,%rdx
        jz L(2exit)

/*handle blocks up to 1KB (copy 64 bytes a loop, no need to save/restore
registers */
        cmpq $1024,%rdx
        ja L(stride2Big)

        shrq $2, %rcx
L(2loop):
        movq 0(%rsi),%r8
        movq 16(%rsi),%r9
        movq 32(%rsi),%r10
        movq 48(%rsi),%r11

        movq %r8,0(%rdi)
        movq %r9,8(%rdi)
        movq %r10,16(%rdi)
        movq %r11,24(%rdi)

        leaq 64(%rsi),%rsi
        leaq 32(%rdi),%rdi

        decq %rdx
        jz L(2exit)

        movq 0(%rsi),%r8
        movq 16(%rsi),%r9
        movq 32(%rsi),%r10
        movq 48(%rsi),%r11

        movq %r8,0(%rdi)
        movq %r9,8(%rdi)
        movq %r10,16(%rdi)
        movq %r11,24(%rdi)

        leaq 64(%rsi),%rsi
        leaq 32(%rdi),%rdi

        decq %rdx
        jnz L(2loop)
L(2exit):
        movq RETVAL(%rsp),%rax
        retq

        .p2align 4
L(stride2Big):
        /*save rbx,r12,r13,r14 and r15 in the red zone*/

        save_registers
/*what memcpy does is test size against 1/2 the l1 cache size (approx 256k)
  and uses movs to do copying via fast string ops (which I can't do)
  if size is >1/2 l1 but <1/2 l2 it loops prefetching memory during each iteration
  if size is >1/2 l2 it uses nontemporal prefetching and moving to avoid poluting
  the cache

  essently if the array is over 1/2 the l2 cache size it avoids polluting the
  cache while still trynig to do some prefetching. otherwise it tries to get
  memory into the cache before it's copies        
*/
        
L(3a): /*make size a multiple of 8, it should already be a multiple of 4
        so just test if 4 is set or not*/
        testq $4,%rcx
        jz L(3b)

        movq 0(%rsi),%r8
        movq 16(%rsi),%r9
        movq 32(%rsi),%r10
        movq 48(%rsi),%r11

        movq %r8,0(%rdi)
        movq %r9,8(%rdi)
        movq %r10,16(%rdi)
        movq %r11,24(%rdi)

        leaq 64(%rsi),%rsi
        leaq 32(%rdi),%rdi

        subq $4,%rdx
L(3b):
        andq $-8,%rdx
        /*set rcx to rcx/8, then we can just do decq rcx once for every 64 bytes*/
        shrq $3,%rdx
L(3loop):

        movq 0(%rsi),%rax
        movq 16(%rsi),%rbx
        movq 32(%rsi),%r8
        movq 48(%rsi),%r9
        movq 64(%rsi),%r10
        movq 80(%rsi),%r11
        movq 96(%rsi),%r12
        movq 112(%rsi),%r13
/* I'm not sure why 896, but that's what memcpy uses
	prefetcht0	896 +  0(%rsi)
	prefetcht0	896 + 128(%rsi)
*/

        movq %rax,0(%rdi)
        movq %rbx,8(%rdi)
        movq %r8,16(%rdi)
        movq %r9,24(%rdi)
        movq %r10,32(%rdi)
        movq %r11,40(%rdi)
        movq %r12,48(%rdi)
        movq %r13,52(%rdi)


        leaq 128(%rsi),%rsi
        leaq 64(%rdi),%rdi

        decq %rcx
        jz L(3exit)

        movq 0(%rsi),%rax
        movq 16(%rsi),%rbx
        movq 32(%rsi),%r8
        movq 48(%rsi),%r9
        movq 64(%rsi),%r10
        movq 80(%rsi),%r11
        movq 96(%rsi),%r12
        movq 112(%rsi),%r13
/* I'm not sure why 896, but that's what memcpy uses
	prefetcht0	896 +  0(%rdi)
	prefetcht0	896 + 64(%rdi)
*/

        movq %rax,0(%rdi)
        movq %rbx,8(%rdi)
        movq %r8,16(%rdi)
        movq %r9,24(%rdi)
        movq %r10,32(%rdi)
        movq %r11,40(%rdi)
        movq %r12,48(%rdi)
        movq %r13,52(%rdi)

        leaq 128(%rsi),%rsi
        leaq 64(%rdi),%rdi

        decq %rcx
        jnz L(3loop)
L(3exit):

        restore_registers

        movq RETVAL(%rsp), %rax

        retq

        .p2align 4
/*for really big blocks (> 512k) avoid poluting the cache
L(NT):
        L(NT):					/* non-temporal 128-byte /
 HERE I need to put it code to make rdx a multiple of 16
        andq  $-128, %rdx
	shrq	 $7, %rdx

       .p2align 4

L(NTloop):
	prefetchnta	768(%rsi)
	prefetchnta	896(%rsi)

	decq	%rdx

	movq	  (%rsi), %rax
	movq	16(%rsi), %r8
	movq	32(%rsi), %r9
	movq	48(%rsi), %r10
	movq	64(%rsi), %r11
	movq	80(%rsi), %r12
	movq	96(%rsi), %r13
	movq    112(%rsi), %r14

	movntiq	%rax,   (%rdi)
	movntiq	 %r8,  8(%rdi)
	movntiq	 %r9, 16(%rdi)
	movntiq	%r10, 24(%rdi)
	movntiq	%r11, 32(%rdi)
	movntiq	%r12, 40(%rdi)
	movntiq	%r13, 48(%rdi)
	movntiq	%r14, 56(%rdi)
necessary to fit displacement in 8 bits
	leaq	128(%rsi), %rsi

	movq	  (%rsi), %rax
	movq	16(%rsi), %r8
	movq	32(%rsi), %r9
	movq	48(%rsi), %r10
	movq	64(%rsi), %r11
	movq	80(%rsi), %r12
	movq	96(%rsi), %r13
	movq    112(%rsi), %r14

	movntiq	%rax,  64(%rdi)
	movntiq	 %r8,  72(%rdi)
	movntiq	 %r9,  80(%rdi)
	movntiq	%r10,  88(%rdi)
	movntiq	%r11,  96(%rdi)
	movntiq	%r12, 104(%rdi)
	movntiq	%r13, 112(%rdi)
	movntiq	%r14, 120(%rdi)

	leaq	128(%rsi), %rsi
	leaq	128(%rdi), %rdi

	jnz	L(NTloop)

	sfence				/* serialize memory stores /

	movq	SAVE2(%rsp), %r12
	cfi_restore (%r12)
	movq	SAVE1(%rsp), %r13
	cfi_restore (%r13)
	movq	SAVE0(%rsp), %r14
	cfi_restore (%r14)

L(NTskip):
	andl	$127, %edx		/* check for left overs /
#ifdef USE_AS_MEMPCPY
	jnz	L(1)

	movq	%rdi, %rax
#else
	movq	RETVAL(%rsp), %rax
	jnz	L(1)

	rep
#endif
	retq				/* exit /
*/

END memcpy_stride
