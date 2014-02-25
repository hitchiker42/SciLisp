/* ABI Info
register|         usage           | Preserved across function calls
----------------------------------------------------------------------
   rax:  1st return register,     |no
   rbx:  callee saved register    |yes
   rcx:  4th arg register         |no
   rdx:  3rd arg register,        |no
         2nd return register
   rsp: stack pointer             |yes         
   rbp: frame pointer (optional)  |yes
   rsi: 2nd arg register          |no
   rdi: 1st arg register          |no
   r8:  5th arg register          |no
   r9:  6th arg register          |no
   r10,r11: temp registers        |no
   r12-r15: callee saved registers|yes
 */
/* Assembler macros for x86-64. (also included is the generic sysdeps.h header)
   Copyright (C) 2001-2014 Free Software Foundation, Inc.
   This file is part of the GNU C Library.

   The GNU C Library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2.1 of the License, or (at your option) any later version.

   The GNU C Library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with the GNU C Library; if not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _X86_64_SYSDEP_H
#define _X86_64_SYSDEP_H 1
#ifndef C_LABEL

/* Define a macro we can use to construct the asm name for a C symbol.  */
#define C_LABEL(name)	name##:

#endif

#ifdef __ASSEMBLER__
/* Mark the end of function named SYM.  This is used on some platforms
   to generate correct debugging information.  */
#ifndef END
#define END(sym)
#endif

#ifndef JUMPTARGET
#define JUMPTARGET(sym)	sym
#endif
#endif /* __ASSEMBLER__ */

/* Macros to generate eh_frame unwind information.  */
#ifdef __ASSEMBLER__
#define cfi_startproc			.cfi_startproc
#define cfi_endproc			.cfi_endproc
#define cfi_def_cfa(reg, off)		.cfi_def_cfa reg, off
#define cfi_def_cfa_register(reg)	.cfi_def_cfa_register reg
#define cfi_def_cfa_offset(off)	.cfi_def_cfa_offset off
#define cfi_adjust_cfa_offset(off)	.cfi_adjust_cfa_offset off
#define cfi_offset(reg, off)		.cfi_offset reg, off
#define cfi_rel_offset(reg, off)	.cfi_rel_offset reg, off
#define cfi_register(r1, r2)		.cfi_register r1, r2
#define cfi_return_column(reg)	.cfi_return_column reg
#define cfi_restore(reg)		.cfi_restore reg
#define cfi_same_value(reg)		.cfi_same_value reg
#define cfi_undefined(reg)		.cfi_undefined reg
#define cfi_remember_state		.cfi_remember_state
#define cfi_restore_state		.cfi_restore_state
#define cfi_window_save		.cfi_window_save
#define cfi_personality(enc, exp)	.cfi_personality enc, exp
#define cfi_lsda(enc, exp)		.cfi_lsda enc, exp

#else /* ! __ASSEMBLER__ */

#define CFI_STRINGIFY(Name) CFI_STRINGIFY2 (Name)
#define CFI_STRINGIFY2(Name) #Name
#define CFI_STARTPROC	".cfi_startproc"
#define CFI_ENDPROC	".cfi_endproc"
#define CFI_DEF_CFA(reg, off)	\
   ".cfi_def_cfa " CFI_STRINGIFY(reg) "," CFI_STRINGIFY(off)
#define CFI_DEF_CFA_REGISTER(reg) \
   ".cfi_def_cfa_register " CFI_STRINGIFY(reg)
#define CFI_DEF_CFA_OFFSET(off) \
   ".cfi_def_cfa_offset " CFI_STRINGIFY(off)
#define CFI_ADJUST_CFA_OFFSET(off) \
   ".cfi_adjust_cfa_offset " CFI_STRINGIFY(off)
#define CFI_OFFSET(reg, off) \
   ".cfi_offset " CFI_STRINGIFY(reg) "," CFI_STRINGIFY(off)
#define CFI_REL_OFFSET(reg, off) \
   ".cfi_rel_offset " CFI_STRINGIFY(reg) "," CFI_STRINGIFY(off)
#define CFI_REGISTER(r1, r2) \
   ".cfi_register " CFI_STRINGIFY(r1) "," CFI_STRINGIFY(r2)
#define CFI_RETURN_COLUMN(reg) \
   ".cfi_return_column " CFI_STRINGIFY(reg)
#define CFI_RESTORE(reg) \
   ".cfi_restore " CFI_STRINGIFY(reg)
#define CFI_UNDEFINED(reg) \
   ".cfi_undefined " CFI_STRINGIFY(reg)
#define CFI_REMEMBER_STATE \
   ".cfi_remember_state"
#define CFI_RESTORE_STATE \
   ".cfi_restore_state"
#define CFI_WINDOW_SAVE \
   ".cfi_window_save"
#define CFI_PERSONALITY(enc, exp) \
   ".cfi_personality " CFI_STRINGIFY(enc) "," CFI_STRINGIFY(exp)
#define CFI_LSDA(enc, exp) \
   ".cfi_lsda " CFI_STRINGIFY(enc) "," CFI_STRINGIFY(exp)
#endif /* __ASSEMBLER__ */


#ifdef	__ASSEMBLER__

/* Syntactic details of assembler.  */

/* ELF uses byte-counts for .align, most others use log2 of count of bytes.  */
#define ALIGNARG(log2) 1<<log2
#define ASM_SIZE_DIRECTIVE(name) .size name,.-name;


/* Define an entry point visible from C.  */
#define	ENTRY(name)                                                     \
  .globl C_SYMBOL_NAME(name);                                           \
  .type C_SYMBOL_NAME(name),@function;                                  \
  .p2align 4;                                                           \
  C_LABEL(name)                                                         \
  cfi_startproc;                                                        \
  CALL_MCOUNT

#undef	END
#define END(name)							      \
  cfi_endproc;								      \
  ASM_SIZE_DIRECTIVE(name)

#define ENTRY_CHK(name) ENTRY (name)
#define END_CHK(name) END (name)

/* If compiled for profiling, call `mcount' at the start of each function.  */
#ifdef	PROF
/* The mcount code relies on a normal frame pointer being on the stack
   to locate our caller, so push one just for its benefit.  */
#define CALL_MCOUNT                                                          \
  pushq %rbp;                                                                \
  cfi_adjust_cfa_offset(8);                                                  \
  movq %rsp, %rbp;                                                           \
  cfi_def_cfa_register(%rbp);                                                \
  call JUMPTARGET(mcount);                                                   \
  popq %rbp;                                                                 \
  cfi_def_cfa(rsp,8);
#else
#define CALL_MCOUNT		/* Do nothing.  */
#endif

/* Since C identifiers are not normally prefixed with an underscore
   on this system, the asm identifier `syscall_error' intrudes on the
   C name space.  Make sure we use an innocuous name.  */
#define	syscall_error	__syscall_error
#define mcount		_mcount

#define	PSEUDO(name, syscall_name, args)				      \
lose:									      \
  jmp JUMPTARGET(syscall_error)						      \
  .globl syscall_error;							      \
  ENTRY (name)								      \
  DO_CALL (syscall_name, args);						      \
  jb lose

#undef	PSEUDO_END
#define	PSEUDO_END(name)						      \
  END (name)

#undef JUMPTARGET
#ifdef PIC
#define JUMPTARGET(name)	name##@PLT
#else
#define JUMPTARGET(name)	name
#endif

#undef FUNCALL
#ifdef PIC
#define FUNCALL(name) call name##@PLT
#else
#define FUNCALL(name) call name
#endif

/* Local label name for asm code. */
#ifndef L
/* ELF-like local names start with `.L'.  */

#endif

#define atom_text_section .section ".text.atom", "ax"

/* Long and pointer size in bytes.  */
#define LP_SIZE	8

/* Instruction to operate on long and pointer.  */
#define LP_OP(insn) insn##q

/* Assembler address directive. */
#define ASM_ADDR .quad

/* Registers to hold long and pointer.  */
#define RAX_LP	rax
#define RBP_LP	rbp
#define RBX_LP	rbx
#define RCX_LP	rcx
#define RDI_LP	rdi
#define RDX_LP	rdx
#define RSI_LP	rsi
#define RSP_LP	rsp
#define R8_LP	r8
#define R9_LP	r9
#define R10_LP	r10
#define R11_LP	r11
#define R12_LP	r12
#define R13_LP	r13
#define R14_LP	r14
#define R15_LP	r15

#else	/* !__ASSEMBLER__ */

/* Long and pointer size in bytes.  */
#define LP_SIZE "8"

/* Instruction to operate on long and pointer.  */
#define LP_OP(insn) #insn "q"

/* Assembler address directive. */
#define ASM_ADDR ".quad"

/* Registers to hold long and pointer.  */
#define RAX_LP	"rax"
#define RBP_LP	"rbp"
#define RBX_LP	"rbx"
#define RCX_LP	"rcx"
#define RDI_LP	"rdi"
#define RDX_LP	"rdx"
#define RSI_LP	"rsi"
#define RSP_LP	"rsp"
#define R8_LP	"r8"
#define R9_LP	"r9"
#define R10_LP	"r10"
#define R11_LP	"r11"
#define R12_LP	"r12"
#define R13_LP	"r13"
#define R14_LP	"r14"
#define R15_LP	"r15"

#endif	/* __ASSEMBLER__ */

#ifdef __ASSEMBLER__
/*
  The red zone is a 128 byte below rsp that is effectively free stack space
  for functions, it can be used to store callee saved registers
  (rbx,rbp,r12-r15) and for small temporary storage (i.e saving the inital 
  dest ptr in memcpy), if it's possible to contstrain stack usage to 128 bytes
  then the red zone can be used and avoid adjusting the stack pointer

 */
/*simple definations for 16 8 byte chunks in the redzone*/
#define RED_ZONE 0(%rsp)
#define STACK_1 -8(%rsp)
#define STACK_2 -16(%rsp)
#define STACK_3 -24(%rsp)
#define STACK_4 -32(%rsp)
#define STACK_5 -40(%rsp)
#define STACK_6 -48(%rsp)
#define STACK_7 -56(%rsp)
#define STACK_8 -64(%rsp)
#define STACK_9 -72(%rsp)
#define STACK_10 -80(%rsp)
#define STACK_11 -88(%rsp)
#define STACK_12 -96(%rsp)
#define STACK_13 -104(%rsp)
#define STACK_14 -112(%rsp)
#define STACK_15 -120(%rsp)

/* definations for storing a return value and callee save registers 
   in the red zone (not rbp for some reason)
*/


#define save_rbx()                      \

#define restore_rbx()                   \
        movq  SAVE0(%rsp), %rbx         \
        cfi_restore (%rbx)

#define save_registers()                \
        movq	%r14, SAVE0(%rsp)       \
        cfi_rel_offset (%r14, $SAVE0)   \
        movq	%r13, SAVE1(%rsp)       \
        cfi_rel_offset (%r13, $SAVE1)   \
        movq	%r12, SAVE2(%rsp)       \
        cfi_rel_offset (%r12, $SAVE2)   \
        movq	%rbx, SAVE3(%rsp)       \
        cfi_rel_offset (%rbx, $SAVE3)
#define restore_registers()             \
        movq	SAVE3(%rsp), %rbx       \
        cfi_restore (%rbx)              \
        movq	SAVE2(%rsp), %r12       \
        cfi_restore (%r12)              \
        movq	SAVE1(%rsp), %r13       \
        cfi_restore (%r13)              \
        movq	SAVE0(%rsp), %r14       \
        cfi_restore (%r14)
/*this doesn't need to be valid c, all we're doing is including it in an
  assembly file, so these are assembly macros
*/
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
#endif /* __ASSEMBLER__ */
#endif	/* _X86_64_SYSDEP_H */
