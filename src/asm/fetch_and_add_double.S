/* fetch_and_add_double.S
              
   Copyright (C) 2014 Tucker DiNapoli

   This file is part of SciLisp.

   SciLisp is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   SciLisp is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with SciLisp.  If not, see <http://www.gnu.org*/
/*double fetch_and_add_double(double *addr,double val);*/
#define MAX_TRIES $3
/*This might be different but that's why it's a macro*/
#define QNAN $0ffffffffffffffff
fetch_and_add_double:
.L(start):
        .cfi_startproc
        movq (%rdi),%rax #load *addr into rax
        movq MAX_TRIES,%rcx #set loop index
.L(loop):        
        paddsd (%rdi),%xmm0  #val += *addr
        movq %xmm0,%rdx
        lock cmpxchgq (%rdi),%rdx
        loopne .L(start) #try again if xchg failed and ecx > 0
        jnz .L(fail) #unless ZF is set we failed
        ret 
.L(fail)
        movq QNAN,%rax
        ret
       .cfi_endproc
