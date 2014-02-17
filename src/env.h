/* definitions of symbols, obarrays and environments, included by common.h

   Copyright (C) 2013-2014 Tucker DiNapoli

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
//TODO: Split into env.c/h and obarray.c/h
//or atleast env.h and env.c/obarray.c
#ifndef __ENV_H__
#define __ENV_H__
//#include "common.h"
enum symbol_interned {
  symbol_interned = 0,
  symbol_uninterned = 1,
  symbol_interned_in_initial_obarray = 2,
};
enum externally_visable {
  symbol_in_current_or_global_obarray = 0,
  symbol_externally_visable = 1,
  symbol_not_externally_visable = 2,
  symbol_locally_visable = 3,
};
//should be allocated using gc_malloc_atomic(sizeof(symbol_name)*name_len)
/*structure for symbol name and simple/common properties, to avoid having
  to access the plist to determine things like constness or typing*/
struct symbol_name {
  uint64_t hashv;
  uint32_t name_len;
  uint8_t multibyte;//really just needs a bit, but we can spare the space
  const char *name;//needs to be last(its basically a variable sized array)
};
struct symbol {
  sexp val;
  sexp plist;
  uint8_t type;
  unsigned interned :2;//enum symbol_interned
  unsigned constant :2;//0 no, 1 yes, 2 warn before changing
  unsigned visibility :2;//enum externally_visable
  //special (aka dynamic variable)
  unsigned special :1;//non special variables don't need to have their values saved
  struct symbol_name *name;
  //pointer to next symbol, in obarray bucket
  symbol *next;
};
//alternate dynamic bindings are thread local, to get a dynamic binding
//we need to search through the bindings stack, this is likely less
//efficent for single threaded programs than rebinding the symbol value
//in the obarray, but it lets me do the same thing regardless of threads
typedef struct binding binding;
typedef struct subr_call subr_call;
struct binding {
  symbol *sym;//pointer to symbol
  sexp val;
};
/* Lexical environments don't need to be special, they're just alists
*/
//the signaling push/pop macros might need to use a statement expression
//rather than a ?: operator
#define push_generic_signal(stack,env,data)     \
  ({HERE();                                     \
    if(env->stack##_ptr>=env->stack##_top){     \
      env->error_num=2;                         \
      raise(SIGUSR1);                           \
    }                                           \
    HERE();                                     \
    env->stack##_index++;                       \
      *(env->stack##_ptr++)=data;})
#define push_generic_no_signal(stack,env,data)                          \
  (env->stack##_ptr>=env->stack##_top?NULL:                             \
   env->stack##_index++,*env->stack##_ptr++=data,1)
#define push_generic_unsafe(stack,env,data)                             \
  (env->stack##_index++,*env->stack##ptr++=data)
#define pop_generic_signal(stack,env)                                   \
  ({if(env->stack##_ptr<=env->stack##_stack){                           \
      env->error_num=2;                                                 \
      raise(SIGUSR1);                                                   \
    }                                                                   \
    env->stack##_index--;                                               \
    *env->stack##_ptr--;})
#define pop_generic_no_signal(stack,env,data)                           \
  (env->stack##_ptr<=env->stack##_stack?NULL:                           \
   env->stack##_size--,data=*env->stack##_ptr--,1)
#define pop_generic_unsafe(stack,env)           \
  (env->stack##_index--,env->stack##_ptr--)
#define stack_size(stack,env) (env->stack##_ptr-env->stack##_stack)

#define data_size(env) (stack_size(data,env))
#define binding_size(env) (stack_size(binding,env))
#define frame_size(env) (stack_size(frame,env))
#define call_size(env) (stack_size(call,env))

#define push_binding(env,data) push_generic_signal(binding,env,data)
#define pop_binding(env) pop_generic_signal(binding,env)
#define push_frame(env,data) push_generic_signal(frame,env,data)
#define pop_frame(env) pop_generic_signal(frame,env)
#define push_call(env,data) push_generic_signal(call,env,data)
#define pop_call(env) pop_generic_signal(call,env)
#define push_data(env,data) push_generic_signal(data,env,data)
#define pop_data(env) pop_generic_signal(data,env)

#define try_push_binding(env,data) push_generic_no_signal(binding,env,data)
#define try_pop_binding(env,data) pop_generic_no_signal(binding,env)
#define try_push_frame(env,data) push_generic_no_signal(frame,env,data)
#define try_pop_frame(env,data) pop_generic_no_signal(frame,env)
#define try_push_call(env,data) push_generic_no_signal(call,env,data)
#define try_pop_call(env,data) pop_generic_no_signal(call,env)
#define try_push_data(env,data) push_generic_no_signal(data,env,data)
#define try_pop_data(env,data) pop_generic_no_signal(data,env)

#define LEX_BIND(env,val,sym) (env->lex_env=Cons(Cons(sym,val),env->lex_env),env->lex_bindings++)
#define LEX_BIND_CURRENT(val,sym) LEX_BIND(current_env,val,sym)

//two megs seems a normal default, but that seems a bit much for now
//so I'll use 2^15 bytes, whatever that is
static uint32_t frame_stack_size=2<<15;//max size of an signed short
static uint32_t data_stack_size=2<<14;
static uint32_t bindings_stack_size=2<<14;
static uint32_t call_stack_size=2<<15;
void init_environment(void);
void *init_environment_pthread(void*);
int lisp_pthread_create(pthread_t *thread,const pthread_attr_t *attr,
                       void*(*start_routine)(void*),void *arg);
/* per thread values (no need for a lock)*/
struct environment {
  //data which shouldn't be set to 0
  package *current_package;//contains current obarray and
  //c thread local data
  stack_t *sigstack;//alternative stack for signals
  //stacks
  binding *bindings_stack;//dynamic bindings stack
  binding *bindings_ptr;//stack pointer
  binding *bindings_top;//top of dynamic bindings stack
  //holds lables(frames,jump points, whatever) for functions/errors,etc
  frame *frame_stack;//stack of jump points (returns, catches, handlers)
  frame *frame_ptr;//stack pointer
  frame *frame_top;
  //records function calls
  //also holds the lexical environment
  subr_call *call_stack;//call stack
  subr_call *call_ptr;//stack pointer
  subr_call *call_top;
  //holds function arguments mostly (also used by reader)
  sexp *data_stack;//data/function argument stack
  sexp *data_ptr;//stack ptr
  sexp *data_top;
  //return values? for now I'll just use the stack
  //everything below here can be set to zero on error
  sexp lex_env;//current lexical environment, call stack is used to unwind it
  //used for dealng with c signals, if error_num is set to a non-zero
  //value that means the current signal was sent due to a lisp error
  uint32_t error_num;//maybe define to sig_atomic_t?
  uint32_t frame_index;
  uint32_t data_index;
  uint32_t bindings_index;
  uint32_t call_index;
};
struct subr_call {
  sexp lex_env;
  //  sexp old_lex_env;
  sexp lisp_subr;
  uint32_t bindings_index;
};
static inline sexp lex_assq(sexp lex_env,symbol *var){
  while(CONSP(lex_env)){
    if(XCAR(XCAR(lex_env)).val.uint64 == (uint64_t)var){
      return XCAR(lex_env);
    }
    lex_env=XCDR(lex_env);
  }
  return NIL;
}
/* for now dealt with by the call stack
//should it be an error if num_bindings > env->lex_bindings?
static void unwind_lex_env(environment *env,uint32_t num_bindings){
  if(num_bindings==env->lex_bindings){
    env->lex_env=NIL;
    return;
  } else {
    int i;
    for(i=0;i<num_bindings;i++){
      env->lex_env=XCDR(env->lex_env);
      env->lex_bindings--;
    }
  }
  }*/
obarray *global_obarray;
//extern uint64_t bindings_stack_size;
//extern uint64_t handler_stack_size;
symbol *copy_symbol(symbol *sym,int copy_props);
sexp get_keyword_type(sexp obj);
struct obarray {
  symbol **buckets;
  uint32_t size;//number of buckets
  uint32_t used;//buckets used
  uint32_t entries;//number of symbols in the table
  float capacity;//entries/size(for convience)
  float capacity_inc;//1/(size*10) (10 is soft cap on entries/bucket)
  float gthreshold;//value of capacity at which to enlarge the table
  float gfactor;//ammount to multiply size by when growing the table
#ifdef MULTI_THREADED
  pthread_rwlock_t *restrict lock;
#endif
  //32 bits of padding
};
/*
  These need to be modified to have a more consistant interface
  and allow for specifying a multibyte string, or not
 */
symbol* lookup_symbol(const char* name,struct obarray *ob);
//something like a default arg for lookup_symbol
#define lookup_symbol_global(name) lookup_symbol(name,global_obarray)
obarray *make_obarray(uint32_t size,float gthreshold,float gfactor);
symbol *c_intern(const char* name,uint32_t len,struct obarray *ob);
symbol *c_intern_no_copy(const char* name,uint32_t len,obarray *ob);
symbol *obarray_lookup_sym(symbol_name *sym_name,obarray *ob);
sexp lisp_intern(sexp sym_or_name,sexp ob);
void c_intern_unsafe(obarray *ob,symbol* new);
symbol_name* make_symbol_name(const char *name,uint32_t len,uint64_t hashv);
symbol_name* make_symbol_name_no_copy(const char *name,uint32_t len,
                                      uint64_t hashv);
symbol* make_symbol_from_name(symbol_name *name);
void c_signal_handler(int signo,siginfo_t *info,void *context_ptr);
//needs to be in a global header, and xmalloc isn't defined in types.h
static inline lisp_string *make_string(const char *str){
  lisp_string *retval=xmalloc(sizeof(lisp_string));
  if(str[0] == '\0'){
    *retval=(lisp_string){.cord=str,.len=(CORD_len(str))};
  } else {
    *retval=(lisp_string){.string=str,.len=(strlen(str))};
  }
  return retval;
}
//should probably make the normal make_string this, but I don't want to
//go through and replace things right now
static inline lisp_string *make_string_len(const char *str,uint32_t len){
 lisp_string *retval=xmalloc(sizeof(lisp_string));
  if(!len){
    len=CORD_len(str);
  }
  if(str[0] == '\0'){
    *retval=(lisp_string){.cord=str,.len=(CORD_len(str))};
  } else {
    *retval=(lisp_string){.string=str,.len=(strlen(str))};
  }
  return retval;
}
#endif
