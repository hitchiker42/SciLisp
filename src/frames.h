/* Header file for frames used for control flow and signal handling

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
#ifndef _FRAME_H
#define _FRAME_H
#define UNWIND_PROTECT_TAG Qunwind_protect
enum  frame_type {//might not use this
  //needs as many bits as possible values,if extended to 9 values
  //will need to be enlarged to a 16 bit value
  //to search for a frame type do
  // if(<frame_type> & frame->value.meta)
  //so that unwind protect catches anything
    function_frame=0x81,
    block_frame=0x82,
    catch_frame=0x84,
    tagbody_frame=0x88,
    simple_error_frame=0x90,//non-restartable errors
    condition_frame=0xa0,//uses ucontext, restartable, special
    unwind_protect_frame=0x80,//catches anythings
};
struct frame {
  uint64_t tag;//a pointer to a symbol or an integer
  sexp value;//for throw or return
  uint32_t bindings_index;//used to unwind bindings
  uint32_t call_index;//used to unwind lex env/call stack
  uint32_t frame_index;
  jmp_buf dest;
};
#define make_frame(tag_val,frame_type)                                  \
  ({frame_addr retval=xmalloc_atomic(sizeof(struct frame));             \
    retval->tag=tag_val;                                                \
    retval->value=NIL;                                                  \
    retval->value.meta=frame_type;                                      \
    retval;})
#define make_simple_error_handler(_tag_) make_frame(_tag_,simple_error_frame)
typedef void __attribute__((noreturn)) (*error_handler)(frame);
#define establish_simple_error_handler(name,_tag_,handler_fun)  \
  type_assert(error_handler,handler_fun);                       \
  frame_addr name=make_frame(_tag_,nil,simple_error_frame);     \
  push_frame(current_env,*name);                                \
  if(setjmp(name->dest)){                                       \
    handler_fun(*name);                                         \
  }


frame_addr frame_search(uint64_t tag);
void unwind_to_frame(env_ptr env,frame_addr fr) __attribute__((noreturn));
void unwind_to_tag(env_ptr env,uint64_t tag) __attribute__((noreturn));
void unwind_with_value(uint64_t tag,sexp value) __attribute__((noreturn));
void unwind_call_stack(env_ptr env,uint64_t index);
void unwind_bindings(env_ptr env,uint64_t n);
#define raise_sexp_error(tag,value) unwind_with_value((uint64_t)tag,value)
#define raise_simple_error(tag,value) unwind_with_value((uint64_t)tag,c_string_sexp(value))
#define raise_simple_error_cord(tag,value) unwind_with_value((uint64_t)tag,\
                                                             string_sexp(make_string(value)))
#define raise_simple_error_fmt(tag,format,args...)                      \
  unwind_with_value((uint64_t)tag,                                      \
                    string_sexp(make_string(CORD_asprintf(format,##args))))

#endif
