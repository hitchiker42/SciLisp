#ifndef _FRAME_H
#define _FRAME_
#define UNWIND_PROTECT_TAG 1
#include "common.h"
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
#define make_frame(tag_val,value_val,frame_type)            \
  ({frame_addr retval=xmalloc_atomic(sizeof(struct frame)+sizeof(jmp_buf)); \
  retval->tag=tag;                                                      \
  retval->value=value;                                                  \
  retval->value.meta=frame_type;                                        \
  retval->dest=retval+sizeof(struct frame);                       \
  retval;})
#define make_simple_error_handler(_tag_) make_frame(_tag_,NIL,simple_error_frame)
typedef void __attribute__((noreturn)) (*error_handler)(frame);
#define establish_simple_error_handler(name,_tag_,handler_fun)  \
  type_assert(error_handler,handler_fun);                       \
  frame_addr name=make_frame(_tag_,nil,simple_error_frame);     \
  push_frame(current_environment,*name);                        \
  if(setjmp(name->dest)){                                       \
    handler_fun(*name);                                         \
  }


frame_addr frame_search(uint64_t tag);
void unwind_to_frame(env_ptr env,frame_addr fr) __attribute__((noreturn));
void unwind_to_tag(env_ptr env,uint64_t tag) __attribute__((noreturn));
void unwind_with_value(uint64_t tag,sexp value) __attribute__((noreturn));
void unwind_call_stack(env_ptr env,uint64_t index);
void unwind_bindings(env_ptr env,uint64_t n);
#define raise_simple_error(tag,value) unwind_with_value(tag,value)
#endif
