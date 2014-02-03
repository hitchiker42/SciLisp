/* frames for nonlocal exits, returns, error handling and the like

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

//look for a frame, don't unwind, ignore proctect flags
#include "common.h"
frame_addr frame_search(uint64_t tag){
  env_ptr env=current_env;
  frame_addr ptr=env->frame_ptr;
  for(ptr=env->frame_ptr;ptr>=env->frame_stack;ptr--){
    if(ptr->tag == tag){
      return ptr;
    }
  }
  return NULL;
}
#define unwind_stacks(env)                      \
  unwind_bindings(env,(env->bindings_index)-(env->frame_ptr->bindings_index)); \
  unwind_call_stack(env,(env->call_index)-(env->frame_ptr->call_index)); \
  env->frame_index=env->frame_ptr->frame_index;
void __attribute__((noreturn)) unwind_to_frame(env_ptr env,frame_addr fr){
  while(env->frame_ptr != fr && env->frame_ptr->tag != (uint64_t)UNWIND_PROTECT_TAG){
    env->frame_ptr--;    
  }
  unwind_stacks(env);
  longjmp(env->frame_ptr->dest,1);
}
void __attribute__((noreturn)) unwind_to_tag(env_ptr env,uint64_t tag){
  while(env->frame_ptr->tag != tag && 
        env->frame_ptr != (uint64_t)UNWIND_PROTECT_TAG){
    env->frame_ptr--;
  }
  unwind_stacks(env);
  longjmp(env->frame_ptr->dest,1);
}
void __attribute__((noreturn)) unwind_with_value(uint64_t tag,sexp value){
  frame_addr fr=frame_search(tag);
  fr->value=value;
  unwind_to_frame(current_env,fr);
}
void unwind_call_stack(env_ptr env,uint64_t index){
  env->call_ptr=env->call_ptr-index;
  env->call_index=(uint64_t)env->call_ptr-index;
  env->lex_env=env->call_ptr->lex_env;
}
void unwind_bindings(env_ptr env,uint64_t n){
  while(n>0){
    env->bindings_ptr--;
    env->bindings_index--;
  }
}
  
/*
  typedef struct frame frame[1]
  #define make_frame(tag,type){.tag=tag,.frame_type=type}
  various non local exits:
  blocks:
  (block name &rest body)
    functions are implicitly enclosed in blocks named nil, to
    allow returning, (actually probably not, because
    I should be able to do that via a local jump and untill I
    figure out how to best do that it's not worth the overhead
    of longjmp for every function)
  defun block(name,body){
    frame new_frame;
    *new_frame=make_frame(name,block);
    if(setjmp(new_frame->dest)){
      return new_frame->value;
    }
    push_frame(current_environment,new_frame);//signals on everflow
    return eval(body);
  }
  tagbody:
    tagbody (tag|form)*
    evaluate forms, tags are symbols or unsigned integers and are targets for go
    should be able to optimize to local jumps for some cases
    defun tagbody(args){
    frame *tag;
    sexp start=args;
    while(CONSP(args)){
    if(SYMBOLP(XCAR(args)) || INTP(XCAR(args)){
    *tag=xmalloc(sizeof(struct frame));
    *tag->tag=XCAR(args).val.uint64_t;
    //not sure if this will work or not
    if(setjmp(*tag->dest)){
      eval(*tag->value);
    } else {
    *tag->value=XCDR(args);
      //strip out the tag so we don't set it twice
      XCAR(args)=XCADR(args);
      XCDR(args)=XCDDR(args);
      push_frame(current_environment,*tag);
    }
    } else {
      args=XCDR(args);
     }
     return eval(start);
}
  go:
    (go tag)
    goto the first tagbody frame on the frame stack who's value is eq to tag
  catch:
    (catch tag forms*)
    evaluate and return value of forms unless interrupted by throw,
  throw:
    (throw tag form)
    return the value of form from the nearest enclosing catch with a
    tag eq to tag
  return;
    (return form)
    return the value of form from the nearest enclosing block with a tag of nil
  return-from:
    (return-from tag form)
    return the value of form from the nearest enclosing block with
    a tag eq to tag
  unwind-protect:
    (unwind-protect protected cleanup*)
    establish a handler to catch any nonlocal exit from protected
    then evaluate protected followed by cleanup, regardless of how
    protected is exited, return value of protected
    
 */
