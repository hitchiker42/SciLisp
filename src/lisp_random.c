#include "SFMT/SFMT.h"
//some helper functions to make translating things to lisp eaiser
//essentially translate everything to functions of one argument
struct rand_state {
  sfmt_t sfmt;
  sfmt_buf buf;
  uint32_t seed;
};
#define call_rand(f,state) f(&state->sfmt,&state->buf)
typedef struct rand_state rand_state;
static uint32_t lisp_nrand32(rand_state *state){
  return call_rand(sfmt_nrand32_buf,state);
}
static uint32_t lisp_jrand32(rand_state *state){
  return call_rand(sfmt_jrand32_buf,state);
}
static uint64_t lisp_nrand64(rand_state *state){
  return call_rand(sfmt_nrand64_buf,state);
}
static uint64_t lisp_jrand64(rand_state *state){
  return call_rand(sfmt_jrand64_buf,state);
}
static double lisp_erand64(rand_state *state){
  return sfmt_to_res53(lisp_nrand64(state));
}
static void init_rand_state(rand_state *state,uint32_t seed){
  if(!seed){
    seed=sfmt_init_fast_seed_r(&state->sfmt);
  } else {
    sfmt_init_explicit_r(&state->sfmt,seed);
  }
  sfmt_init_buf_r(&state->sfmt,&state->buf);
  state->seed=seed;
  return state;
}
static rand_state static_rand_state[1];
//(defun init-rand (&optional seed)
//when I add better support for unsigned/specific sized ints
//I should look at this again
sexp lisp_init_rand(sexp seed){
  rand_state_static->sfmt=sfmt_static;
  rand_state_static->buf=sfmt_static_buf;
  if(NILP(seed)){
    sfmt_init_fast_static();
    return NIL;
  } else if (!INTP(seed)){
    raise_simple_error(Etype,format_type_error_opt("init-rand","integer",seed.tag));
  } else {
    uint32_t seed_val=(uint32_t)seed.val.int64;
    sfmt_init_explicit_static(seed_val);
    return NIL;
  }
}
//(defun init-rand-r (&optional seed)
sexp lisp_init_rand_r(sexp seed){
  if(!NILP(seed) && !INTP(seed)){
    raise_simple_error(Etype,format_type_error_opt("init-rand","integer",seed.tag));
  }
  rand_state *sfmt=xmalloc_atomic(sizeof(rand_state));
  uint32_t seed_val=(NILP(seed)?0:(uint32_t)seed.val.int64);
  rand_state_init(sfmt,seed_val);
  return opaque_sexp(sfmt);
}
//(defun rand-int (&optional state unsigned)
sexp lisp_randint(sexp sfmt,sexp un_signed){
  if(!OPAQUEP(sfmt) && !NILP(sfmt)){
    raise_simple_error(Etype,format_type_error_opt("float","random-state",sfmt.tag));
  }
  rand_state *sfmt_val;
  if(NILP(sfmt)){
    sfmt_val=rand_state_static;
  } else{
    sfmt_val=(rand_state*)sfmt.val.opaque;
  }
  if(is_true(un_signed)){
    return long_sexp(rand_state_nrand64(sfmt_val));
  } else {
    return long_sexp(rand_state_jrand64(sfmt_val));
  }
}
//(defun rand-float (&optional state scale))
sexp lisp_randfloat(sexp sfmt,sexp scale){
  if(!OPAQUEP(sfmt) && !NILP(sfmt)){
    raise_simple_error(Etype,format_type_error_opt("rand-float","random-state",sfmt.tag));
  }
  rand_state *sfmt_val;
  if(NILP(sfmt)){
    sfmt_val=rand_state_static;
  } else{
    sfmt_val=(rand_state*)sfmt.val.opaque;
  }
  if(NILP(scale)){
    return double_sexp(rand_state_erand64(sfmt_val));
  } else if (!NUMBERP(scale)){
    raise_simple_error(Etype,format_type_error_opt("rand-float","number",scale.tag));
  } else {
    return lisp_mul_num(scale,
                        double_sexp(rand_state_erand64(sfmt_val)));
  }
}
/*
//I wonder if it'd be faster to allocate one array
//and fill it with 2x the needed number of random numbers
//then modify it in place to get the result, being as it
//would only need 1 allocation
static sexp c_rand_array(int len,sfmt_t *sfmt,sexp_tag type){
  sexp *retval=xmalloc_atomic(sizeof(sexp)*len);
  uint32_t *sfmt_array=alloca(sizeof(uint32_t)*len);
  //needs typechecking
  sfmt_fill_array32(sfmt,sfmt_array,len*2);
  int i;
  //should compile no a noop
  uint64_t *sfmt_array64=(uint64_t*)(sfmt_array);
  switch(type){
    case sexp_int64:
      for(i=0;i<len;i++){
        retval[i]=long_sexp(sfmt_array64[i]);
      };
      return array_sexp(retval,len);
    case sexp_real64:
      for(i=0;i<len;i++){
        retval[i]=double_sexp(sfmt_to_res53(sfmt_array64[i]));
      }
      return array_len_sexp(retval,len);
  }
}
//(defun rand-array (len &optional sfmt type))
sexp rand_array_r(sexp len,sexp sfmt,sexp type){
  if(!INTP(len)){
    raise_simple_error(Etype,format_type_error("rand-array-r","integer",len.tag));
  }
  if(!OPAQUEP(sfmt) && !NILP(sfmt)){
    raise_simple_error(Etype,format_type_error_opt_named
                       ("rand-array","state","random-state",sfmt.tag));
  }
  sfmt_t *sfmt_val;
  if(NILP(sfmt)){
    sfmt_val=&(rand_state_static->sfmt);
  } else {
    sfmt_val=&(((rand_state*)(sfmt.val.opaque))->sfmt);
  }
  uint64_t array_len=len.val.int64;
  sexp_tag type_tag;
  if(!SYMBOLP(type) && !NILP(type)){
    raise_simple_error(Etype,format_type_error_opt("rand-array-r",
                                                   "keyword",type.tag));
  }
  if(NILP(type)){
    type_tag=sexp_long;
  } else {//type must be a keyword
    sexp type_sexp=getKeywordType(type);
    if(!TYPEP(type_sexp)){
      return type_sexp;
    }
    type_tag=type_sexp.val.type;
  }
  return c_rand_array(array_len,sfmt_val,type_tag);
}
/*sexp lisp_randint(sexp un_signed){
  if(NILP(un_signed)){
    return long_sexp(mrand48());
  } else {
    return long_sexp(lrand48());
  }
}
sexp lisp_randfloat(sexp scale){
  double retval;
  if(scale.tag != _nil){
    retval=drand48()*get_double_val(scale);
  } else {
    retval = drand48();
  }
  return double_sexp(retval);
  }*/
