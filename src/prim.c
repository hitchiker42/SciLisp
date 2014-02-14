#define INSIDE_PRIMS
#include "scilisp.h"
#include <locale.h>
#include <langinfo.h>
#include "prim.h"
MAKE_TYPE(Tint8,int8,4,0xf5a67dc57a8fe232 ,{0},sexp_int8);
MAKE_TYPE(Tint16,int16,5,0xf9e84c8f42970271 ,{0},sexp_int16);
MAKE_TYPE(Tint32,int32,5,0xf9e1c08f4291a8df ,{0},sexp_int32);
MAKE_TYPE(Tint64,int64,5,0xf9d0c88f42834344 ,{0},sexp_int64);
MAKE_TYPE(Tuint8,uint8,5,0x34fa7f24f14f37fb ,{0},sexp_uint8);
MAKE_TYPE(Tuint16,uint16,6,0x54bf46c60981dbb2 ,{0},sexp_uint16);
MAKE_TYPE(Tuint32,uint32,6,0x54c64ac60988012c ,{0},sexp_uint32);
MAKE_TYPE(Tuint64,uint64,6,0x54d746c609966d93 ,{0},sexp_uint64);
MAKE_TYPE(Terror,error,5,0x9f7452dd75d54d31 ,{0},sexp_error);
MAKE_TYPE(Treal32,real32,6,0x5db280f342c8a248 ,{0},sexp_real32);
MAKE_TYPE(Treal64,real64,6,0x5da874f342c0395f ,{0},sexp_real64);
MAKE_TYPE(Tbigint,bigint,6,0x102f3138836b306a ,{0},sexp_bigint);
MAKE_TYPE(Tbigfloat,bigfloat,8,0xf7de010e01156121 ,{0},sexp_bigfloat);
MAKE_TYPE(Tchar,char,4,0xf2a393910b5b3ebd ,{0},sexp_char);
MAKE_TYPE(Tc_char,c_char,6,0x781741dbb946858f ,{0},sexp_c_char);
MAKE_TYPE(Tstring,string,6,0x704be0d8faaffc58 ,{0},sexp_string);
MAKE_TYPE(Tarray,array,5,0x4f9e14b634c6b026 ,{0},sexp_array);
MAKE_TYPE(Tstream,stream,6,0x4f6a36d8e8907985 ,{0},sexp_stream);
MAKE_TYPE(Tsubr,subr,4,0xb6d0791921c78b65 ,{0},sexp_subr);
MAKE_TYPE(Tsymbol,symbol,6,0xe81b0096bc73f511 ,{0},sexp_symbol);
MAKE_TYPE(Ttype,type,4,0xa79439ef7bfa9c2d ,{0},sexp_type);
MAKE_TYPE(Thashtable,hashtable,9,0xdc0a27c41e40e62b ,{0},sexp_hashtable);
MAKE_TYPE(Tregex,regex,5,0xbc35a7f3228f2b18 ,{0},sexp_regex);
MAKE_TYPE(Tnil,nil,3,0x2146ba19257dc6ac ,{0},sexp_nil);
MAKE_TYPE(Tcons,cons,4,0x0bca0591195d8188 ,{0},sexp_cons);
MAKE_TYPE(Tenv,env,3,0xc2f01118f05367d4 ,{0},sexp_env);
MAKE_TYPE(Tobarray,obarray,7,0x44bf7978b8fde4e9 ,{0},sexp_obarray);
MAKE_TYPE(Ttrue,true,4,0x5b5c98ef514dbfa5 ,{0},sexp_true);
MAKE_TYPE(Tfalse,false,5,0xb5fae2c14238b978 ,{0},sexp_false);
MAKE_TYPE(Tuninterned,uninterned,10,0x4c424d27afb1f41b ,{0},sexp_uninterned);
MAKE_SELF_QUOTING_SYMBOL(Etype,"type",4,0xa79439ef7bfa9c2d ,{0});
MAKE_SELF_QUOTING_SYMBOL(Ebounds,"bounds",6,0x52f60c4caef0b768 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Efile,"file",4,0xaad01178f02a6a23 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eread,"read",4,0x4ce6531fbfddd605 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eargs,"args",4,0x894930843ebc9d7c ,{0});
MAKE_SELF_QUOTING_SYMBOL(Ekey,"key",3,0x3dc94a19365b10ec ,{0});
MAKE_SELF_QUOTING_SYMBOL(Efatal,"fatal",5,0x83eeb7c1b653d9b9 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eundefined,"undefined",9,0x22a0e850add468a3 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eunbound,"unbound",7,0xc585bfa8c28cb01a ,{0});
MAKE_SELF_QUOTING_SYMBOL(Emath,"math",4,0x1f4f66a2ce8fb60f ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eeof,"eof",3,0xc2f32318f055bc41 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eio,"io",2,0x08b73907b55c4d71 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eoverflow,"overflow",8,0xe5be01d8506421ab ,{0});
MAKE_SELF_QUOTING_SYMBOL(Erange,"range",5,0x526eb811b28d5cb2 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Econst,"const",5,0x65c9718e19e3df34 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Esystem,"system",6,0xbfb559c71c56b4fc ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eprint,"print",5,0x2f0792248c7d6068 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Evisibility,"visibility",10,0x512b4b435e30c025 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Eilseq,"ilseq",5,0xce27407d802a4137 ,{0});
MAKE_SYMBOL(lisp_stdin,"stdin",5,0x07ce0cad53eda57d ,STDIN_FILENO,{0},0);
MAKE_SYMBOL(lisp_stdout,"stdout",6,0x42e6d785a74f8c66 ,STDOUT_FILENO,{0},0);
MAKE_SYMBOL(lisp_stderr,"stderr",6,0x104ce5858b0a80b5 ,STDERR_FILENO,{0},0);
MAKE_SELF_QUOTING_SYMBOL(Kend,"end",3,0xc2f00318f053500a ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kstart1,"start1",6,0xb62de96d65362d2a ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kcount,"count",5,0xb1e5e28e4479a274 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kdocumentation,"documentation",13,0x1fc49e5ff4bc9a81 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kend1,"end1",4,0x91257e605d915441 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kend2,"end2",4,0x91257b605d914f28 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kexport,"export",6,0xedb2d39755161f73 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kimport,"import",6,0xf4d30088534a1af4 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Ktest,"test",4,0xf9e6e6ef197c2b25 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kkey,"key",3,0x3dc94a19365b10ec ,{0});
MAKE_SELF_QUOTING_SYMBOL(Ksize,"size",4,0x4dea9618e618ae3c ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kstart,"start",5,0xee5d97ad45ad251f ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kstart2,"start2",6,0xb62de86d65362b77 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Kuse,"use",3,0x4c52b0193dcce634 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qlambda,"lambda",6,0x826b4caaf325324a ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qclosure,"closure",7,0xeb2a39e5ed4703c2 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qnil,"nil",3,0x2146ba19257dc6ac ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qlet,"let",3,0x127284191dcc577a ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qlet_star,"let_star",8,0x1556c21189ca5c83 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qwhile,"while",5,0xce87a3885811296e ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qtagbody,"tagbody",7,0x3d7e664e220d379b ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qgo,"go",2,0x08953907b53f670b ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qthrow,"throw",5,0x5a5fe3720c9584cf ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qcatch,"catch",5,0xc1b2e33b13ec076c ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qsetq,"setq",4,0x2d34e218d41e5556 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qunwind_protect,"unwind_protect",14,0x7b68d0b2f7893d3c ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qif,"if",2,0x08b73007b55c3e26 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qprogv,"progv",5,0xf987a0246dc0f3a1 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qprogn,"progn",5,0xf98788246dc0cad9 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qreturn_from,"return_from",11,0xca3146fbdb07b230 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qblock,"block",5,0x14e5faab9ce0e362 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qquote,"quote",5,0x5a60f1143b2fd6f7 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qcomma,"comma",5,0x6d47878e1dcd3758 ,{0});
MAKE_SELF_QUOTING_SYMBOL(Qbackquote,"backquote",9,0x3237a633eea1ab5e ,{0});
mpz_t *lisp_mpz_1,*lisp_mpz_0;
mpfr_t *lisp_mpfr_1,*lisp_mpfr_0;
static void init_global_obarray();
void init_prims(){
if(init_prims_flag){
  init_prims_flag=0;
} else {
  return;
}
global_obarray=xmalloc(sizeof(obarray));
*global_obarray=(obarray)
{.size=128,.used=0,.entries=0,.capacity=0.0,
 .capacity_inc=(1.0/(128*10)),.gthreshold=0.75,.gfactor=2};
global_obarray->buckets=xmalloc(128*sizeof(symbol*));
#ifdef MULTI_THREADED
global_obarray->lock=xmalloc(sizeof(pthread_rwlock_t));
pthread_rwlock_init(global_obarray->lock,NULL);
#endif
//global_environment=xmalloc(sizeof(environment));
HERE();
init_global_obarray(global_obarray);
HERE();
mpfr_set_default_prec(256);
mp_set_memory_functions(GC_MALLOC_1,GC_REALLOC_3,GC_FREE_2);
//set_global_vars();
srand48(time(NULL));
//lisp_init_rand(NIL);
//prep_sexp_cifs();
//test if the user's locale is utf-8 compatable
setlocale(LC_ALL,"");//set locale based on environment variables
char *locale_codeset=nl_langinfo(CODESET);
if(!strcmp("UTF-8",locale_codeset)){
  ;//hopefully the most common case, we're in a utf-8 locale, good
} else {
  //not utf-8, in a desperate attempt to get things working
  //we try to set the locale to en_US.UTF-8, after printing a warning
  fprintf(stderr,"Warning default locale does not use UTF-8 encoding,\n"
  "Attempting to set locale to en_US.UTF-8\n");
  if(setlocale(LC_ALL,"en_US.UTF-8")){
    ;//somehow that worked
  } else {
    fprintf(stderr,"Error, SciLisp requires a locale with a utf-8 codeset\n");
    exit(EXIT_FAILURE);
  }
}
//INIT_SYNONYM(lisp_consp,"cons?",1);
}
static void init_global_obarray(){

c_intern_unsafe(global_obarray,Qlambda);
c_intern_unsafe(global_obarray,Qclosure);
c_intern_unsafe(global_obarray,Qnil);
c_intern_unsafe(global_obarray,Qlet);
c_intern_unsafe(global_obarray,Qlet_star);
c_intern_unsafe(global_obarray,Qwhile);
c_intern_unsafe(global_obarray,Qtagbody);
c_intern_unsafe(global_obarray,Qgo);
c_intern_unsafe(global_obarray,Qthrow);
c_intern_unsafe(global_obarray,Qcatch);
c_intern_unsafe(global_obarray,Qsetq);
c_intern_unsafe(global_obarray,Qunwind_protect);
c_intern_unsafe(global_obarray,Qif);
c_intern_unsafe(global_obarray,Qprogv);
c_intern_unsafe(global_obarray,Qprogn);
c_intern_unsafe(global_obarray,Qreturn_from);
c_intern_unsafe(global_obarray,Qblock);
c_intern_unsafe(global_obarray,Qquote);
c_intern_unsafe(global_obarray,Qcomma);
c_intern_unsafe(global_obarray,Qbackquote);
c_intern_unsafe(global_obarray,Etype);
c_intern_unsafe(global_obarray,Ebounds);
c_intern_unsafe(global_obarray,Efile);
c_intern_unsafe(global_obarray,Eread);
c_intern_unsafe(global_obarray,Eargs);
c_intern_unsafe(global_obarray,Ekey);
c_intern_unsafe(global_obarray,Efatal);
c_intern_unsafe(global_obarray,Eundefined);
c_intern_unsafe(global_obarray,Eunbound);
c_intern_unsafe(global_obarray,Emath);
c_intern_unsafe(global_obarray,Eeof);
c_intern_unsafe(global_obarray,Eio);
c_intern_unsafe(global_obarray,Eoverflow);
c_intern_unsafe(global_obarray,Erange);
c_intern_unsafe(global_obarray,Econst);
c_intern_unsafe(global_obarray,Esystem);
c_intern_unsafe(global_obarray,Eprint);
c_intern_unsafe(global_obarray,Evisibility);
c_intern_unsafe(global_obarray,Eilseq);
c_intern_unsafe(global_obarray,Tint8);
c_intern_unsafe(global_obarray,Tint16);
c_intern_unsafe(global_obarray,Tint32);
c_intern_unsafe(global_obarray,Tint64);
c_intern_unsafe(global_obarray,Tuint8);
c_intern_unsafe(global_obarray,Tuint16);
c_intern_unsafe(global_obarray,Tuint32);
c_intern_unsafe(global_obarray,Tuint64);
c_intern_unsafe(global_obarray,Terror);
c_intern_unsafe(global_obarray,Treal32);
c_intern_unsafe(global_obarray,Treal64);
c_intern_unsafe(global_obarray,Tbigint);
c_intern_unsafe(global_obarray,Tbigfloat);
c_intern_unsafe(global_obarray,Tchar);
c_intern_unsafe(global_obarray,Tc_char);
c_intern_unsafe(global_obarray,Tstring);
c_intern_unsafe(global_obarray,Tarray);
c_intern_unsafe(global_obarray,Tstream);
c_intern_unsafe(global_obarray,Tsubr);
c_intern_unsafe(global_obarray,Tsymbol);
c_intern_unsafe(global_obarray,Ttype);
c_intern_unsafe(global_obarray,Thashtable);
c_intern_unsafe(global_obarray,Tregex);
c_intern_unsafe(global_obarray,Tnil);
c_intern_unsafe(global_obarray,Tcons);
c_intern_unsafe(global_obarray,Tenv);
c_intern_unsafe(global_obarray,Tobarray);
c_intern_unsafe(global_obarray,Ttrue);
c_intern_unsafe(global_obarray,Tfalse);
c_intern_unsafe(global_obarray,Tuninterned);
c_intern_unsafe(global_obarray,Kend);
c_intern_unsafe(global_obarray,Kstart1);
c_intern_unsafe(global_obarray,Kcount);
c_intern_unsafe(global_obarray,Kdocumentation);
c_intern_unsafe(global_obarray,Kend1);
c_intern_unsafe(global_obarray,Kend2);
c_intern_unsafe(global_obarray,Kexport);
c_intern_unsafe(global_obarray,Kimport);
c_intern_unsafe(global_obarray,Ktest);
c_intern_unsafe(global_obarray,Kkey);
c_intern_unsafe(global_obarray,Ksize);
c_intern_unsafe(global_obarray,Kstart);
c_intern_unsafe(global_obarray,Kstart2);
c_intern_unsafe(global_obarray,Kuse);
c_intern_unsafe(global_obarray,lisp_stdin);
c_intern_unsafe(global_obarray,lisp_stdout);
c_intern_unsafe(global_obarray,lisp_stderr);
}
