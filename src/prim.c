#define INSIDE_PRIMS
#include "scilisp.h"
#include <locale.h>
#include <langinfo.h>
#include "prim.h"
/*Types*/
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
/*Errors*/
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
MAKE_SELF_QUOTING_SYMBOL(Einternal,"internal",8,0xe08a40f0f6141500 ,{0});
/*Subroutines*/
PRIM_DEFSUBR("array?",lisp_arrayp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("cons?",lisp_consp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("number?",lisp_numberp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("integer?",lisp_integerp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("string?",lisp_stringp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("stream?",lisp_streamp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("sequence?",lisp_sequencep,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("real?",lisp_realp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("bignum?",lisp_bignump,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("bigint?",lisp_bigintp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("bigfloat?",lisp_bigfloatp,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("hashtable?",lisp_hashtablep,1,0,0,0,1,f1,"(object)",subr_compiled);
PRIM_DEFSUBR("eq",lisp_eq,2,0,0,0,2,f2,"(obj1 obj2)",subr_compiled);
PRIM_DEFSUBR("eql",lisp_eql,2,0,0,0,2,f2,"(obj1 obj2)",subr_compiled);
PRIM_DEFSUBR("equal",lisp_equal,2,0,0,0,2,f2,"(obj1 obj2)",subr_compiled);
PRIM_DEFSUBR("even?",lisp_evenp,1,0,0,0,1,f1,"(integer)",subr_compiled);
PRIM_DEFSUBR("odd?",lisp_oddp,1,0,0,0,1,f1,"(integer)",subr_compiled);
PRIM_DEFSUBR("zero?",lisp_zerop,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("!=",lisp_numne,2,0,0,0,2,f2,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("*",lisp_mul_driver,1,0,0,1,2,fmany,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("+",lisp_add_driver,1,0,0,1,2,fmany,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("1+",lisp_inc,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("-",lisp_sub_driver,1,0,0,1,2,fmany,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("1-",lisp_dec,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("/",lisp_div_driver,1,0,0,1,2,fmany,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("<",lisp_numlt,2,0,0,0,2,f2,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("<=",lisp_numle,2,0,0,0,2,f2,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("=",lisp_numeq,2,0,0,0,2,f2,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR(">",lisp_numgt,2,0,0,0,2,f2,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR(">=",lisp_numge,2,0,0,0,2,f2,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("abs",lisp_abs,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("cos",lisp_cos,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("exp",lisp_exp,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("expt",lisp_pow,2,0,0,0,2,f2,"(num1 num2)",subr_compiled);
PRIM_DEFSUBR("log",lisp_log,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("mod",lisp_mod,2,0,0,0,2,f2,"()",subr_compiled);
PRIM_DEFSUBR("pow",lisp_pow_driver,1,0,0,1,2,fmany,"()",subr_compiled);
PRIM_DEFSUBR("sin",lisp_sin,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("tan",lisp_tan,1,0,0,0,1,f1,"(number)",subr_compiled);
PRIM_DEFSUBR("assoc",assoc,2,0,0,0,2,f2,"(key list Skey list)",subr_compiled);
PRIM_DEFSUBR("assq",assq,2,0,0,0,2,f2,"(key list Skey list)",subr_compiled);
PRIM_DEFSUBR("cons",Fcons,2,0,0,0,2,f2,"(car cdr)",subr_compiled);
PRIM_DEFSUBR("drop",cons_drop,2,0,0,0,2,f2,"(list n)",subr_compiled);
PRIM_DEFSUBR("last",last,1,0,0,0,1,f1,"(list)",subr_compiled);
PRIM_DEFSUBR("rand-list",rand_list,1,1,0,0,2,f2,"()",subr_compiled);
PRIM_DEFSUBR("push!",push_cons,2,0,0,0,2,f2,"(new-val place)",subr_compiled);
PRIM_DEFSUBR("reduce",cons_reduce,2,1,0,0,3,f3,"(seq function)",subr_compiled);
PRIM_DEFSUBR("reverse!",cons_nreverse,1,0,0,0,1,f1,"(seq)",subr_compiled);
PRIM_DEFSUBR("reverse",cons_reverse,1,0,0,0,1,f1,"(seq)",subr_compiled);
PRIM_DEFSUBR("set-car!",set_car,2,0,0,0,2,f2,"(cell new-val)",subr_compiled);
PRIM_DEFSUBR("set-cdr!",set_cdr,2,0,0,0,2,f2,"(cell new-val)",subr_compiled);
PRIM_DEFSUBR("split",cons_split,1,1,0,0,2,f2,"()",subr_compiled);
PRIM_DEFSUBR("take",cons_take,2,0,0,0,2,f2,"(list n)",subr_compiled);
MAKE_SYMBOL(Sarrayp,"array?",6,0x104d4a9bad9d3a7b ,const_subr_sexp(&lisp_arrayp_subr),{0},2);
MAKE_SYMBOL(Sconsp,"cons?",5,0x65c92c8e19e369f5 ,const_subr_sexp(&lisp_consp_subr),{0},2);
MAKE_SYMBOL(Snumberp,"number?",7,0xcb4c6ddc0c92cbed ,const_subr_sexp(&lisp_numberp_subr),{0},2);
MAKE_SYMBOL(Sintegerp,"integer?",8,0x7e2a1481098e008e ,const_subr_sexp(&lisp_integerp_subr),{0},2);
MAKE_SYMBOL(Sstringp,"string?",7,0x80eb77b1f909e305 ,const_subr_sexp(&lisp_stringp_subr),{0},2);
MAKE_SYMBOL(Sstreamp,"stream?",7,0x81f4ec932d7ed70e ,const_subr_sexp(&lisp_streamp_subr),{0},2);
MAKE_SYMBOL(Ssequencep,"sequence?",9,0x3375f288d393f895 ,const_subr_sexp(&lisp_sequencep_subr),{0},2);
MAKE_SYMBOL(Srealp,"real?",5,0x895078f30609cc46 ,const_subr_sexp(&lisp_realp_subr),{0},2);
MAKE_SYMBOL(Sbignump,"bignum?",7,0x9d48773d512c7998 ,const_subr_sexp(&lisp_bignump_subr),{0},2);
MAKE_SYMBOL(Sbigintp,"bigint?",7,0xeb60f8074f23206f ,const_subr_sexp(&lisp_bigintp_subr),{0},2);
MAKE_SYMBOL(Sbigfloatp,"bigfloat?",9,0x439ce8cbd75405fa ,const_subr_sexp(&lisp_bigfloatp_subr),{0},2);
MAKE_SYMBOL(Shashtablep,"hashtable?",10,0x2627a63f6846f3fc ,const_subr_sexp(&lisp_hashtablep_subr),{0},2);
MAKE_SYMBOL(Seq,"eq",2,0x088e3b07b5394bc3 ,const_subr_sexp(&lisp_eq_subr),{0},2);
MAKE_SYMBOL(Seql,"eql",3,0xc2f9fd18f05b9a5d ,const_subr_sexp(&lisp_eql_subr),{0},2);
MAKE_SYMBOL(Sequal,"equal",5,0x6a7933d70f43faaf ,const_subr_sexp(&lisp_equal_subr),{0},2);
MAKE_SYMBOL(Sevenp,"even?",5,0x112b550079b0072a ,const_subr_sexp(&lisp_evenp_subr),{0},2);
MAKE_SYMBOL(Soddp,"odd?",4,0x5d95b2b49bd10bf3 ,const_subr_sexp(&lisp_oddp_subr),{0},2);
MAKE_SYMBOL(Szerop,"zero?",5,0x6dd9993119363874 ,const_subr_sexp(&lisp_zerop_subr),{0},2);
MAKE_SYMBOL(Sne,"!=",2,0x07c28707b48c6683 ,const_subr_sexp(&lisp_numne_subr),{0},2);
MAKE_SYMBOL(Smul,"*",1,0xaf63a74c8601927d ,const_subr_sexp(&lisp_mul_driver_subr),{0},2);
MAKE_SYMBOL(Sadd,"+",1,0xaf63a64c860190ca ,const_subr_sexp(&lisp_add_driver_subr),{0},2);
MAKE_SYMBOL(Sinc,"1+",2,0x07f89d07b4ba1b55 ,const_subr_sexp(&lisp_inc_subr),{0},2);
MAKE_SYMBOL(Ssub,"-",1,0xaf63a04c86018698 ,const_subr_sexp(&lisp_sub_driver_subr),{0},2);
MAKE_SYMBOL(Sdec,"1-",2,0x07f89707b4ba1123 ,const_subr_sexp(&lisp_dec_subr),{0},2);
MAKE_SYMBOL(Sdiv,"/",1,0xaf63a24c860189fe ,const_subr_sexp(&lisp_div_driver_subr),{0},2);
MAKE_SYMBOL(Slt,"<",1,0xaf63b14c8601a37b ,const_subr_sexp(&lisp_numlt_subr),{0},2);
MAKE_SYMBOL(Sle,"<=",2,0x08098b07b4c86ff2 ,const_subr_sexp(&lisp_numle_subr),{0},2);
MAKE_SYMBOL(Snum_eq,"=",1,0xaf63b04c8601a1c8 ,const_subr_sexp(&lisp_numeq_subr),{0},2);
MAKE_SYMBOL(Sgt,">",1,0xaf63b34c8601a6e1 ,const_subr_sexp(&lisp_numgt_subr),{0},2);
MAKE_SYMBOL(Sge,">=",2,0x08108707b4ce87d4 ,const_subr_sexp(&lisp_numge_subr),{0},2);
MAKE_SYMBOL(Sabs,"abs",3,0xe71fb2190541727b ,const_subr_sexp(&lisp_abs_subr),{0},2);
MAKE_SYMBOL(Scos,"cos",3,0xf604fc190d0176dc ,const_subr_sexp(&lisp_cos_subr),{0},2);
MAKE_SYMBOL(Sexp,"exp",3,0xc3120b18f0704408 ,const_subr_sexp(&lisp_exp_subr),{0},2);
MAKE_SYMBOL(Sexpt,"expt",4,0xe7ed57608ec45eb4 ,const_subr_sexp(&lisp_pow_subr),{0},2);
MAKE_SYMBOL(Slog,"log",3,0x125073191daf5431 ,const_subr_sexp(&lisp_log_subr),{0},2);
MAKE_SYMBOL(Smod,"mod",3,0x0808591917670ee3 ,const_subr_sexp(&lisp_mod_subr),{0},2);
MAKE_SYMBOL(Spow,"pow",3,0x779b5f19564f3b35 ,const_subr_sexp(&lisp_pow_driver_subr),{0},2);
MAKE_SYMBOL(Ssin,"sin",3,0x8248a1195cecc4ad ,const_subr_sexp(&lisp_sin_subr),{0},2);
MAKE_SYMBOL(Stan,"tan",3,0x56d7b2194448b0d8 ,const_subr_sexp(&lisp_tan_subr),{0},2);
MAKE_SYMBOL(Sassoc,"assoc",5,0xc91983bcc1eb5d84 ,const_subr_sexp(&assoc_subr),{0},2);
MAKE_SYMBOL(Sassq,"assq",4,0x906f6384425bb111 ,const_subr_sexp(&assq_subr),{0},2);
MAKE_SYMBOL(Scons,"cons",4,0x0bca0591195d8188 ,const_subr_sexp(&Fcons_subr),{0},2);
MAKE_SYMBOL(Sdrop,"drop",4,0xf1a2a766dcd55bac ,const_subr_sexp(&cons_drop_subr),{0},2);
MAKE_SYMBOL(Slast,"last",4,0x0456d2ad905847d9 ,const_subr_sexp(&last_subr),{0},2);
MAKE_SYMBOL(Srand_list,"rand-list",9,0x7973ad3c0d23670d ,const_subr_sexp(&rand_list_subr),{0},2);
MAKE_SYMBOL(Spush,"push!",5,0xbfcaed0cebfc7e74 ,const_subr_sexp(&push_cons_subr),{0},2);
MAKE_SYMBOL(Sreduce,"reduce",6,0x2f92df0bac03dce7 ,const_subr_sexp(&cons_reduce_subr),{0},2);
MAKE_SYMBOL(Snreverse,"reverse!",8,0x89b26d029f5c048c ,const_subr_sexp(&cons_nreverse_subr),{0},2);
MAKE_SYMBOL(Sreverse,"reverse",7,0xc95b3c1f3263ab65 ,const_subr_sexp(&cons_reverse_subr),{0},2);
MAKE_SYMBOL(Sset_car,"set-car!",8,0x6899af1d2ce1faff ,const_subr_sexp(&set_car_subr),{0},2);
MAKE_SYMBOL(Sset_cdr,"set-cdr!",8,0x93f6ac1d45757d94 ,const_subr_sexp(&set_cdr_subr),{0},2);
MAKE_SYMBOL(Ssplit,"split",5,0x5fdb7a8ac3147783 ,const_subr_sexp(&cons_split_subr),{0},2);
MAKE_SYMBOL(Stake,"take",4,0xd94551ef0792aff6 ,const_subr_sexp(&cons_take_subr),{0},2);
/*Globals*/
MAKE_SYMBOL(lisp_stdin,"stdin",5,0x07ce0cad53eda57d ,STDIN_FILENO,{0},0);
MAKE_SYMBOL(lisp_stdout,"stdout",6,0x42e6d785a74f8c66 ,STDOUT_FILENO,{0},0);
MAKE_SYMBOL(lisp_stderr,"stderr",6,0x104ce5858b0a80b5 ,STDERR_FILENO,{0},0);
/*Keywords*/
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
/*Special Forms*/
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
MAKE_SELF_QUOTING_SYMBOL(Qquasiquote,"quasiquote",10,0xc04623432a914336 ,{0});
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
c_intern_unsafe(global_obarray,Qquasiquote);
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
c_intern_unsafe(global_obarray,Einternal);
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
