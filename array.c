/*****************************************************************
 * Copyright (C) 2013 Tucker DiNapoli                            *
 * SciLisp is Licensed under the GNU General Public License V3   *
 ****************************************************************/
#include "array.h"
#include "prim.h"
sexp aref(sexp obj,sexp ind){
  if(!ARRAYP(obj)){
    return error_sexp("aref type error");
  } else if(ind.val.int64 > obj.len || ind.val.int64<0){
      return error_sexp("aref bounds error");
  } else {
    return XAREF(obj,ind.val.int64);
  }
}
/*#define iota_loop(sign,cmp,type,tag)         \
      for(i=0;i cmp imax;i sign##sign){        \
        newarray[sign i].tag=(type)i;          \
        }*/
sexp array_iota(sexp start,sexp stop,sexp step,sexp should_round){
  int i=0,k;
  double j,dstep;
  if(!NUMBERP(start)){goto TYPE_ERROR;}
  if(NILP(stop)){
    stop=start;
    start=long_sexp(0);
  } else if(!NUMBERP(stop)){goto TYPE_ERROR;}
  double range=getDoubleValUnsafe(lisp_sub(stop,start));
  if(NILP(step)){
    step=(range < 0)?long_sexp(-1):long_sexp(1);
  } else if(!NUMBERP(step)){goto TYPE_ERROR;}
  dstep=getDoubleValUnsafe(step);
  int imax=ceil(fabs(range/dstep));
  data* newarray=xmalloc(sizeof(data)*imax+1);
  j=getDoubleValUnsafe(start);
  int rnd=!NILP(should_round);
  for(i=0;i<imax;i++){
    if(rnd){
      newarray[i].int64=round(j);
    } else {
      newarray[i].real64=j;
    }
    j+=dstep;
  }
  if(rnd){
    return (sexp){.tag=_array,.meta=_long_array,.val={.array=newarray},.len=imax};
  } else {
    return (sexp){.tag=_array,.meta=_double_array,.val={.array=newarray},.len=imax};
  }
  TYPE_ERROR:
    return error_sexp("iota type error, arguments must be numbers");
}
/*sexp array_iota(sexp start,sexp stop,sexp step){
  long i,imax;
  double dstep;
  data* newarray;
  if(!NUMBERP(start)){
    return error_sexp("iota type error, arguments must be numbers");
  }
  if(NILP(stop)){//only got one argument,so use that as stop
    if(FLOATP(start)){
      double fstop=AS_DOUBLE(start);
      long sign=(signbit(fstop));
      imax=lround(fstop);
      newarray=xmalloc(sizeof(data)*(abs(imax)));
      if(sign){
        iota_loop(-,>,double,real64);
      } else {        
        iota_loop(+,<,double,real64);
      }
      return (sexp){.tag=_array,.val={.array=newarray},
          .len=abs(imax),.meta=_double_array};      
    } else {
      long lstop=AS_LONG(start);
      long sign=AS_LONG(start) & 0x8;
      imax=lstop;
      newarray=xmalloc(sizeof(data)*abs(imax));
      if(sign){
        iota_loop(-,>,long,int64);
      } else {
        iota_loop(+,<,long,int64);
      }
      return (sexp){.tag=_array,.val={.array=newarray},
          .len=abs(imax),.meta=_long_array};
    }
  } else if(!NUMBERP(stop)){
    return error_sexp("iota type error, arguments must be numbers");
  } else {
    imax=ceil(fabs(getDoubleVal(lisp_sub(stop,start))/dstep));
    long sign=signbit(getDoubleVal(lisp_sub(stop,start))/dstep);
    if(NILP(step)){
      dstep=(sign?-1.0:1.0);
    } else {
      if(!NUMBERP(step)){
        return error_sexp("iota type error, arguments must be numbers");
      }
      dstep=getDoubleVal(step);
    }
    newarray=xmalloc(sizeof(data)*imax);
    double j=getDoubleVal(start);
    for(i=0;i<imax;i++){
      newarray[i].real64=j;
      j+=dstep;
    }
    return (sexp){.tag=_array,.val={.array=newarray},
        .len=imax,.meta=_double_array};
  }
  }    */
