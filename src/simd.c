/*
  simd floating point map-reduce, with sequential reduce
  xmm1 accumulator;
  xmm2 working_register
  
  if we have and initial value
    load it into xmm1
  else 
    load the first value in array into xmm1
    set array to array+1
  endif
  int width=N,index,i;//where N=2/4/8 depending on floatingpt width & vector width
  for(index=0;index<length;index+=width){
    load(array,xmm2);//load should only use the next width elements
    perform map operation on xmm2
    for(i=0;i<with;i++){
    perform reduce operation on xmm1,xmm2;
    shift xmm1 right by 32/64 bits depending;
    }
    shift xmm1 left by width-(32/64 bits);
   }
   //for excess elements
   load single element into xmm2
   perform sigular version of reduce op on xmm2,xmm1

   zero out leftmost bits of xmm1 before returning;

   core loop is effectively(assuming 4x32bit elements)
   
      op xmm2 # whatever op is used for mapping
      mov $4,rax;
LABEL op2 xmm2,xmm1;
      psrldq $4,xmm1;
      dec rax;
      jnz LABEL

 */
