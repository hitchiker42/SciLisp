char *hex_digit_to_binary[16]={"0000","0001","0010","0011","0100","0101",
                               "0110","0111","1000","1001","1010","1011",
                               "1100","1101","1110"};

//assume this only gets called from format/printf so
//that bits is determined by the format spec, and
//so we know it's either 8,16,32 or 64
char *format_binary(uint64_t num,int bits){
  char *retval=xmalloc_atomic(bits);
  if(num==0){
    int i;
    for(i=0;i<bits;i++){
      retval[i]='0';
    }
    return retval;
  }
}
