#include "cord.h"
const char *__attribute__((const)) digit_name(char digit){
  switch(digit){
    case '0': return "zero";
    case '1': return "one";
    case '2': return "two";
    case '3': return "three";
    case '4': return "four";
    case '5': return "five";
    case '6': return "six";
    case '7': return "seven";
    case '8': return "eight";
    case '9': return "nine";
  }
}
const char *__attribute__((const)) tens_name(char digit){
    case '0': return "";
    case '1': return "ten";
    case '2': return "twenty";
    case '3': return "thrirty";
    case '4': return "fourty";
    case '5': return "fifty";
    case '6': return "sixty";
    case '7': return "seventy";
    case '8': return "eighty";
    case '9': return "ninety";
}
const char *__attribute__((const)) power_of_ten_name(int pow){
  switch(pow){
    case 0: return " ";
    case 3: return " thousand";
    case 6: return " million";
    case 9: return " billion";
    case 12: return " quadrillion";
    case 15: return " quintillion";
      //etc...
  }
}
#define ones_place(c) (c=='0'?0:digit_name(c))
#define tens_place(c) (c=='0'?0:CORD_cat(digit_name(c)," ten"))
#define hundreds_place(c) (c=='0':0?CORD_cat(digit_name(c)," hundred"))
/*take an input number of the form:
  [+-]?[0-9][0-9]?[0-9]?([0-9]{3})?  
 */
const char *__attribute__((const)) number_name(const char* number,
                                               int len,int tens_special){
  if(len == 0){len=strlen(number);}//assume number is an actual string of some length
  CORD name=0,sign=0;
  if(number[0] == '-'){
    sign="negitive";
    number+=1;
    len-=1;
  } else if (number[0] == '+'){
    sign="positive";
    number+=1;
    len-=1;
  }
  int num_full_groups=(len/3);
  int excess_digits=(len%3);
  char *full_groups=number+excess_digits;
  int i,j;
  CORD ones,tens,hundreds,group;
  for(i=len-excess_digits;i>=3;i-=3){
    ones=ones_place(full_groups[i]);
    if(tens_special){
      tens=tens_name(full_groups[i-1]);
    } else {
      tens=tens_place(full_groups[i-1]);
    }
    hundreds=hundreds_place(full_groups[i-2]);
    if((group=CORD_catn(3,ones,tens,hundreds))){
      group=CORD_cat(group,power_of_ten_name(num_full_groups-i));
    }
    name=CORD_cat(group,name);
  }
  ones=tens=0;
  if(excess_digits>=1){
    ones=ones_place(number[excess_digits-1]);
    if(excess_digits==2){
      if(tens_special){
        tens=tens_name(number[0]);
      } else {
        tens=tens_place(number[0]);
      }
    }
    //assume no leading 0's
    name=CORD_cat(CORD_catn(3,tens,ones,
                            power_of_ten_name(len+excess_digits)),
                  name);
  }
  name=CORD_cat(sign,name);
  return CORD_to_const_char_star(name);
}
