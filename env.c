#include "common.h"
inline sexp lookupSym(env* cur_env,CORD name){
  if(cur_env.tag==_hash){
    symref tempsym;
    getSym(name,tempsym);
  } else {
    getSymLocal(cur_env,name);
  }
}    
sexp getSymlocal(local_env cur_env,CORD name){
  local_symbol* local_symref=cur_env.head;
  while(local_symref != NULL){
    if(CORD_cmp(local_symref->name,name)){
      return local_symref->val;      
    }
    local_symref=local_symref->next;
  }
  lookupSym(cur_env.enclosing,name);
}
