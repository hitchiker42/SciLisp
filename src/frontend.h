/* Constants for the frontend, i.e help string, version info etc...

   Copyright (C) 2013-2014 Tucker DiNapoli

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
#ifndef _FRONTEND_H_
#define _FRONTEND_H_
#include "common.h"
static void SciLisp_help(int exitCode) __attribute__((noreturn));
static void SciLisp_version(int exitCode) __attribute__((noreturn));
static const char *banner=
  "SciLisp  Copyright (C) 2013-2014 Tucker DiNapoli\n"
  "SciLisp is free software licensed under the GNU GPL V3+";
static const char *SciLisp_Banner;
static int no_banner=0;
static int no_copyright=0;
/*just to note I didn't write this I got it from
  http://patorjk.com/software/taag/#p=display&f=Small%20Slant&t=SciLisp*/
static c_string SciLisp_Banner=
"    ____      _  __    _          \n"
"   / __/____ (_)/ /   (_)___  ___ \n"
"  _\\ \\ / __// // /__ / /(_-< / _ \\\n"
" /___/ \\__//_//____//_//___// .__/\n"
"                           /_/     ";
static void SciLisp_help(int exitCode){
  CORD_printf(Make_SciLisp_help_string());
  exit(exitCode);
}
static void SciLisp_version(int exitCode){
  puts(Make_SciLisp_verson_string(PACKAGE_VERSION));
  exit(exitCode);
}
static CORD Make_SciLisp_verson_string(c_string Version_no){
  CORD version_string;
  CORD_sprintf(&version_string,"SciLisp %s",PACKAGE_VERSION);
  return version_string;
}
static CORD Make_SciLisp_Copyright_string(){
  CORD retval;
  CORD_sprintf(&retval,"Copyright %lc 2013 Tucker DiNapoli\n"
               "License GPLv3+: GNU GPL version 3 or "
               "later <http://gnu.org/licenses/gpl.html>.",0x00A9);
  return retval;
}
static CORD Make_SciLisp_help_string(){
  CORD copyright_string=Make_SciLisp_Copyright_string();
  CORD version_string=Make_SciLisp_verson_string(PACKAGE_VERSION);
  CORD help_string=CORD_catn(4,version_string," ",copyright_string,"\n");
  help_string=CORD_cat
    (help_string,
     "SciLisp [-hqv] [-e|--eval] [-f|--file] [-l|--load] [-o|--output] [file]\n"
     "Options:\n"
     "eval|e [file|string], evaluate code in file or double quote delimited string\n"
     "help|h, print this help and exit\n"
     "load|l [file], eval code from file and start interpreter\n"
     "output|o [file], output compiled code to file, requires a file to compile\n"
     "test|t, run tests contatined in test.lisp and print results\n"
     "version|v, print version number and exit\n");
     // "quiet|q, insure debug messages are not printed (redirect to /dev/null)\n");
  return help_string;
}
int parens_matched(const char* line,int parens)__attribute__((pure));
int lisp_getline(FILE* outfile,char* filename);
void read_eval_print_loop();
#ifdef HAVE_READLINE
int lisp_readline(FILE* outfile,char* filename);
static int(*lisp_readline_ptr)(FILE*,char*)=lisp_readline;
#include <readline/readline.h>
#include <readline/history.h>
#else
static int(*lisp_readline_ptr)(FILE*,char*)=lisp_getline;
#endif
#endif
