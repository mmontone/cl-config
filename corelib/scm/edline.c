/* "readline.c" Scheme interface to readline library
 * Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this program.  If not, see
 * <http://www.gnu.org/licenses/>.
 */

/* Author: Radey Shouman */

#include "scm.h"

char *readline   P((const char *prompt));
void add_history P((char *p));

             /* Reads on stdin/stdout only */
static char s_readline[] = "read-edited-line";
SCM lreadline(prompt)
     SCM prompt;
{
  SCM res;
  char *s;
  ASRTER(NIMP(prompt) && STRINGP(prompt), prompt, ARG1, s_readline);
  s = readline(CHARS(prompt));
  if (NULL == s) return EOF_VAL;
  NEWCELL(res);
  DEFER_INTS;
  SETCHARS(res, s);
  SETLENGTH(res, strlen(s), tc7_string);
  ALLOW_INTS;
  return res;
}
static char s_add_history[] = "add-history";
SCM ladd_history(line)
     SCM line;
{
  ASRTER(NIMP(line) && STRINGP(line), line, ARG1, s_add_history);
  add_history(CHARS(line));
  return UNSPECIFIED;
}
static char s_def_inport[] = "default-input-port";
SCM def_inport()
{
  return def_inp;
}
static char s_def_outport[] = "default-output-port";
SCM def_outport()
{
  return def_outp;
}
static char s_Iedline[] = "Iedline.scm";
void init_edline()
{
  make_subr(s_def_inport, tc7_subr_0, def_inport);
  make_subr(s_def_outport, tc7_subr_0, def_outport);
  make_subr(s_readline, tc7_subr_1, lreadline);
  make_subr(s_add_history, tc7_subr_1, ladd_history);
  if (scm_ldprog(s_Iedline))
    wta(*loc_errobj, "couldn't init", s_Iedline);
}
