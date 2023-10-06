/* "sc2.c" R2RS and R3RS procedures not in R4RS.
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
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

/* Author: Aubrey Jaffer */

#include "scm.h"

static char s_last_pair[] = "last-pair";
SCM last_pair(sx)
     SCM sx;
{
  register SCM res = sx;
  register SCM x;
  ASRTER(NIMP(res) && CONSP(res), res, ARG1, s_last_pair);
  while (!0) {
    x = CDR(res);
    if (IMP(x) || NCONSP(x)) return res;
    res = x;
    x = CDR(res);
    if (IMP(x) || NCONSP(x)) return res;
    res = x;
    sx = CDR(sx);
    ASRTER(x != sx, sx, ARG1, s_last_pair);
  }
}

static char s_subml[] = "substring-move-left!";
SCM subml(str1, start1, args)
     SCM str1, start1, args;
{
  SCM end1, str2, start2;
  long i, j, e;
  ASRTER(3==ilength(args), args, WNA, s_subml);
  end1 = CAR(args); args = CDR(args);
  str2 = CAR(args); args = CDR(args);
  start2 = CAR(args);
  ASRTER(NIMP(str1) && STRINGP(str1), str1, ARG1, s_subml);
  ASRTER(INUMP(start1), start1, ARG2, s_subml);
  ASRTER(INUMP(end1), end1, ARG3, s_subml);
  ASRTER(NIMP(str2) && STRINGP(str2), str2, ARG4, s_subml);
  ASRTER(INUMP(start2), start2, ARG5, s_subml);
  i = INUM(start1), j = INUM(start2), e = INUM(end1);
  ASRTER(i <= LENGTH(str1) && i >= 0, start1, OUTOFRANGE, s_subml);
  ASRTER(j <= LENGTH(str2) && j >= 0, start2, OUTOFRANGE, s_subml);
  ASRTER(e <= LENGTH(str1) && e >= 0, end1, OUTOFRANGE, s_subml);
  ASRTER(e-i+j <= LENGTH(str2), start2, OUTOFRANGE, s_subml);
  while(i<e) CHARS(str2)[j++] = CHARS(str1)[i++];
  return UNSPECIFIED;
}
static char s_submr[] = "substring-move-right!";
SCM submr(str1, start1, args)
     SCM str1, start1, args;
{
  SCM end1, str2, start2;
  long i, j, e;
  ASRTER(3==ilength(args), args, WNA, s_submr);
  end1 = CAR(args); args = CDR(args);
  str2 = CAR(args); args = CDR(args);
  start2 = CAR(args);
  ASRTER(NIMP(str1) && STRINGP(str1), str1, ARG1, s_submr);
  ASRTER(INUMP(start1), start1, ARG2, s_submr);
  ASRTER(INUMP(end1), end1, ARG3, s_submr);
  ASRTER(NIMP(str2) && STRINGP(str2), str2, ARG4, s_submr);
  ASRTER(INUMP(start2), start2, ARG5, s_submr);
  i = INUM(start1), j = INUM(start2), e = INUM(end1);
  ASRTER(i <= LENGTH(str1) && i >= 0, start1, OUTOFRANGE, s_submr);
  ASRTER(j <= LENGTH(str2) && j >= 0, start2, OUTOFRANGE, s_submr);
  ASRTER(e <= LENGTH(str1) && e >= 0, end1, OUTOFRANGE, s_submr);
  ASRTER((j = e-i+j) <= LENGTH(str2), start2, OUTOFRANGE, s_submr);
  while(i<e) CHARS(str2)[--j] = CHARS(str1)[--e];
  return UNSPECIFIED;
}
static char s_subfl[] = "substring-fill!";
SCM subfl(str, start, args)
     SCM str, start, args;
{
  SCM end, fill;
  long i, e;
  char c;
  ASRTER(2==ilength(args), args, WNA, s_subfl);
  end = CAR(args); args = CDR(args);
  fill = CAR(args);
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_subfl);
  ASRTER(INUMP(start), start, ARG2, s_subfl);
  ASRTER(INUMP(end), end, ARG3, s_subfl);
  ASRTER(ICHRP(fill), fill, ARG4, s_subfl);
  i = INUM(start), e = INUM(end);c = ICHR(fill);
  ASRTER(i <= LENGTH(str) && i >= 0, start, OUTOFRANGE, s_subfl);
  ASRTER(e <= LENGTH(str) && e >= 0, end, OUTOFRANGE, s_subfl);
  while(i<e) CHARS(str)[i++] = c;
  return UNSPECIFIED;
}

static char s_strnullp[] = "string-null?";
SCM strnullp(str)
     SCM str;
{
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_strnullp);
  if (LENGTH(str)) return BOOL_F;
  else return BOOL_T;
}

static char s_appendb[] = "append!";
SCM appendb(args)
     SCM args;
{
  SCM arg;
 tail:
  if (NULLP(args)) return EOL;
  arg = CAR(args);
  args = CDR(args);
  if (NULLP(args)) return arg;
  if (NULLP(arg)) goto tail;
  ASRTER(NIMP(arg) && CONSP(arg), arg, ARG1, s_appendb);
  CDR(last_pair(arg)) = appendb(args);
  return arg;
}

static iproc lsubr2s[] = {
  {s_subml, subml},
  {s_submr, submr},
  {s_subfl, subfl},
  {0, 0}};

SCM_DLL_EXPORT void init_sc2 P((void));

void init_sc2()
{
  make_subr(s_last_pair, tc7_subr_1, last_pair);
  make_subr(s_strnullp, tc7_subr_1, strnullp);
  make_subr(s_appendb, tc7_lsubr, appendb);
  init_iprocs(lsubr2s, tc7_lsubr_2);
  add_feature("rev2-procedures");
  add_feature("rev3-procedures");
}
