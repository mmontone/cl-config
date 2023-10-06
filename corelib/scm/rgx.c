/* "rgx.c" regular expression matching using C regex library.
 * Copyright (C) 1995, 1996, 1997 Free Software Foundation, Inc.
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
#ifdef __FreeBSD__
# include "gnuregex.h"
#else
# include "regex.h"
#endif
#include <stdio.h>
/* added by Denys Duchier: for bcopy */
#ifdef sun
#include <strings.h>
#endif

static char rcsid[] =
   "$Id: rgx.c,v 1.20 2013/03/14 04:42:23 jaffer Exp $";

#ifdef HAVE_ALLOCA
# include <alloca.h>
# define ALLOCA_PROTECT typedef int foobazzz
# define ALLOCA		alloca
#else
# define ALLOCA_PROTECT SCM alloca_protect=EOL
# define ALLOCA(size)							\
	(alloca_protect=cons(makstr((long)(size)), alloca_protect),	\
	 (void *)CDR(CAR(alloca_protect)))

#endif

#ifdef _GNU_SOURCE
/* following two lines stolen from GNU regex.c */
# define CHAR_SET_SIZE 256
# define ISUPPER(c) (isascii (c) && isupper (c))
#endif

/* forward function defs */

SCM lregsearch();
SCM lregsearchv();

/* Posix regexp bindings.  */

static char s_regex[] = "regex";
static char s_regcomp[] = "regcomp", s_regerror[] = "regerror";
static char s_regexec[] = "regexec", s_regmatp[] = "regmatch?";
static char s_regsearch[] = "regsearch", s_regmatch[] = "regmatch";
static char s_regsearchv[] = "regsearchv", s_regmatchv[] = "regmatchv";
static char s_stringsplit[]  = "string-split";
static char s_stringsplitv[] = "string-splitv";
static char s_stringedit[] = "string-edit";

#define s_error &s_regerror[3]

#define RGX_INFO(obj) ((regex_info*)CDR(obj))
#define RGX_PATTERN(obj) (((regex_info*)CDR(obj))->pattern)
#define RGX(obj) (&((regex_info*)CDR(obj))->rgx)
#ifndef _GNU_SOURCE
# define RGX2(obj) (&((regex_info*)CDR(obj))->rgx_anchored)
#endif

#define FIXUP_REGEXP(prog)						\
{									\
    if (STRINGP(prog))							\
	prog = lregcomp(prog, UNDEFINED);				\
    if (NIMP(prog) && CONSP(prog) && STRINGP(CAR(prog)) &&		\
	NIMP(CDR(prog)) && CONSP(CDR(prog)) && STRINGP(CAR(CDR(prog)))) \
	prog = lregcomp(CAR(prog), CAR(CDR(prog)));			\
}

typedef struct regex_info {
    SCM pattern;    /* string we compiled to create our reg exp */
    regex_t rgx;
#ifndef _GNU_SOURCE
    int options;    /* for anchored pattern when matching */
    regex_t rgx_anchored;
#endif
} regex_info;

sizet fregex(ptr)
     CELLPTR ptr;
{
  regfree(RGX((SCM)ptr));
#ifndef _GNU_SOURCE
  /* options are null => we compiled the anchored pattern */
  if (RGX_INFO((SCM)ptr)->options==0)
    regfree(RGX2((SCM)ptr));
#endif
  must_free(CHARS((SCM)ptr), (sizet)LENGTH((SCM)ptr));
  return 0;
}

int prinregex(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  lputs("#<regex ", port);
  scm_intprint(CDR(exp), -16, port);
  lputc(' ', port);
  scm_iprin1(RGX_PATTERN(exp), port, writing);
  lputc('>', port);
  return 1;
}

SCM markregex(ptr)
     SCM ptr;
{
  SETGC8MARK(RGX_PATTERN(ptr));
  return BOOL_F;
}

int tc16_rgx;
static smobfuns rgxsmob = {markregex, fregex, prinregex};

SCM lregerror(scode)
     SCM scode;
{
  int code;
  /* added by Denys Duchier: conditional declaration */
#ifdef __REGEXP_LIBRARY_H__
  int len;
#endif
  SCM str;
  ASRTER(INUMP(scode), scode, ARG1, s_regerror);
  code = INUM(scode);
  if (code < 0)
    return makfromstr("Invalid code", sizeof("Invalid code")-1);
  /* XXX - is regerror posix or not? */
#ifdef __REGEXP_LIBRARY_H__
  /* XXX - gnu regexp doesn't use the re parameter, so we will
     ignore it in a very untidy way. */
  len = regerror(code, 0L, 0L, 0);
  str = makstr(len-1);
  regerror(code, 0L, CHARS(str), len);
#else
  str = makfromstr(s_error, (sizet)5);
#endif
  return str;
}

SCM lregcomp(pattern, flags)
     SCM pattern, flags;
{
  SCM z;
  int i, options;
  regex_t *prog;
  regex_info *info;
  char *flagchars;
#ifdef _GNU_SOURCE
  int fastmap = 0;
  int ignore_case = 0;
  char *err_msg;
#endif

  ASRTER(NIMP(pattern) && STRINGP(pattern), pattern, ARG1, s_regcomp);
  ASRTER(UNBNDP(flags) || (NIMP(flags) && STRINGP(flags)),
	 flags, ARG2, s_regcomp);
  DEFER_INTS;
  z = must_malloc_cell((long)sizeof(regex_info), (SCM)tc16_rgx, s_regex);
  scm_protect_temp(&z);
  info=(regex_info*)CHARS(z);
  prog = &(info->rgx);
#ifdef __REGEXP_LIBRARY_H__
  for (i=sizeof(regex_t);i--;((char *)prog)[i] = 0);
# ifndef _GNU_SOURCE
  {
    regex_t *prog2;
    prog2 = &(info->rgx_anchored);
    for (i=sizeof(regex_t);i--;((char *)prog2)[i] = 0);
  }
# endif
#endif

  ALLOW_INTS;
  info->pattern = pattern;

#ifdef _GNU_SOURCE
  options = RE_SYNTAX_POSIX_EXTENDED;
#else
  options = REG_EXTENDED;
#endif

  if (!UNBNDP(flags)) {
    flagchars = CHARS(flags);
    for (i=0; i<LENGTH(flags); i++)
      switch (flagchars[i]) {
#ifdef _GNU_SOURCE
      case 'n':
	options |= RE_HAT_LISTS_NOT_NEWLINE;
	options &= (~RE_DOT_NEWLINE);
	break;
      case 'i':
	ignore_case = 1;
	break;
      case '0':
	options &= (~RE_DOT_NOT_NULL);
	break;
      case 'f':
	fastmap = 1;
	break;
#else
      case 'n':
	options |= REG_NEWLINE;
	break;
      case 'i':
	options |= REG_ICASE;
	break;
#endif
      }
  }

#ifdef _GNU_SOURCE
  DEFER_INTS;
  if (fastmap)
    prog->fastmap = must_malloc(CHAR_SET_SIZE, s_regex);
  else
    prog->fastmap = 0;

  if (ignore_case) {
    prog->translate = must_malloc(CHAR_SET_SIZE, s_regex);
    for (i = 0; i < CHAR_SET_SIZE; i++)
      prog->translate[i] = ISUPPER (i) ? tolower (i) : i;
  }
  else
    prog->translate = 0;

  prog->buffer = 0;
  prog->allocated = 0;

  re_set_syntax(options);
  err_msg = (char *)re_compile_pattern(CHARS(pattern), LENGTH(pattern), prog);
  ALLOW_INTS;
  prog->regs_allocated = REGS_FIXED;

  /* if error, compile using regcomp to get the error number */
  if (err_msg) {
    int i;
    char *tmppat;
    SCM protect;

    /* Fixup in case pattern has null characters */
    tmppat = CHARS(protect=makstr(LENGTH(pattern)));
    bcopy(CHARS(pattern), tmppat, LENGTH(pattern));
    for (i=0; i<LENGTH(pattern); i++)
      if (tmppat[i] == 0)
	tmppat[i] = ' ';

    i = regcomp(prog, tmppat, options);
    z = MAKINUM(i);
  }
#else
  info->options = options;
  i = regcomp(prog, CHARS(pattern), options);
  if (i) z = MAKINUM(i);
#endif
  return z;
}

SCM lregexec(prog, str)
     SCM prog, str;
{
  ALLOCA_PROTECT;

  FIXUP_REGEXP(prog);
  ASRTER(NIMP(prog) && tc16_rgx==CAR(prog), prog, ARG1, s_regexec);
  ASRTER(NIMP(str) && STRINGP(str), str, ARG2, s_regexec);

#ifdef _GNU_SOURCE
  return lregsearchv(prog, str, EOL);
#else  /* not _GNU_SOURCE */
  {
    size_t nsub;
    SCM ans;
    regmatch_t *pm;
    int flags = 0;		/* XXX - optional arg? */

    nsub = RGX(prog)->re_nsub + 1; /* XXX - is this posix? */
    pm = ALLOCA(nsub * sizeof(regmatch_t));
    if (regexec(RGX(prog), CHARS(str), nsub, pm, flags) != 0)
      ans = BOOL_F;
    else {
      ans = make_vector(MAKINUM(2L * nsub), MAKINUM(-1L));
      while (nsub--) {
	VELTS(ans)[2*nsub+0] = MAKINUM(pm[nsub].rm_so);
	VELTS(ans)[2*nsub+1] = MAKINUM(pm[nsub].rm_eo);
      }
    }
    return ans;
  }
#endif /* _GNU_SOURCE */
}

SCM lregmatp(prog, str)
     SCM prog, str;
{
  FIXUP_REGEXP(prog);
  ASRTER(NIMP(prog) && tc16_rgx==CAR(prog), prog, ARG1, s_regmatp);
  ASRTER(NIMP(str) && STRINGP(str), str, ARG2, s_regmatp);

#ifdef _GNU_SOURCE
  return (lregsearch(prog, str, EOL)==BOOL_F)?BOOL_F:BOOL_T;
#else  /* not _GNU_SOURCE */
  {
    int flags = 0;		/* XXX - optional arg? */

    flags = regexec(RGX(prog), CHARS(str), 0, 0, flags);
    if (!flags) return BOOL_T;
    if (REG_NOMATCH!=flags) wta(MAKINUM(flags), s_error, s_regmatp);
    return BOOL_F;
  }
#endif
}

#define SCALAR 0
#define VECTOR 1

#define MATCH  0
#define SEARCH 1

SCM lregsearchmatch(prog, str, args, search, vector)
     SCM prog, str, args;
     int vector, search;
{
  int len = ilength(args);
  int start, size, nsub;
  SCM matches;
  ALLOCA_PROTECT;

  FIXUP_REGEXP(prog);
  ASRTER(NIMP(prog) && tc16_rgx==CAR(prog), prog, ARG1, s_regsearch);
  ASRTER(NIMP(str) && STRINGP(str), str, ARG2, s_regsearch);
  ASRTER(len<=2, args, WNA, s_regsearch);
  ASRTER((len<1)||(INUMP(CAR(args))), CAR(args), ARG3, s_regsearch);
  ASRTER((len<2)||(INUMP(CAR(CDR(args)))), CAR(CDR(args)), ARG4, s_regsearch);

  start = (len>=1)?(INUM(CAR(args))):0;
  size  = (len>=2)?(INUM(CAR(CDR(args)))):LENGTH(str);

#ifdef _GNU_SOURCE
  {
    int ret, dir=1;
    struct re_registers regs, *pregs=0;

    if (search && start<0)
      start *= -1, dir = -1;

    if (vector) {
      pregs = &regs;
      nsub = RGX(prog)->re_nsub + 1;
      regs.num_regs = nsub;

      regs.start	  = ALLOCA(nsub * sizeof(regoff_t));
      regs.end	  = ALLOCA(nsub * sizeof(regoff_t));
    }

    if (search)
      ret = re_search(RGX(prog), CHARS(str), size, start, dir*size, pregs);
    else
      ret = re_match(RGX(prog), CHARS(str), size, start, pregs);

    if (ret < 0)
      return BOOL_F;

    if (!vector)
      return MAKINUM(ret);

    matches = make_vector(MAKINUM(2L * nsub), MAKINUM(-1L));
    while (nsub--) {
      VELTS(matches)[2*nsub+0] = MAKINUM(regs.start[nsub]);
      VELTS(matches)[2*nsub+1] = MAKINUM(regs.end[nsub]);
    }
    return matches;
  }
#else  /* not _GNU_SOURCE */
  {
    regex_t *regexp;
    regmatch_t *pm;
    char *search_string;
    if (size > LENGTH(str))
      size = LENGTH(str);

    if (start<0 || start >= size)
      return BOOL_F;

    if (size < LENGTH(str)) {
      search_string = ALLOCA(size-start+1);
      bcopy(CHARS(str)+start, search_string, size-start);
      search_string[size-start] = 0;
    } else
      search_string = CHARS(str)+start;

    nsub = RGX(prog)->re_nsub + 1;
    pm = ALLOCA(nsub * sizeof(regmatch_t));
    if (search)
      regexp = RGX(prog);
    else {
      /* doing a match */
      if (RGX_INFO(prog)->options) {
	/* strlen & strcpy OK, posix patterns are null terminated */
	char *pattern;

	pattern = ALLOCA(strlen(CHARS(RGX_PATTERN(prog)))+2);
	pattern[0] = '^';
	strcpy(pattern+1, CHARS(RGX_PATTERN(prog)));
	regcomp(RGX2(prog), pattern, RGX_INFO(prog)->options);
	RGX_INFO(prog)->options = 0;
      }
      regexp = RGX2(prog);
    }

    if (regexec(regexp, search_string, nsub, pm, 0) != 0)
      return BOOL_F;

    if (vector) {
      matches = make_vector(MAKINUM(2L * nsub), MAKINUM(-1L));
      while (nsub--) {
	VELTS(matches)[2*nsub+0] = MAKINUM(pm[nsub].rm_so + start);
	VELTS(matches)[2*nsub+1] = MAKINUM(pm[nsub].rm_eo + start);
      }
      return matches;
    }

    if (search)
      return MAKINUM(pm[0].rm_so + start);
    else
      return MAKINUM(pm[0].rm_eo - pm[0].rm_so);
  }

#endif /* _GNU_SOURCE */
}

SCM lregsearch(prog, str, args)
     SCM prog, str, args;
{
  return lregsearchmatch(prog, str, args, SEARCH, SCALAR);
}

SCM lregsearchv(prog, str, args)
     SCM prog, str, args;
{
  return lregsearchmatch(prog, str, args, SEARCH, VECTOR);
}

SCM lregmatch(prog, str, args)
     SCM prog, str, args;
{
  return lregsearchmatch(prog, str, args, MATCH, SCALAR);
}

SCM lregmatchv(prog, str, args)
     SCM prog, str, args;
{
  return lregsearchmatch(prog, str, args, MATCH, VECTOR);
}

SCM stringsplitutil(prog, str, vector)
     SCM prog, str;
     int vector;
{
  int anchor, match_start, match_end, num_substrings, num_elements;
  int search_base;
  SCM next_break, substrings, ret;
  SCM st_start, st_end;

  FIXUP_REGEXP(prog);
  ASRTER(NIMP(prog) && tc16_rgx==CAR(prog), prog, ARG1, s_stringsplit);
  ASRTER(NIMP(str) && STRINGP(str), str, ARG2, s_stringsplit);

  substrings = EOL;
  anchor = 0;
  search_base = 0;
  num_substrings = 0;
  next_break = lregsearchv(prog, str, cons(MAKINUM(search_base), EOL));

  while (next_break != BOOL_F) {
    match_start = INUM(VELTS(next_break)[0]);
    match_end   = INUM(VELTS(next_break)[1]);

    if (match_start < match_end) {
      substrings=cons2(MAKINUM(anchor), MAKINUM(match_start), substrings);
      anchor = match_end;
      num_substrings++;
    }

    search_base = ((match_end>search_base)?match_end:(search_base+1));
    next_break  = lregsearchv(prog, str, cons(MAKINUM(search_base), EOL));
  }

  /* get that tail bit */
  if (anchor < LENGTH(str)) {
    substrings = cons2(MAKINUM(anchor), MAKINUM(LENGTH(str)), substrings);
    num_substrings++;
  }

  num_elements = vector?(2*num_substrings):num_substrings;
  ret = make_vector(MAKINUM(num_elements), EOL);

  while (num_substrings--) {
    st_start = CAR(substrings);
    st_end	 = CAR(CDR(substrings));

    if (vector) {
      VELTS(ret)[num_substrings*2+0] = st_start;
      VELTS(ret)[num_substrings*2+1] = st_end;
    } else
      VELTS(ret)[num_substrings] = substring(str, st_start, st_end);

    substrings = CDR(CDR(substrings));
  }

  return ret;
}

SCM lstringsplit(prog, str)
     SCM prog, str;
{
  return stringsplitutil(prog, str, SCALAR);
}

SCM lstringsplitv(prog, str)
     SCM prog, str;
{
  return stringsplitutil(prog, str, VECTOR);
}

typedef struct _item {
    struct _item *next;
    char *string;
    int start;
    int end;
} *editItem;

#define PUSH(list, string_parm, start_parm, end_parm)	\
    {							\
	editItem item;					\
							\
	item = ALLOCA(sizeof(*item));			\
	item->next = list;				\
	list = item;					\
	item->string = string_parm;			\
	item->start  = start_parm;			\
	item->end    = end_parm;			\
    }

/*  (string-edit <re> <edit-spec> <string> [<count>]) */
SCM lstringedit(prog, editspec, args)
     SCM prog, editspec, args;
{
  int match_start, match_end, search_base, editcount;
  int total_len;
  int i, args_len, anchor, maxsubnum;
  int backslash;
  char *ptr;
  editItem editlist, substrings, edit;
  SCM str, count, next_edit;
  SCM result;
  ALLOCA_PROTECT;

  args_len = ilength(args);

  FIXUP_REGEXP(prog);
  ASRTER(NIMP(prog) && tc16_rgx==CAR(prog), prog, ARG1, s_stringedit);
  ASRTER(NIMP(editspec) && STRINGP(editspec), editspec, ARG2, s_stringedit);
  ASRTER((args_len==1)||(args_len==2), args, WNA, s_stringedit);

  str = CAR(args);
  ASRTER(NIMP(str)&&STRINGP(str), str, ARG3, s_stringedit);

  if (args_len==2) {
    count = CAR(CDR(args));
    ASRTER(INUMP(count)||(count==BOOL_T), count, ARG4, s_stringedit);
  } else
    count = MAKINUM(1L);

  /* process the editspec - break it into a list of dotted pairs
   * of integers for substrings to be inserted and
   * integers representing matched subexpressions that
   * should be inserted.
   */

  maxsubnum = RGX(prog)->re_nsub;
  anchor = 0;
  backslash = 0;
  editlist = 0;
  ptr = CHARS(editspec);

  for (i=0; i<LENGTH(editspec); i++) {
    if (backslash && (ptr[i]>='0') && (ptr[i] <='9') &&
	((ptr[i]-'0')<=maxsubnum))
      {
	if ((i-1)>anchor)
	  PUSH(editlist, CHARS(editspec), anchor, i-1);

	PUSH(editlist, CHARS(editspec), ptr[i]-'0', -1);
	anchor = i+1;
      }
    backslash = (ptr[i] == '\\')?1:0;
  }

  if (anchor < LENGTH(editspec))
    PUSH(editlist, CHARS(editspec), anchor, LENGTH(editspec));

  /* now, reverse the list of edit items */
  {
    editItem prev, cur, next;

    for (prev=0, cur=editlist; cur; prev=cur, cur=next) {
      next = cur->next;
      cur->next = prev;
    }
    editlist = prev;
  }

  anchor = 0;
  search_base = 0;
  editcount = 0;
  substrings = 0;

  next_edit = lregsearchv(prog, str, cons(MAKINUM(search_base), EOL));

  while (next_edit != BOOL_F) {
    if (INUMP(count) && (editcount==INUM(count)))
      break;

    match_start = INUM(VELTS(next_edit)[0]);
    match_end   = INUM(VELTS(next_edit)[1]);

    if (match_start < match_end) {
      PUSH(substrings, CHARS(str), anchor, match_start);
      anchor = match_end;
    }

    for (edit=editlist; edit; edit=edit->next) {
      if (edit->end == -1) {
	/* A backslash number in the original editspec */
	PUSH(substrings, CHARS(str),
	     INUM(VELTS(next_edit)[edit->start*2+0]),
	     INUM(VELTS(next_edit)[edit->start*2+1]));
      } else
	/* normal string in the editspec */
	PUSH(substrings, edit->string, edit->start, edit->end);
    }

    editcount++;
    search_base = ((match_end>search_base)?match_end:(search_base+1));
    next_edit  = lregsearchv(prog, str, cons(MAKINUM(search_base), EOL));
  }

  /* get that tail bit */
  if (anchor < LENGTH(str))
    PUSH(substrings, CHARS(str), anchor, LENGTH(str));

  /* assemble the result string */
  for (edit=substrings, total_len=0; edit; edit=edit->next)
    total_len += (edit->end - edit->start);

  result = makstr(total_len);
  ptr = CHARS(result) + total_len; /* point at the null at the end */

  for (edit=substrings; edit; edit=edit->next) {
    ptr -= (edit->end - edit->start);
    bcopy(edit->string + edit->start, ptr, edit->end - edit->start);
  }
  return result;
}
#undef PUSH

void init_rgx()
{
  tc16_rgx = newsmob(&rgxsmob);
  make_subr(s_regcomp, tc7_subr_2o, lregcomp);
  make_subr(s_regexec, tc7_subr_2, lregexec);
  make_subr(s_regmatp, tc7_subr_2, lregmatp);
  make_subr(s_regerror, tc7_subr_1, lregerror);
  make_subr(s_regsearch, tc7_lsubr_2, lregsearch);
  make_subr(s_regsearchv, tc7_lsubr_2, lregsearchv);
  make_subr(s_regmatch, tc7_lsubr_2, lregmatch);
  make_subr(s_regmatchv, tc7_lsubr_2, lregmatchv);
  make_subr(s_stringsplit, tc7_subr_2, lstringsplit);
  make_subr(s_stringsplitv, tc7_subr_2, lstringsplitv);
  make_subr(s_stringedit, tc7_lsubr_2, lstringedit);
  add_feature(s_regex);
}
