/* "rope.c" interface between C and SCM.
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1997, 2006 Free Software Foundation, Inc.
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

				/* Numeric conversions */
				/* Convert longs to SCM */
SCM long2num(sl)
     long sl;
{
  if (!FIXABLE(sl)) {
# ifdef BIGDIG
    return long2big(sl);
# else
#  ifdef FLOATS
    return makdbl((double) sl, 0.0);
#  else
    return BOOL_F;
#  endif
# endif
  }
  return MAKINUM(sl);
}
SCM ulong2num(sl)
     unsigned long sl;
{
  if (!POSFIXABLE(sl)) {
#ifdef BIGDIG
    return ulong2big(sl);
#else
# ifdef FLOATS
    return makdbl((double) sl, 0.0);
# else
    return BOOL_F;
# endif
#endif
  }
  return MAKINUM(sl);
}

				/* Convert SCM to numbers */
unsigned char num2uchar(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  unsigned long res = INUM(num);
  ASRTER(INUMP(num) && (255L >= res), num, pos, s_caller);
  return (unsigned char) res;
}
unsigned short num2ushort(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  unsigned long res = INUM(num);
  ASRTER(INUMP(num) && (65535L >= res), num, pos, s_caller);
  return (unsigned short) res;
}
unsigned long num2ulong(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  unsigned long res;
  if (INUMP(num)) {
    ASRTGO(0 < num, errout);
    res = INUM((unsigned long)num);
    return res;
  }
  ASRTGO(NIMP(num), errout);
#ifdef FLOATS
  if (REALP(num)) {
    double u = REALPART(num);
    if ((0 <= u) && (u <= (unsigned long)~0L)) {
      res = u;
      return res;
    }
  }
#endif
#ifdef BIGDIG
  if (TYP16(num)==tc16_bigpos) {
    sizet l = NUMDIGS(num);
    ASRTGO(DIGSPERLONG >= l, errout);
    res = 0;
    for (;l--;) res = BIGUP(res) + BDIGITS(num)[l];
    return res;
  }
#endif
 errout: wta(num, pos, s_caller);
}
long num2long(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  long res;
  if (INUMP(num)) {
    res = INUM((long)num);
    return res;
  }
  ASRTGO(NIMP(num), errout);
# ifdef FLOATS
  if (REALP(num)) {
    double u = REALPART(num);
    if (((MOST_NEGATIVE_FIXNUM * 4) <= u)
	&& (u <= (MOST_POSITIVE_FIXNUM * 4 + 3))) {
      res = u;
      return res;
    }
  }
# endif
# ifdef BIGDIG
  if (BIGP(num)) {
    sizet l = NUMDIGS(num);
    ASRTGO(DIGSPERLONG >= l, errout);
    res = 0;
    for (;l--;) res = BIGUP(res) + BDIGITS(num)[l];
    ASRTGO(0<res, errout);
    return (tc16_bigpos==TYP16(num) ? res : -res);
  }
# endif
 errout: wta(num, pos, s_caller);
}
short num2short(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  long lres = INUM((long)num);
  short res = lres;
  if (INUMP(num) && lres==res) return res;
  wta(num, pos, s_caller);
}
signed char num2char(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  long lres = INUM((long)num);
  char res = lres;
  if (INUMP(num) && lres==res) return res;
  wta(num, pos, s_caller);
}
#ifdef FLOATS
double num2dbl(num, pos, s_caller)
     SCM num;
     char *pos, *s_caller;
{
  if (INUMP(num)) return (double)INUM(num);
  ASRTGO(NIMP(num), errout);
  if (REALP(num)) return REALPART(num);
  if (scm_narn==num) return REALPART(num);
#ifdef BIGDIG
  if (BIGP(num)) return int2dbl(num);
#endif
 errout: wta(num, pos, s_caller);
}
#endif

				/* Convert string to SCM */
SCM makfromstr(src, len)
     const char *src;
     sizet len;
{
  SCM s;
  register char *dst;
  s = makstr((long)len);
  dst = CHARS(s);
  while (len--) *dst++ = *src++;
  return s;
}
				/* Convert null-terminated string to SCM */
SCM makfrom0str(src)
     const char *src;
{
  if (!src) return BOOL_F;
  return makfromstr(src, (sizet) strlen(src));
}
/* converts C array of strings to SCM list of strings. */
/* If argc < 0, a null terminated array is assumed. */
SCM makfromstrs(argc, argv)
     int argc;
     const char * const *argv;
{
  int i = argc;
  SCM lst = EOL;
  if (0 > i) for (i = 0; argv[i]; i++);
  while (i--) lst = cons(makfrom0str(argv[i]), lst);
  return lst;
}
/* Converts SCM list of strings to NULL terminated array of strings. */
/* INTS must be DEFERed around this call and the use of the returned array. */
char **makargvfrmstrs(args, s_name)
     SCM args;
     const char *s_name;
{
  char ** argv;
  int argc = ilength(args);
  argv = (char **)must_malloc((1L+argc)*sizeof(char *), s_vector);
  for (argc = 0; NNULLP(args); args=CDR(args), ++argc) {
    ASRTER(NIMP(CAR(args)) && STRINGP(CAR(args)), CAR(args), ARG2, s_name);
    {
      sizet len = 1 + LENGTH(CAR(args));
      char *dst = (char *)must_malloc((long)len, s_string);
      char *src = CHARS(CAR(args));
      while (len--) dst[len] = src[len];
      argv[argc] = dst;
    }
  }
  argv[argc] = 0;
  return argv;
}
void must_free_argv(argv)
     char **argv;
{
  sizet i;
  for (i = 0; argv[i]; i++) {
    must_free(argv[i], 1+strlen(argv[i]));
  }
  must_free((char *)argv, i*sizeof(char *));
}

				/* Hooks to call SCM from C */
SCM scm_evstr(str)
     char *str;
{
  SCM lsym;
  NEWCELL(lsym);
  SETLENGTH(lsym, strlen(str), tc7_ssymbol);
  SETCHARS(lsym, str);
  return scm_eval_string(lsym);
}
void scm_ldstr(str)
     char *str;
{
  SCM lsym;
  NEWCELL(lsym);
  SETLENGTH(lsym, strlen(str), tc7_ssymbol);
  SETCHARS(lsym, str);
  scm_load_string(lsym);
}
int scm_ldfile(path)
     char *path;
{
  SCM name = makfrom0str(path);
  *loc_errobj = name;
  return BOOL_F==tryload(name, UNDEFINED);
}
int scm_ldprog(path)
     char *path;
{
  SCM name = makfrom0str(path);
  *loc_errobj = name;
  return
    BOOL_F==scm_evstr("(try-load (in-vicinity (program-vicinity) errobj))");
}

				/* Get byte address of SCM array */
#ifdef ARRAYS
long	aind P((SCM ra, SCM args, const char *what));
void* scm_addr(args, s_name)
     SCM args;
     const char *s_name;
{
  long pos;
  void* ptr = 0;	/* gratuitous assignment squelches cc warn. */
  SCM v;
  ASRTGO(NIMP(args), wna);
  v = CAR(args);
  args = CDR(args);
  if (IMP(v)) {goto badarg;}
  else if (ARRAYP(v)) {
    pos = aind(v, args, s_name);
    v = ARRAY_V(v);
  }
  else {
    if (NIMP(args)) {
      ASRTER(CONSP(args) && INUMP(CAR(args)), args, ARG2, s_name);
      pos = INUM(CAR(args));
      ASRTGO(NULLP(CDR(args)), wna);
    }
    else if (NULLP(args)) pos = 0;
    else {
      ASRTER(INUMP(args), args, ARG2, s_name);
      pos = INUM(args);
    }
    ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
  }
  switch TYP7(v) {
  case tc7_string:
    ptr = (void*)&(CHARS(v)[pos]);
    break;
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_VfloC32: pos = 2 * pos;
  case tc7_VfloR32: ptr = (void*)&(((float *)CDR(v))[pos]);
    break;
#  endif
  case tc7_VfloC64: pos = 2 * pos;
  case tc7_VfloR64: ptr = (void*)&(((double *)CDR(v))[pos]);
    break;
# endif
  case tc7_Vbool: ASRTGO(0==(pos%LONG_BIT), outrng);
    pos = pos/LONG_BIT;
  case tc7_VfixN32:
  case tc7_VfixZ32:
  case tc7_vector: ptr = (void*)&(VELTS(v)[pos]);
    break;
  case tc7_VfixN16:
  case tc7_VfixZ16: ptr = (void*)&(((short *)CDR(v))[pos]);
    break;
  case tc7_VfixN8:
  case tc7_VfixZ8: ptr = (void*)&(((char *)CDR(v))[pos]);
    break;
  outrng: wta(MAKINUM(pos), (char *)OUTOFRANGE, s_name);
  default:
  badarg: wta(v, (char *)ARG1, s_name);
  wna: wta(UNDEFINED, (char *)WNA, s_name);
  }
  return ptr;
}
void* scm_base_addr(v, s_name)
     SCM v;
     const char *s_name;
{
  long pos = 0;
  void* ptr = 0;	/* gratuitous assignment squelches cc warn. */
  if (IMP(v)) {goto badarg;}
  else if (ARRAYP(v)) {
    pos = ARRAY_BASE(v);
    v = ARRAY_V(v);
  }
  switch TYP7(v) {
  case tc7_string:
    ptr = (void*)&(CHARS(v)[pos]);
    break;
# ifdef FLOATS
#  ifdef SINGLES
  case tc7_VfloC32: pos = 2 * pos;
  case tc7_VfloR32:
    ptr = (void*)&(((float *)CDR(v))[pos]);
    break;
#  endif
  case tc7_VfloC64: pos = 2 * pos;
  case tc7_VfloR64: ptr = (void*)&(((double *)CDR(v))[pos]);
    break;
# endif
  case tc7_Vbool: ASRTGO(0==(pos%LONG_BIT), outrng);
    pos = pos/LONG_BIT;
  case tc7_VfixN32:
  case tc7_VfixZ32:
  case tc7_vector: ptr = (void*)&(VELTS(v)[pos]);
    break;
  case tc7_VfixN16:
  case tc7_VfixZ16: ptr = (void*)&(((short *)CDR(v))[pos]);
    break;
  case tc7_VfixN8:
  case tc7_VfixZ8: ptr = (void*)&(((char *)CDR(v))[pos]);
    break;
  outrng: wta(MAKINUM(pos), (char *)OUTOFRANGE, s_name);
  default:
  badarg: wta(v, (char *)ARG1, s_name);
  }
  return ptr;
}
#endif /* ARRAYS */

extern sizet hplim_ind;
extern CELLPTR *hplims;

/* scm_cell_p() returns !0 if the SCM argument `x' is cell-aligned and
   points into a valid heap segment.  This code is duplicated from
   mark_locations() and obunhash() in "sys.c", which means that
   changes to these routines must be coordinated. */

int scm_cell_p(x)
     SCM x;
{
	register int i, j;
	register CELLPTR ptr;
	if (NCELLP(x)) return 0;
	ptr = (CELLPTR)SCM2PTR(x);
	i = 0;
	j = hplim_ind;
	do {
		if (PTR_GT(hplims[i++], ptr)) break;
		if (PTR_LE(hplims[--j], ptr)) break;
		if ((i != j)
		    && PTR_LE(hplims[i++], ptr)
		    && PTR_GT(hplims[--j], ptr)) continue;
		return !0; /* NFREEP(x) */
	} while(i<j);
	if (PTR_LE(scm_ecache, ptr)
	    && PTR_GT(scm_ecache+scm_ecache_len, ptr))
	  return !0; /* so we can print environments while debugging */
	return 0;
}

long scm_protidx = 0;

SCM scm_gc_protect(obj)
     SCM obj;
{
  long len;
  ASRTER(NIMP(scm_uprotects), MAKINUM(20), NALLOC, "protects");
  if (IMP(obj)) return obj;
  len = LENGTH(scm_uprotects);
  if (scm_protidx >= len) resizuve(scm_uprotects, MAKINUM(len + (len>>2)));
  VELTS(scm_uprotects)[scm_protidx++] = obj;
  return obj;
}

void init_rope()
{
  scm_protidx = 0;
  scm_uprotects = make_vector(MAKINUM(20), UNDEFINED);
}
