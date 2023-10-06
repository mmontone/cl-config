/* "unif.c" Uniform vectors and arrays
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996, 1997, 2002, 2006 Free Software Foundation, Inc.
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

/* Authors: Aubrey Jaffer & Radey Shouman.

The set of uniform vector types is:
 Vector of:		 Called:
char			string
boolean			Vbool
signed int		VfixZ32
unsigned int		VfixN32
float			VfloR32
complex float		VfloC32
double			VfloR64
complex double		VfloC64
*/

#include "scm.h"

#ifndef STDC_HEADERS
	int ungetc P((int c, FILE *stream));
# ifndef sun
	sizet fwrite ();
# endif
#endif

long tc16_array = 0;

char s_resizuve[] = "vector-set-length!";
SCM resizuve(vect, len)
     SCM vect, len;
{
  long ol, l = INUM(len);
  sizet siz, sz;
  ASRTGO(NIMP(vect), badarg1);
  ol = LENGTH(vect);
  switch TYP7(vect) {
  default: badarg1: wta(vect, (char *)ARG1, s_resizuve);
  case tc7_string:
    ASRTGO(vect != nullstr, badarg1);
    sz = sizeof(char);
    ol++;
    l++;
    break;
  case tc7_vector:
    ASRTGO(vect != nullvect, badarg1);
    sz = sizeof(SCM);
    break;
#ifdef ARRAYS
  case tc7_Vbool:
    ol = (ol+LONG_BIT-1)/LONG_BIT;
    l = (l+LONG_BIT-1)/LONG_BIT;
  case tc7_VfixN32:
  case tc7_VfixZ32:
    sz = sizeof(long);
    break;
  case tc7_VfixN16:
  case tc7_VfixZ16:
    sz = sizeof(short);
    break;
  case tc7_VfixN8:
  case tc7_VfixZ8:
    sz = sizeof(char);
    break;
# ifdef FLOATS
  case tc7_VfloR32:
    sz = sizeof(float);
    break;
  case tc7_VfloC32:
    sz = 2*sizeof(float);
    break;
  case tc7_VfloR64:
    sz = sizeof(double);
    break;
  case tc7_VfloC64:
    sz = 2*sizeof(double);
    break;
# endif
#endif
  }
  ASRTER(INUMP(len), len, ARG2, s_resizuve);
  if (!l) l = 1L;
  siz = l * sz;
  if (siz != l * sz) wta(MAKINUM(l * sz), (char *) NALLOC, s_resizuve);
  DEFER_INTS;
  must_realloc_cell(vect, ol*sz, (long)siz, s_resizuve);
  if (VECTORP(vect))
    while(l > ol)
      VELTS(vect)[--l] = UNSPECIFIED;
  else if (STRINGP(vect))
    CHARS(vect)[l-1] = 0;
  SETLENGTH(vect, INUM(len), TYP7(vect));
  ALLOW_INTS;
  return vect;
}

#ifdef ARRAYS

# ifdef FLOATS
#  ifdef SINGLES
SCM makflo (x)
     float x;
{
  SCM z;
  if (x==0.0) return flo0;
  NEWCELL(z);
  DEFER_INTS;
  CAR(z) = tc_flo;
  FLO(z) = x;
  ALLOW_INTS;
  return z;
}
#  else
#   define makflo(x) makdbl((double)(x), 0.0)
#  endif
# endif

long scm_prot2type(prot)
     SCM prot;
{
  if (ICHRP(prot)) return tc7_string;
  switch (prot) {
  case BOOL_T: return tc7_Vbool;
  case MAKINUM(8L):   return tc7_VfixN8;
  case MAKINUM(16L):  return tc7_VfixN16;
  case MAKINUM(64L):  return tc7_VfixN64;
  case MAKINUM(-64L): return tc7_VfixZ64;
  case MAKINUM(32L):  return tc7_VfixN32;
  case MAKINUM(-32L): return tc7_VfixZ32;
  case MAKINUM(-16L): return tc7_VfixZ16;
  case MAKINUM(-8L):  return tc7_VfixZ8;
  }
  /* if (INUMP(prot)) return INUM(prot) > 0 ? tc7_VfixN32 : tc7_VfixZ32; */
  if (IMP(prot)) return tc7_vector;
# ifdef FLOATS
  if (INEXP(prot)) {
    double x;
    if (CPLXP(prot)) return (32.0==IMAG(prot)) ? tc7_VfloC32 : tc7_VfloC64;
    x = REALPART(prot);
    if (32.0==x) return tc7_VfloR32;
    if (64.0==x) return tc7_VfloR64;
    return tc7_VfloR64;
  }
# endif
  return tc7_vector;
}

SCM make_uve(k, prot)
     long k;
     SCM prot;
{
  SCM v;
  long i;
  long type = scm_prot2type(prot);
  switch (type) {
  default:
  case tc7_vector: /* Huge non-unif vectors are NOT supported. */
    return make_vector(MAKINUM(k), UNDEFINED); /* no special vector */
  case tc7_Vbool:
    i = sizeof(long)*((k+LONG_BIT-1)/LONG_BIT);
    break;
  case tc7_string:
    i = sizeof(char)*(k + 1);
    break;
  case tc7_VfixN32:
  case tc7_VfixZ32:
    i = sizeof(long)*k;
    break;
  case tc7_VfixN16:
  case tc7_VfixZ16:
    i = sizeof(short)*k;
    break;
  case tc7_VfixN8:
  case tc7_VfixZ8:
    i = sizeof(char)*k;
    break;
# ifdef FLOATS
  case tc7_VfloR32:
    i = sizeof(float)*k;
    break;
  case tc7_VfloC32:
    i = 2*sizeof(float)*k;
    break;
  case tc7_VfloR64:
    i = sizeof(double)*k;
    break;
  case tc7_VfloC64:
    i = 2*sizeof(double)*k;
    break;
# endif
  }
  DEFER_INTS;
  /* Make a potentially HUGE object */
  v = must_malloc_cell((i ? i : 1L),
		       MAKE_LENGTH((k < LENGTH_MAX ? k : LENGTH_MAX), type),
		       s_vector);
  if (tc7_string==type) CHARS(v)[k] = 0;
  ALLOW_INTS;
  return v;
}

SCM arrayp(v, prot)
     SCM v, prot;
{
  int enclosed = 0;
  long typ;
  if (IMP(v)) return BOOL_F;
 loop:
  typ = TYP7(v);
  switch (typ) {
  case tc7_smob: if (!ARRAYP(v)) return BOOL_F;
    if (UNBNDP(prot)) return BOOL_T;
    if (enclosed++) return BOOL_F;
    v = ARRAY_V(v);
    goto loop;
  case tc7_vector:
  case tc7_VfloR64: case tc7_VfloC64: case tc7_VfloR32: case tc7_VfloC32:
  case tc7_VfixN64: case tc7_VfixZ64: case tc7_VfixN32: case tc7_VfixZ32:
  case tc7_VfixN16: case tc7_VfixZ16: case tc7_VfixN8: case tc7_VfixZ8:
  case tc7_Vbool:
  case tc7_string:
    if (UNBNDP(prot)) return BOOL_T;
    if (scm_prot2type(prot)==typ) return BOOL_T;
  }
  return BOOL_F;
}
SCM array_rank(ra)
     SCM ra;
{
  if (IMP(ra)) return INUM0;
  switch (TYP7(ra)) {
  default: return INUM0;
  case tc7_vector:
  case tcs_uves:
    return MAKINUM(1L);
  case tc7_smob:
    if (ARRAYP(ra)) return MAKINUM(ARRAY_NDIM(ra));
    return INUM0;
  }
}
static char s_array_dims[] = "array-dimensions";
SCM array_dims(ra)
     SCM ra;
{
  SCM res=EOL;
  sizet k;
  array_dim *s;
  if (IMP(ra)) return BOOL_F;
  switch (TYP7(ra)) {
  default: return BOOL_F;
  case tc7_vector:
  case tcs_uves:
    return cons(MAKINUM(LENGTH(ra)), EOL);
  case tc7_smob:
    if (!ARRAYP(ra)) return BOOL_F;
    k = ARRAY_NDIM(ra);
    s = ARRAY_DIMS(ra);
    while (k--)
      res = cons(s[k].lbnd ? cons2(MAKINUM(s[k].lbnd), MAKINUM(s[k].ubnd), EOL) :
		 MAKINUM(1+(s[k].ubnd))
		 , res);
    return res;
  }
}
static char s_bad_ind[] = "Bad array index";
long aind(ra, args, what)
     SCM ra, args;
     const char *what;
{
  SCM ind;
  register long j;
  register sizet pos = ARRAY_BASE(ra);
  register sizet k = ARRAY_NDIM(ra);
  array_dim *s = ARRAY_DIMS(ra);
  if (INUMP(args)) {
    ASRTER(1==k, UNDEFINED, WNA, what);
    j = INUM(args);
    ASRTER(j >= (s->lbnd) && j <= (s->ubnd), args, OUTOFRANGE, what);
    return pos + (j - s->lbnd)*(s->inc);
  }
  ASRTER((IMP(args) ? NULLP(args) : CONSP(args)), args, s_bad_ind, what);
  while (k && NIMP(args)) {
    ind = CAR(args);
    args = CDR(args);
    ASRTER(INUMP(ind), ind, s_bad_ind, what);
    j = INUM(ind);
    ASRTER(j >= (s->lbnd) && j <= (s->ubnd), ind, OUTOFRANGE, what);
    pos += (j - s->lbnd)*(s->inc);
    k--;
    s++;
  }
  ASRTER(0==k && NULLP(args), UNDEFINED, WNA, what);
  return pos;
}

/* Given rank, allocate cell only. */
SCM make_ra(ndim)
     int ndim;
{
  SCM ra;
  DEFER_INTS;
  ra = must_malloc_cell(sizeof(array)+((long)ndim)*sizeof(array_dim),
			(((long)ndim) << 17) + tc16_array,
			"array");
  ARRAY_V(ra) = nullvect;
  ALLOW_INTS;
  return ra;
}

static char s_bad_spec[] = "Bad array dimension";
/* Increments will still need to be set. */
SCM shap2ra(args, what)
     SCM args;
     const char *what;
{
  array_dim *s;
  SCM ra, spec, sp;
  int ndim = ilength(args);
  ASRTER(0 <= ndim, args, s_bad_spec, what);
  ra = make_ra(ndim);
  ARRAY_BASE(ra) = 0;
  s = ARRAY_DIMS(ra);
  for (; NIMP(args); s++, args = CDR(args)) {
    spec = CAR(args);
    if (IMP(spec)) {
      ASRTER(INUMP(spec)&&INUM(spec)>=0, spec, s_bad_spec, what);
      s->lbnd = 0;
      s->ubnd = INUM(spec) - 1;
      s->inc = 1;
    }
    else {
      ASRTER(CONSP(spec) && INUMP(CAR(spec)), spec, s_bad_spec, what);
      s->lbnd = INUM(CAR(spec));
      sp = CDR(spec);
      ASRTER(NIMP(sp) && INUMP(CAR(sp)) && NULLP(CDR(sp)),
	     spec, s_bad_spec, what);
      s->ubnd = INUM(CAR(sp));
      ASRTER(s->ubnd >= s->lbnd, spec, s_bad_spec, what);
      s->inc = 1;
    }
  }
  return ra;
}

char s_array_fill[] = "array-fill!";
int rafill(ra, fill, ignore)
     SCM ra, fill, ignore;
{
  sizet i, n;
  long inc = 1;
  sizet base = 0;
  if (ARRAYP(ra)) {
    n = ARRAY_DIMS(ra)->ubnd - ARRAY_DIMS(ra)->lbnd + 1;
    inc = ARRAY_DIMS(ra)->inc;
    base = ARRAY_BASE(ra);
    ra = ARRAY_V(ra);
  }
  else
    n = LENGTH(ra);
  switch TYP7(ra) {
  badarg2: wta(fill, (char *)ARG2, s_array_fill);
  default: ASRTER(NFALSEP(arrayp(ra, UNDEFINED)), ra, ARG1, s_array_fill);
    for (i = base; n--; i += inc)
      aset(ra, fill, MAKINUM(i));
    break;
  case tc7_vector: {
    SCM *ve = VELTS(ra);
    for (i = base; n--; i += inc)
      ve[i] = fill;
    break;
  }
  case tc7_string: {
    char *ve = CHARS(ra);
    SCM f = ICHR(fill);
    ASRTGO(ICHRP(fill), badarg2);
    for (i = base; n--; i += inc)
      ve[i] = f;
    break;
  }
  case tc7_Vbool: {
    long *ve = (long *)VELTS(ra);
    if (1==inc && (n >= LONG_BIT || n==LENGTH(ra))) {
      i = base/LONG_BIT;
      if (BOOL_F==fill) {
	if (base % LONG_BIT)	/* leading partial word */
	  ve[i++] &= ~(~0L << (base % LONG_BIT));
	for (; i < (base + n)/LONG_BIT; i++)
	  ve[i] = 0L;
	if ((base + n) % LONG_BIT) /* trailing partial word */
	  ve[i] &= (~0L << ((base + n) % LONG_BIT));
      }
      else if (BOOL_T==fill) {
	if (base % LONG_BIT)
	  ve[i++] |= ~0L << (base % LONG_BIT);
	for (; i < (base + n)/LONG_BIT; i++)
	  ve[i] = ~0L;
	if ((base + n) % LONG_BIT)
	  ve[i] |= ~(~0L << ((base + n) % LONG_BIT));
      }
      else goto badarg2;
    }
    else {
      if (BOOL_F==fill)
	for (i = base; n--; i += inc)
	  ve[i/LONG_BIT] &= ~(1L<<(i%LONG_BIT));
      else if (BOOL_T==fill)
	for (i = base; n--; i += inc)
	  ve[i/LONG_BIT] |= (1L<<(i%LONG_BIT));
      else goto badarg2;
    }
    break;
  }
  case tc7_VfixN32:
  case tc7_VfixZ32:
    {
      long *ve = VELTS(ra);
      long f = (tc7_VfixN32==TYP7(ra) ?
		num2ulong(fill, (char *)ARG2, s_array_fill) :
		num2long(fill, (char *)ARG2, s_array_fill));
      for (i = base; n--; i += inc)
	ve[i] = f;
      break;
    }
# ifdef FLOATS
  case tc7_VfloR32: {
    float *ve = (float *)VELTS(ra);
    float f = num2dbl(fill, (char *)ARG2, s_array_fill);
    for (i = base; n--; i += inc)
      ve[i] = f;
    break;
  }
  case tc7_VfloC32: {
    float fr, fi=0.0;
    float (*ve)[2] = (float (*)[2])VELTS(ra);
    if (NIMP(fill) && CPLXP(fill)) {
      fr = REAL(fill);
      fi = IMAG(fill);
    }
    else
      fr = num2dbl(fill, (char *)ARG2, s_array_fill);
    for (i = base; n--; i += inc) {
      ve[i][0] = fr;
      ve[i][1] = fi;
    }
    break;
  }
  case tc7_VfloR64: {
    double *ve = (double *)VELTS(ra);
    double f = num2dbl(fill, (char *)ARG2, s_array_fill);
    for (i = base; n--; i += inc)
      ve[i] = f;
    break;
  }
  case tc7_VfloC64: {
    double fr, fi=0.0;
    double (*ve)[2] = (double (*)[2])VELTS(ra);
    if (NIMP(fill) && CPLXP(fill)) {
      fr = REAL(fill);
      fi = IMAG(fill);
    }
    else
      fr = num2dbl(fill, (char *)ARG2, s_array_fill);
    for (i = base; n--; i += inc) {
      ve[i][0] = fr;
      ve[i][1] = fi;
    }
    break;
  }
# endif /* FLOATS */
  }
  return 1;
}

static char s_dims2ura[] = "dimensions->uniform-array";
SCM dims2ura(dims, prot, fill)
     SCM dims, prot, fill;
{
  sizet k, vlen = 1;
  long rlen = 1;
  array_dim *s;
  SCM ra;
  if (INUMP(dims)) {
    if (INUM(dims) < LENGTH_MAX) {
      ra = make_uve(INUM(dims), prot);
      if (NNULLP(fill))
	rafill(ra, CAR(fill), UNDEFINED);
      return ra;
    }
    else
      dims = cons(dims, EOL);
  }
  ASRTER(NULLP(dims) || (NIMP(dims) && CONSP(dims)), dims, ARG1, s_dims2ura);
  ra = shap2ra(dims, s_dims2ura);
  CAR(ra) |= ARRAY_CONTIGUOUS;
  s = ARRAY_DIMS(ra);
  k = ARRAY_NDIM(ra);
  while (k--) {
    s[k].inc = (rlen > 0 ? rlen : 0);
    rlen = (s[k].ubnd - s[k].lbnd + 1)*s[k].inc;
    vlen *= (s[k].ubnd - s[k].lbnd + 1);
  }
  if (rlen <= 0)
    ARRAY_V(ra) = make_uve(0L, prot);
  else if (rlen < LENGTH_MAX)
    ARRAY_V(ra) = make_uve(rlen, prot);
  else {
    sizet bit;
    switch TYP7(make_uve(0L, prot)) {
    default: bit = LONG_BIT; break;
    case tc7_vector: wta(dims, (char *)OUTOFRANGE, s_dims2ura);
    case tc7_Vbool: bit = 1; break;
    case tc7_string: bit = CHAR_BIT; break;
    case tc7_VfloR32: bit = sizeof(float)*CHAR_BIT/sizeof(char); break;
    case tc7_VfloC32: bit = 2*sizeof(float)*CHAR_BIT/sizeof(char); break;
    case tc7_VfloR64: bit = sizeof(double)*CHAR_BIT/sizeof(char); break;
    case tc7_VfloC64: bit = 2*sizeof(double)*CHAR_BIT/sizeof(char); break;
    }
    ARRAY_BASE(ra) = (LONG_BIT + bit - 1)/bit;
    rlen += ARRAY_BASE(ra);
    ARRAY_V(ra) = make_uve(rlen, prot);
    *((long *)VELTS(ARRAY_V(ra))) = rlen;
  }
  if (NNULLP(fill)) {
    ASRTER(1==ilength(fill), UNDEFINED, WNA, s_dims2ura);
    rafill(ARRAY_V(ra), CAR(fill), UNDEFINED);
  }
  if (1==ARRAY_NDIM(ra) && 0==ARRAY_BASE(ra))
    if (s->ubnd < s->lbnd || (0==s->lbnd && 1==s->inc)) return ARRAY_V(ra);
  return ra;
}

void ra_set_contp(ra)
     SCM ra;
{
  sizet k =  ARRAY_NDIM(ra);
  long inc;
  if (k) inc = ARRAY_DIMS(ra)[k-1].inc;
  while (k--) {
    if (inc != ARRAY_DIMS(ra)[k].inc) {
      CAR(ra) &= ~ARRAY_CONTIGUOUS;
      return;
    }
    inc *= (ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd + 1);
  }
  CAR(ra) |= ARRAY_CONTIGUOUS;
}
char s_make_sh_array[] = "make-shared-array";
SCM make_sh_array(oldra, mapfunc, dims)
     SCM oldra;
     SCM mapfunc;
     SCM dims;
{
  SCM ra, imap, auto_indv[5], hp_indv;
  SCM *indv = auto_indv;
  sizet i, k;
  long old_min, new_min, old_max, new_max;
  array_dim *s;
  ASRTER(BOOL_T==procedurep(mapfunc), mapfunc, ARG2, s_make_sh_array);
  ASRTER(NIMP(oldra) && arrayp(oldra, UNDEFINED), oldra, ARG1, s_make_sh_array);
# ifndef RECKLESS
  scm_arity_check(mapfunc, ilength(dims), s_make_sh_array);
# endif
  ra = shap2ra(dims, s_make_sh_array);
  if (ARRAYP(oldra)) {
    ARRAY_V(ra) = ARRAY_V(oldra);
    old_min = old_max = ARRAY_BASE(oldra);
    s=ARRAY_DIMS(oldra);
    k = ARRAY_NDIM(oldra);
    while (k--) {
      if (s[k].inc > 0)
	old_max += (s[k].ubnd - s[k].lbnd)*s[k].inc;
      else
	old_min += (s[k].ubnd - s[k].lbnd)*s[k].inc;
    }
  }
  else {
    ARRAY_V(ra) = oldra;
    old_min = 0;
    old_max = (long)LENGTH(oldra) - 1;
  }
  if (ARRAY_NDIM(ra) > 5) {
    scm_protect_temp(&hp_indv);
    hp_indv = make_vector(MAKINUM(ARRAY_NDIM(ra)), BOOL_F);
    indv = VELTS(hp_indv);
  }
  s = ARRAY_DIMS(ra);
  for (k = 0; k < ARRAY_NDIM(ra); k++) {
    indv[k] = MAKINUM(s[k].lbnd);
    if (s[k].ubnd < s[k].lbnd) {
      if (1==ARRAY_NDIM(ra))
	ra = make_uve(0L, array_prot(ra));
      else
	ARRAY_V(ra) = make_uve(0L, array_prot(ra));
      return ra;
    }
  }
  imap = scm_cvapply(mapfunc, ARRAY_NDIM(ra), indv);
  if (ARRAYP(oldra))
    i = (sizet)aind(oldra, imap, s_make_sh_array);
  else {
    if (NINUMP(imap)) {
      ASRTER(1==ilength(imap) && INUMP(CAR(imap)),
	     imap, s_bad_ind, s_make_sh_array);
      imap = CAR(imap);
    }
    i = INUM(imap);
  }
  ARRAY_BASE(ra) = new_min = new_max = i;
  k = ARRAY_NDIM(ra);
  while (k--) {
    if (s[k].ubnd > s[k].lbnd) {
      /*      CAR(indptr) = MAKINUM(INUM(CAR(indptr))+1);
	      imap = apply(mapfunc, reverse(inds), EOL); */
      indv[k] = MAKINUM(INUM(indv[k]) + 1);
      imap = scm_cvapply(mapfunc, ARRAY_NDIM(ra), indv);
      if (ARRAYP(oldra))
	s[k].inc = aind(oldra, imap, s_make_sh_array) - i;
      else {
	if (NINUMP(imap)) {
	  ASRTER(1==ilength(imap) && INUMP(CAR(imap)),
		 imap, s_bad_ind, s_make_sh_array);
	  imap = CAR(imap);
	}
	s[k].inc = (long)INUM(imap) - i;
      }
      i += s[k].inc;
      if (s[k].inc > 0)
	new_max += (s[k].ubnd - s[k].lbnd)*s[k].inc;
      else
	new_min += (s[k].ubnd - s[k].lbnd)*s[k].inc;
    }
    else
      s[k].inc = new_max - new_min + 1; /* contiguous by default */
  }
  ASRTER(old_min <= new_min && old_max >= new_max, UNDEFINED,
	 "mapping out of range", s_make_sh_array);
  if (1==ARRAY_NDIM(ra) && 0==ARRAY_BASE(ra)) {
    if (1==s->inc && 0==s->lbnd
	&& LENGTH(ARRAY_V(ra))==1+s->ubnd) return ARRAY_V(ra);
    if (s->ubnd < s->lbnd) return make_uve(0L, array_prot(ra));
  }
  ra_set_contp(ra);
  return ra;
}

/* args are RA . DIMS */
static char s_trans_array[] = "transpose-array";
SCM trans_array(args)
     SCM args;
{
  SCM ra, res, vargs, *ve = &vargs;
  array_dim *s, *r;
  int ndim, i, k;
  ASRTER(NIMP(args), UNDEFINED, WNA, s_trans_array);
  ra = CAR(args);
  args = CDR(args);
  switch TYP7(ra) {
  default: badarg: wta(ra, (char *)ARG1, s_trans_array);
  case tc7_vector:
  case tcs_uves:
    ASRTER(NIMP(args) && NULLP(CDR(args)), UNDEFINED, WNA, s_trans_array);
    ASRTER(INUM0==CAR(args), CAR(args), ARG1, s_trans_array);
    return ra;
  case tc7_smob: ASRTGO(ARRAYP(ra), badarg);
    vargs = vector(args);
    ASRTER(LENGTH(vargs)==ARRAY_NDIM(ra), UNDEFINED, WNA, s_trans_array);
    ve = VELTS(vargs);
    ndim = 0;
    for (k = 0; k < ARRAY_NDIM(ra); k++) {
      i = INUM(ve[k]);
      ASRTER(INUMP(ve[k]) && i >=0 && i < ARRAY_NDIM(ra),
	     ve[k], ARG2, s_trans_array);
      if (ndim < i) ndim = i;
    }
    ndim++;
    res = make_ra(ndim);
    ARRAY_V(res) = ARRAY_V(ra);
    ARRAY_BASE(res) = ARRAY_BASE(ra);
    for (k = ndim; k--;) {
      ARRAY_DIMS(res)[k].lbnd = 0;
      ARRAY_DIMS(res)[k].ubnd = -1;
    }
    for (k = ARRAY_NDIM(ra); k--;) {
      i = INUM(ve[k]);
      s = &(ARRAY_DIMS(ra)[k]);
      r = &(ARRAY_DIMS(res)[i]);
      if (r->ubnd < r->lbnd) {
	r->lbnd = s->lbnd;
	r->ubnd = s->ubnd;
	r->inc = s->inc;
	ndim--;
      }
      else {
	if (r->ubnd > s->ubnd)
	  r->ubnd = s->ubnd;
	if (r->lbnd < s->lbnd) {
	  ARRAY_BASE(res) += (s->lbnd - r->lbnd)*r->inc;
	  r->lbnd = s->lbnd;
	}
	r->inc += s->inc;
      }
    }
    ASRTER(ndim <= 0, args, "bad argument list", s_trans_array);
    ra_set_contp(res);
    return res;
  }
}

/* args are RA . AXES */
static char s_encl_array[] = "enclose-array";
SCM encl_array(axes)
     SCM axes;
{
  SCM axv, ra, res, ra_inr;
  array_dim vdim, *s = &vdim;
  int ndim, j, k, ninr, noutr;
  ASRTER(NIMP(axes), UNDEFINED, WNA, s_encl_array);
  ra = CAR(axes);
  axes = CDR(axes);
  if (NULLP(axes))
    axes =  cons((ARRAYP(ra) ? MAKINUM(ARRAY_NDIM(ra) - 1) : INUM0), EOL);
  ninr = ilength(axes);
  ra_inr = make_ra(ninr);
  ASRTGO(NIMP(ra), badarg1);
  switch TYP7(ra) {
  default: badarg1: wta(ra, (char *)ARG1, s_encl_array);
  case tc7_vector:
  case tcs_uves:
    s->lbnd = 0;
    s->ubnd = LENGTH(ra) - 1;
    s->inc = 1;
    ARRAY_V(ra_inr) = ra;
    ARRAY_BASE(ra_inr) = 0;
    ndim = 1;
    break;
  case tc7_smob: ASRTGO(ARRAYP(ra), badarg1);
    s = ARRAY_DIMS(ra);
    ARRAY_V(ra_inr) = ARRAY_V(ra);
    ARRAY_BASE(ra_inr) = ARRAY_BASE(ra);
    ndim = ARRAY_NDIM(ra);
    break;
  }
  noutr = ndim - ninr;
  axv = make_string(MAKINUM(ndim), MAKICHR(0));
  ASRTER(0 <= noutr && 0 <= ninr, UNDEFINED, WNA, s_encl_array);
  res = make_ra(noutr);
  ARRAY_BASE(res) = ARRAY_BASE(ra_inr);
  ARRAY_V(res) = ra_inr;
  for (k = 0; k < ninr; k++, axes = CDR(axes)) {
    j = INUM(CAR(axes));
    ASRTER(INUMP(CAR(axes)) && j<ndim, CAR(axes), "bad axis", s_encl_array);
    ARRAY_DIMS(ra_inr)[k].lbnd = s[j].lbnd;
    ARRAY_DIMS(ra_inr)[k].ubnd = s[j].ubnd;
    ARRAY_DIMS(ra_inr)[k].inc = s[j].inc;
    CHARS(axv)[j] = 1;
  }
  for (j = 0, k = 0; k < noutr; k++, j++) {
    while (CHARS(axv)[j]) j++;
    ARRAY_DIMS(res)[k].lbnd = s[j].lbnd;
    ARRAY_DIMS(res)[k].ubnd = s[j].ubnd;
    ARRAY_DIMS(res)[k].inc = s[j].inc;
  }
  ra_set_contp(ra_inr);
  ra_set_contp(res);
  return res;
}

static char s_array_inbp[] = "array-in-bounds?";
SCM array_inbp(args)
  SCM args;
{
  SCM v, ind = EOL;
  register long j;
  ASRTGO(NIMP(args), wna);
  v = CAR(args);
  args = CDR(args);
  if (IMP(v)) goto scalar;
  switch TYP7(v) {
  wna: wta(UNDEFINED, (char *)WNA, s_array_inbp);
  default: scalar:
    if (NULLP(args)) return BOOL_T;
    wta(v, (char *)ARG1, s_array_inbp);
  case tc7_smob:
    if (ARRAYP(v)) {
      SCM ret = BOOL_T;
      register sizet k = ARRAY_NDIM(v);
      array_dim *s = ARRAY_DIMS(v);
      while (k && NIMP(args)) {
	ind = CAR(args);
	args = CDR(args);
	ASRTER(INUMP(ind), ind, s_bad_ind, s_array_inbp);
	j = INUM(ind);
	if (j < (s->lbnd) || j > (s->ubnd)) ret = BOOL_F;
	k--;
	s++;
      }
      ASRTGO(0==k && NULLP(args), wna);
      return ret;
    }
    else goto scalar;
  case tc7_vector:
  case tcs_uves:
    ASRTGO(NIMP(args) && NULLP(CDR(args)), wna);
    ind = CAR(args);
    ASRTER(INUMP(ind), ind, s_bad_ind, s_array_inbp);
    j = INUM(ind);
    return j >= 0 && j < LENGTH(v) ? BOOL_T : BOOL_F;
  }
}
static char s_aref[] = "array-ref";
SCM aref(v, args)
     SCM v, args;
{
  long pos;
  if (IMP(v)) {
    ASRTGO(NULLP(args), badarg);
    return v;
  }
  else if (ARRAYP(v)) {
    pos = aind(v, args, s_aref);
    v = ARRAY_V(v);
  }
  else {
    if (NIMP(args)) {
      ASRTER(CONSP(args) && INUMP(CAR(args)), args, ARG2, s_aref);
      pos = INUM(CAR(args));
      ASRTGO(NULLP(CDR(args)), wna);
    }
    else {
      ASRTER(INUMP(args), args, ARG2, s_aref);
      pos = INUM(args);
    }
    ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
  }
  switch TYP7(v) {
  default: if (NULLP(args)) return v;
  badarg: wta(v, (char *)ARG1, s_aref);
  outrng: wta(MAKINUM(pos), (char *)OUTOFRANGE, s_aref);
  wna: wta(UNDEFINED, (char *)WNA, s_aref);
  case tc7_smob: {		/* enclosed */
    int k = ARRAY_NDIM(v);
    SCM res = make_ra(k);
    if (!ARRAYP(v)) {
      ASRTGO(NULLP(args), badarg);
      return v;
    }
    ARRAY_V(res) = ARRAY_V(v);
    ARRAY_BASE(res) = pos;
    while (k--) {
      ARRAY_DIMS(res)[k].lbnd = ARRAY_DIMS(v)[k].lbnd;
      ARRAY_DIMS(res)[k].ubnd = ARRAY_DIMS(v)[k].ubnd;
      ARRAY_DIMS(res)[k].inc = ARRAY_DIMS(v)[k].inc;
    }
    return res;
  }
  case tc7_Vbool:
    if (VELTS(v)[pos/LONG_BIT]&(1L<<(pos%LONG_BIT)))
      return BOOL_T;
    else return BOOL_F;
  case tc7_string:
    return MAKICHR(CHARS(v)[pos]);
  case tc7_VfixN8:
    return MAKINUM(((unsigned char *)CDR(v))[pos]);
  case tc7_VfixZ8:
    return MAKINUM(((signed char *)CDR(v))[pos]);
  case tc7_VfixN16:
    return MAKINUM(((unsigned short *)CDR(v))[pos]);
  case tc7_VfixZ16:
    return MAKINUM(((short *)CDR(v))[pos]);
# ifdef INUMS_ONLY
  case tc7_VfixN32:
  case tc7_VfixZ32:
    return MAKINUM(VELTS(v)[pos]);
# else
  case tc7_VfixN32:
    return ulong2num(VELTS(v)[pos]);
  case tc7_VfixZ32:
    return long2num(VELTS(v)[pos]);
# endif
# ifdef FLOATS
  case tc7_VfloR32:
    return makflo(((float *)CDR(v))[pos]);
  case tc7_VfloC32:
    return makdbl(((float *)CDR(v))[2*pos],
		  ((float *)CDR(v))[2*pos+1]);
  case tc7_VfloR64:
    return makdbl(((double *)CDR(v))[pos], 0.0);
  case tc7_VfloC64:
    return makdbl(((double *)CDR(v))[2*pos],
		  ((double *)CDR(v))[2*pos+1]);
# endif
  case tc7_vector:
    return VELTS(v)[pos];
  }
}
SCM scm_array_ref(args)
     SCM args;
{
  ASRTER(NIMP(args), UNDEFINED, WNA, s_aref);
  return aref(CAR(args), CDR(args));
}

/* Internal version of aref for uves that does no error checking and
   tries to recycle conses.  (Make *sure* you want them recycled.) */
SCM cvref(v, pos, last)
     SCM v;
     sizet pos;
     SCM last;
{
  switch TYP7(v) {
  default: wta(v, (char *)ARG1, "PROGRAMMING ERROR: cvref");
  case tc7_smob: {		/* enclosed array */
    int k = ARRAY_NDIM(v);
    if (IMP(last) || (!ARRAYP(last))) {
      last = make_ra(k);
      ARRAY_V(last) = ARRAY_V(v);
      ARRAY_BASE(last) = pos;
      while (k--) {
	ARRAY_DIMS(last)[k].ubnd = ARRAY_DIMS(v)[k].ubnd;
	ARRAY_DIMS(last)[k].lbnd = ARRAY_DIMS(v)[k].lbnd;
	ARRAY_DIMS(last)[k].inc = ARRAY_DIMS(v)[k].inc;
      }
    }
    return last;
  }
  case tc7_Vbool:
    if (VELTS(v)[pos/LONG_BIT]&(1L<<(pos%LONG_BIT)))
      return BOOL_T;
    else return BOOL_F;
  case tc7_string:
    return MAKICHR(CHARS(v)[pos]);
  case tc7_VfixN8:
    return MAKINUM(((unsigned char *)CDR(v))[pos]);
  case tc7_VfixZ8:
    return MAKINUM(((signed char *)CDR(v))[pos]);
  case tc7_VfixN16:
    return MAKINUM(((unsigned short *)CDR(v))[pos]);
  case tc7_VfixZ16:
    return MAKINUM(((short *)CDR(v))[pos]);
# ifdef INUMS_ONLY
  case tc7_VfixN32:
  case tc7_VfixZ32:
    return MAKINUM(VELTS(v)[pos]);
# else
  case tc7_VfixN32:
    return ulong2num(VELTS(v)[pos]);
  case tc7_VfixZ32:
    return long2num(VELTS(v)[pos]);
# endif
# ifdef FLOATS
  case tc7_VfloC32:
    if (0.0 != ((float *)CDR(v))[2*pos+1]) {
      if (NIMP(last) && tc_dblc==CAR(last)) {
	REAL(last) = ((float *)CDR(v))[2*pos];
	IMAG(last) = ((float *)CDR(v))[2*pos+1];
	return last;
      }
      return makdbl(((float *)CDR(v))[2*pos],
		    ((float *)CDR(v))[2*pos+1]);
    }
    else pos *= 2;
    /* Fall through */
  case tc7_VfloR32:
#  ifdef SINGLES
    if (NIMP(last) && (last != flo0) && (tc_flo==CAR(last))) {
      FLO(last) = ((float *)CDR(v))[pos];
      return last;
    }
    return makflo(((float *)CDR(v))[pos]);
#  else /* ndef SINGLES */
    if (NIMP(last) && (last != flo0) && (tc_dblr==CAR(last))) {
      REAL(last) = ((float *)CDR(v))[pos];
      return last;
    }
    return makdbl((double)((float *)CDR(v))[pos], 0.0);
#  endif
  case tc7_VfloC64:
    if (0.0!=((double *)CDR(v))[2*pos+1]) {
      if (NIMP(last) && tc_dblc==CAR(last)) {
	REAL(last) = ((double *)CDR(v))[2*pos];
	IMAG(last) = ((double *)CDR(v))[2*pos+1];
	return last;
      }
      return makdbl(((double *)CDR(v))[2*pos],
		    ((double *)CDR(v))[2*pos+1]);
    }
    else pos *= 2;
    /* Fall through */
  case tc7_VfloR64:
#  ifdef CDR_DOUBLES
    if (NIMP(last) && (last != flo0) && (tc_flo==CAR(last))) {
      FLO(last) = ((double *)CDR(v))[pos];
      return last;
    }
#  else
#   ifdef SINGLES
    if (NIMP(last) && tc_dblr==CAR(last))
#   else
      if (NIMP(last) && (last != flo0) && (tc_dblr==CAR(last)))
#   endif
	{
	  REAL(last) = ((double *)CDR(v))[pos];
	  return last;
	}
#  endif /* ndef CDR_DOUBLES */
    return makdbl(((double *)CDR(v))[pos], 0.0);
# endif /* def FLOATS */
  case tc7_vector:
    return VELTS(v)[pos];
  }
}

static char s_aset[] = "array-set!";
SCM aset(v, obj, args)
     SCM v, obj, args;
{
  long pos;
  ASRTGO(NIMP(v), badarg1);
  if (ARRAYP(v)) {
    pos = aind(v, args, s_aset);
    v = ARRAY_V(v);
  }
  else {
    if (NIMP(args)) {
      ASRTER(CONSP(args) && INUMP(CAR(args)), args, ARG2, s_aset);
      pos = INUM(CAR(args));
      ASRTGO(NULLP(CDR(args)), wna);
    }
    else {
      ASRTER(INUMP(args), args, ARG2, s_aset);
      pos = INUM(args);
    }
    ASRTGO(pos >= 0 && pos < LENGTH(v), outrng);
  }
  switch TYP7(v) {
  default: badarg1: wta(v, (char *)ARG1, s_aset);
  outrng: wta(MAKINUM(pos), (char *)OUTOFRANGE, s_aset);
  wna: wta(UNDEFINED, (char *)WNA, s_aset);
  case tc7_smob:		/* enclosed */
    goto badarg1;
  case tc7_Vbool:
    if (BOOL_F==obj)
      VELTS(v)[pos/LONG_BIT] &= ~(1L<<(pos%LONG_BIT));
    else if (BOOL_T==obj)
      VELTS(v)[pos/LONG_BIT] |= (1L<<(pos%LONG_BIT));
    else badarg2: wta(obj, (char *)ARG2, s_aset);
    break;
  case tc7_string:
    ASRTGO(ICHRP(obj), badarg2);
    CHARS(v)[pos] = ICHR(obj); break;
  case tc7_VfixN8:
    ((unsigned char *)VELTS(v))[pos] = num2uchar(obj, (char *)ARG2, s_aset); break;
  case tc7_VfixZ8:
    ((signed char *)VELTS(v))[pos] = num2char(obj, (char *)ARG2, s_aset); break;
  case tc7_VfixN16:
    ((unsigned short *)VELTS(v))[pos] = num2ushort(obj, (char *)ARG2, s_aset); break;
  case tc7_VfixZ16:
    ((short *)VELTS(v))[pos] = num2short(obj, (char *)ARG2, s_aset); break;
# ifdef INUMS_ONLY
  case tc7_VfixN32:
    ASRTGO(INUM(obj) >= 0, badarg2);
  case tc7_VfixZ32:
    ASRTGO(INUMP(obj), badarg2); VELTS(v)[pos] = INUM(obj); break;
# else
  case tc7_VfixN32:
    VELTS(v)[pos] = num2ulong(obj, (char *)ARG2, s_aset); break;
  case tc7_VfixZ32:
    VELTS(v)[pos] = num2long(obj, (char *)ARG2, s_aset); break;
# endif
# ifdef FLOATS
  case tc7_VfloR32:
    ((float*)VELTS(v))[pos] = (float)num2dbl(obj, (char *)ARG2, s_aset); break;
  case tc7_VfloC32:
    if (NIMP(obj) && CPLXP(obj)) {
      ((float *)CDR(v))[2*pos] = REALPART(obj);
      ((float *)CDR(v))[2*pos+1] = IMAG(obj);
    }
    else {
      ((float *)CDR(v))[2*pos] = num2dbl(obj, (char *)ARG2, s_aset);
      ((float *)CDR(v))[2*pos+1] = 0.0;
    }
    break;
  case tc7_VfloR64:
    ((double*)VELTS(v))[pos] = num2dbl(obj, (char *)ARG2, s_aset); break;
  case tc7_VfloC64:
    if (NIMP(obj) && CPLXP(obj)) {
      ((double *)CDR(v))[2*pos] = REALPART(obj);
      ((double *)CDR(v))[2*pos+1] = IMAG(obj);
    }
    else {
      ((double *)CDR(v))[2*pos] = num2dbl(obj, (char *)ARG2, s_aset);
      ((double *)CDR(v))[2*pos+1] = 0.0;
    }
    break;
# endif
  case tc7_vector:
    VELTS(v)[pos] = obj; break;
  }
  return UNSPECIFIED;
}

static char s_array_contents[] = "array-contents";
SCM array_contents(ra, strict)
     SCM ra, strict;
{
  SCM sra;
  if (IMP(ra)) return BOOL_F;
  switch TYP7(ra) {
  default:
    return BOOL_F;
  case tc7_vector:
  case tcs_uves:
    return ra;
  case tc7_smob: {
    sizet k, ndim = ARRAY_NDIM(ra), len = 1;
    if (!ARRAYP(ra) || !ARRAY_CONTP(ra)) return BOOL_F;
    for (k = 0; k < ndim; k++)
      len *= ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd + 1;
    if (!UNBNDP(strict)) {
      if (ndim && (1 != ARRAY_DIMS(ra)[ndim-1].inc)) return BOOL_F;
      if (tc7_Vbool==TYP7(ARRAY_V(ra))) {
	if (ARRAY_BASE(ra)%LONG_BIT) return BOOL_F;
	if (len != LENGTH(ARRAY_V(ra)) && len%LONG_BIT) return BOOL_F;
      }
    }
    if ((len==LENGTH(ARRAY_V(ra))) && 0==ARRAY_BASE(ra) && ARRAY_DIMS(ra)->inc)
      return ARRAY_V(ra);
    sra = make_ra(1);
    ARRAY_DIMS(sra)->lbnd = 0;
    ARRAY_DIMS(sra)->ubnd = len - 1;
    ARRAY_V(sra) = ARRAY_V(ra);
    ARRAY_BASE(sra) = ARRAY_BASE(ra);
    ARRAY_DIMS(sra)->inc = (ndim ? ARRAY_DIMS(ra)[ndim - 1].inc : 1);
    return sra;
  }
  }
}

static char s_uve_rd[] = "uniform-vector-read!";
SCM uve_read(v, port)
     SCM v, port;
{
  long sz, len, ans;
  long start=0;
  if (UNBNDP(port)) port = cur_inp;
  ASRTER(NIMP(port) && OPINFPORTP(port), port, ARG2, s_uve_rd);
  ASRTGO(NIMP(v), badarg1);
  len = LENGTH(v);
 loop:
  switch TYP7(v) {
  default: badarg1: wta(v, (char *)ARG1, s_uve_rd);
  case tc7_smob:
    v = array_contents(v, BOOL_T);
    ASRTGO(NIMP(v), badarg1);
    if (ARRAYP(v)) {
      array_dim *d = ARRAY_DIMS(v);
      start = ARRAY_BASE(v);
      len = d->inc * (d->ubnd - d->lbnd + 1);
      v = ARRAY_V(v);
    }
    else
      len = LENGTH(v);
    goto loop;
  case tc7_string:
    sz = sizeof(char);
    break;
  case tc7_Vbool:
    len = (len+LONG_BIT-1)/LONG_BIT;
    start /= LONG_BIT;
  case tc7_VfixN32:
  case tc7_VfixZ32:
    sz = sizeof(long);
    break;
  case tc7_VfixN16:
  case tc7_VfixZ16:
    sz = sizeof(short);
    break;
  case tc7_VfixN8:
  case tc7_VfixZ8:
    sz = sizeof(char);
    break;
# ifdef FLOATS
  case tc7_VfloR32:
    sz = sizeof(float);
    break;
  case tc7_VfloC32:
    sz = 2*sizeof(float);
    break;
  case tc7_VfloR64:
    sz = sizeof(double);
    break;
  case tc7_VfloC64:
    sz = 2*sizeof(double);
    break;
# endif
  }
  if (0==len) return INUM0;
  /* An ungetc before an fread will not work on some systems if setbuf(0),
     so we read one element char by char. */
  if (CRDYP(port)) {
    int i;
    for (i = 0; i < sz; i++)
      CHARS(v)[start*sz + i] = lgetc(port);
    start += 1;
    len -= 1;
  }
  SYSCALL(ans = fread(CHARS(v)+start*sz, (sizet)sz, (sizet)len, STREAM(port)););
  if (TYP7(v)==tc7_Vbool) ans *= LONG_BIT;
  return MAKINUM(ans);
}

static char s_uve_wr[] = "uniform-vector-write";
SCM uve_write(v, port)
     SCM v, port;
{
  long sz, len, ans;
  long start=0;
  if (UNBNDP(port)) port = cur_outp;
  ASRTER(NIMP(port) && OPOUTFPORTP(port), port, ARG2, s_uve_wr);
  ASRTGO(NIMP(v), badarg1);
  len = LENGTH(v);
 loop:
  switch TYP7(v) {
  default: badarg1: wta(v, (char *)ARG1, s_uve_wr);
  case tc7_smob:
    v = array_contents(v, BOOL_T);
    ASRTGO(NIMP(v), badarg1);
    if (ARRAYP(v)) {
      array_dim *d = ARRAY_DIMS(v);
      start = ARRAY_BASE(v);
      len = d->inc * (d->ubnd - d->lbnd + 1);
      v = ARRAY_V(v);
    }
    else
      len = LENGTH(v);
    goto loop;
  case tc7_string:
    sz = sizeof(char);
    break;
  case tc7_Vbool:
    len = (len+LONG_BIT-1)/LONG_BIT;
    start /= LONG_BIT;
  case tc7_VfixN32:
  case tc7_VfixZ32:
    sz = sizeof(long);
    break;
  case tc7_VfixN16:
  case tc7_VfixZ16:
    sz = sizeof(short);
    break;
  case tc7_VfixN8:
  case tc7_VfixZ8:
    sz = sizeof(char);
    break;
# ifdef FLOATS
  case tc7_VfloR32:
    sz = sizeof(float);
    break;
  case tc7_VfloC32:
    sz = 2*sizeof(float);
    break;
  case tc7_VfloR64:
    sz = sizeof(double);
    break;
  case tc7_VfloC64:
    sz = 2*sizeof(double);
    break;
# endif
  }
  ans = lfwrite(CHARS(v)+start*sz, (sizet)sz, (sizet)len, port);
  if (TYP7(v)==tc7_Vbool) ans *= LONG_BIT;
  return MAKINUM(ans);
}

static char cnt_tab[16] = {0, 1, 1, 2, 1, 2, 2, 3, 1, 2, 2, 3, 2, 3, 3, 4};
static char s_count[] = "bit-count";
SCM lcount(item, seq)
     SCM item, seq;
{
  long i, imin, ubnd, lbnd = 0;
  int enclosed = 0;
  register unsigned long cnt = 0, w;
  ASRTER(NIMP(seq), seq, ARG2, s_count);
  ubnd = LENGTH(seq) - 1;
 tail:
  switch TYP7(seq) {
  default: badarg2: wta(seq, (char *)ARG2, s_count);
  case tc7_Vbool:
    if (lbnd>ubnd) return INUM0;
    i = ubnd/LONG_BIT;
    imin = lbnd/LONG_BIT;
    w = VELTS(seq)[i];
    if (FALSEP(item)) w = ~w;
    w <<= LONG_BIT-1-(ubnd%LONG_BIT);
    w >>= LONG_BIT-1-(ubnd%LONG_BIT);   /* There may be only a partial word. */
    while (imin < i--) {
      for (;w;w >>= 4) cnt += cnt_tab[w & 0x0f];
      w = VELTS(seq)[i];
      if (FALSEP(item)) w = ~w;
    }
    w >>= (lbnd%LONG_BIT);
    for (;w;w >>= 4) cnt += cnt_tab[w & 0x0f];
    return MAKINUM(cnt);
  case tc7_smob:
    ASRTGO(ARRAYP(seq) && 1==ARRAY_NDIM(seq) && 0==enclosed++, badarg2);
    {
      long n, inc = ARRAY_DIMS(seq)->inc;
      switch (inc) {
      default:
	i = ARRAY_BASE(seq);
	n = ARRAY_DIMS(seq)->ubnd - ARRAY_DIMS(seq)->lbnd + 1;
	if (n<=0) return INUM0;
	seq = ARRAY_V(seq);
	if (FALSEP(item)) {
	  for (;n--; i+=inc)
	    if (!((VELTS(seq)[i/LONG_BIT]) & (1L<<(i%LONG_BIT)))) cnt++;
	}
	else
	  for (;n--; i+=inc)
	    if ((VELTS(seq)[i/LONG_BIT]) & (1L<<(i%LONG_BIT))) cnt++;
	return MAKINUM(cnt);
      case 1:
	lbnd = ARRAY_BASE(seq);
	ubnd = lbnd + (ARRAY_DIMS(seq)->ubnd - ARRAY_DIMS(seq)->lbnd)*inc;
	seq = ARRAY_V(seq);
	goto tail;
      case -1:
	ubnd = ARRAY_BASE(seq);
	lbnd = ubnd + (ARRAY_DIMS(seq)->ubnd - ARRAY_DIMS(seq)->lbnd)*inc;
	seq = ARRAY_V(seq);
	goto tail;
      }
    }
  }
}
static char s_uve_pos[] = "bit-position";
SCM bit_position(item, v, k)
     SCM item, v, k;
{
  long i, len, lenw, xbits, pos = INUM(k), offset = 0;
  int enclosed = 0;
  register unsigned long w;
  ASRTER(NIMP(v), v, ARG2, s_uve_pos);
  ASRTER(INUMP(k), k, ARG3, s_uve_pos);
  len = LENGTH(v);
 tail:
  switch TYP7(v) {
  default: badarg2: wta(v, (char *)ARG2, s_uve_pos);
  case tc7_Vbool:
    ASRTER((pos <= len) && (pos >= 0), k, OUTOFRANGE, s_uve_pos);
    if (pos==len) return BOOL_F;
    if (0==len) return MAKINUM(-1L);
    lenw = (len-1)/LONG_BIT; /* watch for part words */
    i = pos/LONG_BIT;
    w = VELTS(v)[i];
    if (FALSEP(item)) w = ~w;
    xbits = (pos%LONG_BIT);
    pos -= xbits;
    w = ((w >> xbits) << xbits);
    xbits = LONG_BIT-1-(len-1)%LONG_BIT;
    while (!0) {
      if (w && (i==lenw))
	w = ((w << xbits) >> xbits);
      if (w) while (w) switch (w & 0x0f)
	{
	default: return MAKINUM(pos-offset);
	case 2: case 6: case 10: case 14: return MAKINUM(pos+1-offset);
	case 4: case 12: return MAKINUM(pos+2-offset);
	case 8: return MAKINUM(pos+3-offset);
	case 0: pos += 4; w >>= 4;
	}
      if (++i > lenw) break;
      pos += LONG_BIT;
      w = VELTS(v)[i];
      if (FALSEP(item)) w = ~w;
    }
    return BOOL_F;
  case tc7_smob: ASRTGO(ARRAYP(v) && 1==ARRAY_NDIM(v) && !enclosed++, badarg2);
    ASRTER(pos >= ARRAY_DIMS(v)->lbnd, k, OUTOFRANGE, s_uve_pos);
    if (1==ARRAY_DIMS(v)->inc) {
      len = ARRAY_DIMS(v)->ubnd - ARRAY_DIMS(v)->lbnd + ARRAY_BASE(v) + 1;
      offset = ARRAY_BASE(v) - ARRAY_DIMS(v)->lbnd;
      pos += offset;
      v = ARRAY_V(v);
      goto tail;
    }
    else {
      long inc = ARRAY_DIMS(v)->inc;
      long ubnd = ARRAY_DIMS(v)->ubnd;
      if (ubnd < ARRAY_DIMS(v)->lbnd)
	return MAKINUM(ARRAY_DIMS(v)->lbnd - 1);
      i = ARRAY_BASE(v) + (pos - ARRAY_DIMS(v)->lbnd)*inc;
      v = ARRAY_V(v);
      for (; pos <= ubnd; pos++) {
	if (item ==
	    ((VELTS(v)[i/LONG_BIT])&(1L<<(i%LONG_BIT)) ? BOOL_T : BOOL_F))
	  return MAKINUM(pos);
	i += inc;
      }
    return BOOL_F;
    }
  }
}

static char s_bit_set[] = "bit-set*!";
SCM bit_set(v, kv, obj)
     SCM v, kv, obj;
{
  register long i, k, vlen;
  ASRTGO(NIMP(v), badarg1);
  ASRTGO(NIMP(kv), badarg2);
  switch TYP7(kv) {
    default: badarg2: wta(kv, (char *)ARG2, s_bit_set);
    case tc7_VfixN32:
      switch TYP7(v) {
	default: badarg1: wta(v, (char *)ARG1, s_bit_set);
	case tc7_Vbool:
	  vlen = LENGTH(v);
	  if (BOOL_F==obj) for (i = LENGTH(kv);i;) {
	    k = VELTS(kv)[--i];
	    ASRTER((k < vlen), MAKINUM(k), OUTOFRANGE, s_bit_set);
	    VELTS(v)[k/LONG_BIT] &= ~(1L<<(k%LONG_BIT));
	  }
	  else if (BOOL_T==obj) for (i = LENGTH(kv); i;) {
	    k = VELTS(kv)[--i];
	    ASRTER((k < vlen), MAKINUM(k), OUTOFRANGE, s_bit_set);
	    VELTS(v)[k/LONG_BIT] |= (1L<<(k%LONG_BIT));
	  }
	  else
	  badarg3: wta(obj, (char *)ARG3, s_bit_set);
	}
      break;
    case tc7_Vbool:
      ASRTGO(TYP7(v)==tc7_Vbool && LENGTH(v)==LENGTH(kv), badarg1);
      if (BOOL_F==obj)
	for (k = (LENGTH(v)+LONG_BIT-1)/LONG_BIT;k--;)
	  VELTS(v)[k] &= ~(VELTS(kv)[k]);
      else if (BOOL_T==obj)
	for (k = (LENGTH(v)+LONG_BIT-1)/LONG_BIT;k--;)
	  VELTS(v)[k] |= VELTS(kv)[k];
      else goto badarg3;
      break;
    }
  return UNSPECIFIED;
}

static char s_bit_count[] = "bit-count*";
SCM bit_count(v, kv, obj)
     SCM v, kv, obj;
{
  register long i, vlen, count = 0;
  register unsigned long k;
  ASRTGO(NIMP(v), badarg1);
  ASRTGO(NIMP(kv), badarg2);
  switch TYP7(kv) {
    default: badarg2: wta(kv, (char *)ARG2, s_bit_count);
    case tc7_VfixN32:
      switch TYP7(v) {
	default: badarg1: wta(v, (char *)ARG1, s_bit_count);
	case tc7_Vbool:
	  vlen = LENGTH(v);
	  if (BOOL_F==obj) for (i = LENGTH(kv);i;) {
	    k = VELTS(kv)[--i];
	    ASRTER((k < vlen), MAKINUM(k), OUTOFRANGE, s_bit_count);
	    if (!(VELTS(v)[k/LONG_BIT] & (1L<<(k%LONG_BIT)))) count++;
	  }
	  else if (BOOL_T==obj) for (i = LENGTH(kv); i;) {
	    k = VELTS(kv)[--i];
	    ASRTER((k < vlen), MAKINUM(k), OUTOFRANGE, s_bit_count);
	    if (VELTS(v)[k/LONG_BIT] & (1L<<(k%LONG_BIT))) count++;
	  }
	  else
	  badarg3: wta(obj, (char *)ARG3, s_bit_count);
	}
      break;
    case tc7_Vbool:
      ASRTGO(TYP7(v)==tc7_Vbool && LENGTH(v)==LENGTH(kv), badarg1);
      if (0==LENGTH(v)) return INUM0;
      ASRTGO(BOOL_T==obj || BOOL_F==obj, badarg3);
      obj = (BOOL_T==obj);
      i = (LENGTH(v)-1)/LONG_BIT;
      k = VELTS(kv)[i] & (obj ? VELTS(v)[i] : ~VELTS(v)[i]);
      k <<= LONG_BIT-1-((LENGTH(v)-1)%LONG_BIT);
      while (!0) {
	for (;k;k >>= 4) count += cnt_tab[k & 0x0f];
	if (0==i--) return MAKINUM(count);
	k = VELTS(kv)[i] & (obj ? VELTS(v)[i] : ~VELTS(v)[i]);
      }
    }
  return MAKINUM(count);
}

static char s_bit_inv[] = "bit-invert!";
SCM bit_inv(v)
     SCM v;
{
  register long k;
  ASRTGO(NIMP(v), badarg1);
  k = LENGTH(v);
  switch TYP7(v) {
  case tc7_Vbool:
    for (k = (k+LONG_BIT-1)/LONG_BIT;k--;)
      VELTS(v)[k] = ~VELTS(v)[k];
    break;
  default: badarg1: wta(v, (char *)ARG1, s_bit_inv);
  }
  return UNSPECIFIED;
}

static char s_strup[] = "string-upcase!";
SCM strup(v)
     SCM v;
{
  register long k;
  register unsigned char *cs;
  ASRTGO(NIMP(v), badarg1);
  k = LENGTH(v);
  switch TYP7(v) {
  case tc7_string:
    cs = UCHARS(v);
    while (k--) cs[k] = upcase[cs[k]];
    break;
  default: badarg1: wta(v, (char *)ARG1, s_strup);
  }
  return v;
}

static char s_strdown[] = "string-downcase!";
SCM strdown(v)
     SCM v;
{
  register long k;
  register unsigned char *cs;
  ASRTGO(NIMP(v), badarg1);
  k = LENGTH(v);
  switch TYP7(v) {
  case tc7_string:
    cs = UCHARS(v);
    while (k--) cs[k] = downcase[cs[k]];
    break;
  default: badarg1: wta(v, (char *)ARG1, s_strdown);
  }
  return v;
}

# include <ctype.h>
static char s_strcap[] = "string-capitalize!";
SCM strcap(v)
     SCM v;
{
  long i = 0, len;
  register unsigned char *str;
  register int non_first_alpha = 0;
  ASRTGO(NIMP(v), badarg1);
  len = LENGTH(v);
  switch TYP7(v) {
  case tc7_string:
    for (str = UCHARS(v);i < len; i++) {
      int c = str[i];
      if (isascii(c) && isalpha(c))
	if (non_first_alpha) str[i] = downcase[c];
	else {
	  non_first_alpha = !0;
	  str[i] = upcase[c];
	}
      else non_first_alpha = 0;
    }
    break;
  default: badarg1: wta(v, (char *)ARG1, s_strcap);
  }
  return v;
}

SCM istr2bve(str, len)
     char *str;
     long len;
{
  SCM v = make_uve(len, BOOL_T);
  long *data = (long *)VELTS(v);
  register unsigned long mask;
  register long k;
  register long j;
  for (k = 0; k < (len+LONG_BIT-1)/LONG_BIT; k++) {
    data[k] = 0L;
    j = len - k*LONG_BIT;
    if (j > LONG_BIT) j = LONG_BIT;
    for (mask = 1L; j--; mask <<= 1)
      switch (*str++) {
      case '0': break;
      case '1': data[k] |= mask; break;
      default:  return BOOL_F;
      }
  }
  return v;
}

static SCM ra2l(ra, base, k)
     SCM ra;
     sizet base;
     sizet k;
{
  register SCM res = EOL;
  register long inc = ARRAY_DIMS(ra)[k].inc;
  register sizet i;
  if (ARRAY_DIMS(ra)[k].ubnd < ARRAY_DIMS(ra)[k].lbnd) return EOL;
  i = base + (1 + ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd)*inc;
  if (k < ARRAY_NDIM(ra) - 1) {
    do {
      i -= inc;
      res = cons(ra2l(ra, i, k+1), res);
    } while (i != base);
  }
  else
    do {
      i -= inc;
      res = cons(cvref(ARRAY_V(ra), i, UNDEFINED), res);
    } while (i != base);
  return res;
}

static char s_array2list[] = "array->list";
SCM array2list(v)
     SCM v;
{
  SCM res = EOL;
  register long k;
  ASRTGO(NIMP(v), badarg1);
  switch TYP7(v) {
  default:
    if (BOOL_T==arrayp(v, UNDEFINED)) {
      for (k = LENGTH(v) - 1; k >= 0; k--)
	res = cons(cvref(v, k, UNDEFINED), res);
      return res;
    }
  badarg1: wta(v, (char *)ARG1, s_array2list);
  case tc7_smob: ASRTGO(ARRAYP(v), badarg1);
    if (0==ARRAY_NDIM(v)) return aref(v, EOL);
    return ra2l(v, ARRAY_BASE(v), 0);
  case tc7_vector: return vector2list(v);
  case tc7_string: return string2list(v);
  case tc7_Vbool: {
    long *data = (long *)VELTS(v);
    register unsigned long mask;
    for (k = (LENGTH(v)-1)/LONG_BIT; k > 0; k--)
      for (mask = 1L<<(LONG_BIT-1); mask; mask >>=1)
	res = cons(((long *)data)[k] & mask ? BOOL_T : BOOL_F, res);
    for (mask = 1L<<((LENGTH(v)%LONG_BIT)-1); mask; mask >>=1)
      res = cons(((long *)data)[k] & mask ? BOOL_T : BOOL_F, res);
    return res;
  }
# ifdef INUMS_ONLY
  case tc7_VfixN32:
  case tc7_VfixZ32: {
    long *data = (long *)VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(MAKINUM(data[k]), res);
    return res;
  }
# else
  case tc7_VfixN32: {
    long *data = (long *)VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(ulong2num(data[k]), res);
    return res;
  }
  case tc7_VfixZ32: {
    long *data = (long *)VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(long2num(data[k]), res);
    return res;
  }
# endif
# ifdef FLOATS
  case tc7_VfloR32: {
    float *data = (float *)VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(makflo(data[k]), res);
    return res;
  }
  case tc7_VfloC32: {
    float (*data)[2] = (float (*)[2])VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(makdbl(data[k][0], data[k][1]), res);
    return res;
  }
  case tc7_VfloR64: {
    double *data = (double *)VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(makdbl(data[k], 0.0), res);
    return res;
  }
  case tc7_VfloC64: {
    double (*data)[2] = (double (*)[2])VELTS(v);
    for (k = LENGTH(v) - 1; k >= 0; k--)
      res = cons(makdbl(data[k][0], data[k][1]), res);
    return res;
  }
# endif /*FLOATS*/
  }
}

static int l2ra P((SCM lst, SCM ra, sizet base, sizet k));
static char s_bad_ralst[] = "Bad array contents list";
static char s_list2ura[] = "list->uniform-array";
SCM list2ura(ndim, prot, lst)
     SCM ndim;
     SCM prot;
     SCM lst;
{
  SCM shp=EOL;
  SCM row=lst;
  SCM ra;
  long n;
  int k = INUM(ndim);
  ASRTER(INUMP(ndim) && k >= 0, ndim, ARG1, s_list2ura);
  for (; --k >= 0 ; (NIMP(row) && (row = CAR(row)))) {
    n = ilength(row);
    ASRTER(n>=0, lst, ARG3, s_list2ura);
    shp = cons(MAKINUM(n), shp);
  }
  ra = dims2ura(reverse(shp), prot, EOL);
  if (!ARRAYP(ra)) {
    for (k = 0; k < LENGTH(ra); k++, lst = CDR(lst))
      aset(ra, CAR(lst), MAKINUM(k));
    return ra;
  }
  if (NULLP(shp)) {
    aset(ra, lst, EOL);
    return ra;
  }
  if (l2ra(lst, ra, ARRAY_BASE(ra), 0))
    return ra;
  else
    wta(lst, s_bad_ralst, s_list2ura);
  return BOOL_F;
}

static int l2ra(lst, ra, base, k)
     SCM lst;
     SCM ra;
     sizet base;
     sizet k;
{
  register long inc = ARRAY_DIMS(ra)[k].inc;
  register long n = (1 + ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd);
  int ok = 1;
  if (n <= 0) return (EOL==lst);
  if (k < ARRAY_NDIM(ra) - 1) {
    while (n--) {
      if (IMP(lst) || NCONSP(lst)) return 0;
      ok = ok && l2ra(CAR(lst), ra, base, k+1);
      base += inc;
      lst = CDR(lst);
    }
    if (NNULLP(lst)) return 0;
  }
  else {
    while (n--) {
      if (IMP(lst) || NCONSP(lst)) return 0;
      ok = ok && aset(ARRAY_V(ra), CAR(lst), MAKINUM(base));
      base += inc;
      lst = CDR(lst);
    }
    if (NNULLP(lst)) return 0;
  }
  return ok;
}

static void rapr1(ra, j, k, port, writing)
     SCM ra;
     sizet j;
     sizet k;
     SCM port;
     int writing;
{
  long inc = 1;
  long n = LENGTH(ra);
  int enclosed = 0;
 tail:
   switch TYP7(ra) {
   case tc7_smob:
     if (enclosed++) {
       ARRAY_BASE(ra) = j;
       if (n-- > 0) scm_iprin1(ra, port, writing);
       for (j += inc; n-- > 0; j += inc) {
	 lputc(' ', port);
	 ARRAY_BASE(ra) = j;
	 scm_iprin1(ra, port, writing);
       }
       break;
     }
     if (k+1 < ARRAY_NDIM(ra)) {
       long i;
       inc = ARRAY_DIMS(ra)[k].inc;
       for (i = ARRAY_DIMS(ra)[k].lbnd; i < ARRAY_DIMS(ra)[k].ubnd; i++) {
	 lputc('(', port);
	 rapr1(ra, j, k+1, port, writing);
	 lputs(") ", port);
	 j += inc;
       }
       if (i==ARRAY_DIMS(ra)[k].ubnd) { /* could be zero size. */
	 lputc('(', port);
	 rapr1(ra, j, k+1, port, writing);
	 lputc(')', port);
       }
       break;
     }
     if (ARRAY_NDIM(ra)) {	/* Could be zero-dimensional */
       inc = ARRAY_DIMS(ra)[k].inc;
       n = (ARRAY_DIMS(ra)[k].ubnd - ARRAY_DIMS(ra)[k].lbnd + 1);
     }
     else
       n = 1;
     ra = ARRAY_V(ra);
     goto tail;
   default:
     if (n-- > 0) scm_iprin1(cvref(ra, j, UNDEFINED), port, writing);
     for (j += inc; n-- > 0; j += inc) {
       lputc(' ', port);
       scm_iprin1(cvref(ra, j, UNDEFINED), port, writing);
     }
     break;
   case tc7_string:
     if (n-- > 0) scm_iprin1(MAKICHR(CHARS(ra)[j]), port, writing);
     if (writing)
       for (j += inc; n-- > 0; j += inc) {
	 lputc(' ', port);
	 scm_iprin1(MAKICHR(CHARS(ra)[j]), port, writing);
       }
     else
       for (j += inc; n-- > 0; j += inc)
	 lputc(CHARS(ra)[j], port);
     break;
   case tc7_VfixN32:
     if (errjmp_bad) {
       scm_ipruk("VfixN32", ra, port);
       break;
     }
     if (n-- > 0) scm_intprint(VELTS(ra)[j], -10, port);
     for (j += inc; n-- > 0; j += inc) {
       lputc(' ', port);
       scm_intprint(VELTS(ra)[j], -10, port);
     }
     break;
   case tc7_VfixZ32:
     if (n-- > 0) scm_intprint(VELTS(ra)[j], 10, port);
     for (j += inc; n-- > 0; j += inc) {
       lputc(' ', port);
       scm_intprint(VELTS(ra)[j], 10, port);
     }
     break;
# ifdef FLOATS
   case tc7_VfloR32:
   case tc7_VfloC32:
   case tc7_VfloR64:
   case tc7_VfloC64:
     if (n-- > 0) {
       SCM z = cvref(ra, j, UNDEFINED);
       floprint(z, port, writing);
       for (j += inc; n-- > 0; j += inc) {
	 lputc(' ', port);
	 z = cvref(ra, j, z);
	 floprint(z, port, writing);
       }
     }
     break;
# endif /*FLOATS*/
   }
}
int raprin1(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  SCM v = exp;
  sizet base = 0;
  lputc('#', port);
 tail:
  switch TYP7(v) {
  case tc7_smob: {
    long ndim = ARRAY_NDIM(v);
    base = ARRAY_BASE(v);
    v = ARRAY_V(v);
    if (ARRAYP(v)) {
      lputs("<enclosed-array ", port);
      rapr1(exp, base, 0, port, writing);
      lputc('>', port);
      return 1;
    }
    else {
      scm_intprint(ndim, 10, port);
      goto tail;
    }
  }
  case tc7_Vbool:
    if (exp==v) {		/* a uve, not an array */
      register long i, j, w;
      lputc('*', port);
      for (i = 0;i<(LENGTH(exp))/LONG_BIT;i++) {
	w = VELTS(exp)[i];
	for (j = LONG_BIT;j;j--) {
	  lputc(w&1?'1':'0', port);
	  w >>= 1;
	}
      }
      j = LENGTH(exp)%LONG_BIT;
      if (j) {
	w = VELTS(exp)[LENGTH(exp)/LONG_BIT];
	for (;j;j--) {
	  lputc(w&1?'1':'0', port);
	  w >>= 1;
	}
      }
      return 1;
    }
  default:
    if (exp==v) lputc('1', port);
    switch TYP7(v) {
    case tc7_Vbool:
      lputs("A:bool", port); break;
    case tc7_vector:
      lputc('A', port); break;
    case tc7_string:
      lputs("A:char", port); break;
    case tc7_VfixN64:
      lputs("A:fixN64b", port); break;
    case tc7_VfixZ64:
      lputs("A:fixZ64b", port); break;
    case tc7_VfixN32:
      lputs("A:fixN32b", port); break;
    case tc7_VfixZ32:
      lputs("A:fixZ32b", port); break;
    case tc7_VfixN16:
      lputs("A:fixN16b", port); break;
    case tc7_VfixZ16:
      lputs("A:fixZ16b", port); break;
    case tc7_VfixN8:
      lputs("A:fixN8b", port); break;
    case tc7_VfixZ8:
      lputs("A:fixZ8b", port); break;
# ifdef FLOATS
    case tc7_VfloR32:
      lputs("A:floR32b", port); break;
    case tc7_VfloC32:
      lputs("A:floC32b", port); break;
    case tc7_VfloR64:
      lputs("A:floR64b", port); break;
    case tc7_VfloC64:
      lputs("A:floC64b", port); break;
# endif /*FLOATS*/
    }
  }
  if ((v != exp) && 0==ARRAY_NDIM(exp)) {
    lputc(' ', port);
    scm_iprin1(aref(exp, EOL), port, writing);
  }
  else {
    lputc('(', port);
    rapr1(exp, base, 0, port, writing);
    lputc(')', port);
  }
  return 1;
}

static char s_array_prot[] = "array-prototype";
SCM array_prot(ra)
     SCM ra;
{
  int enclosed = 0;
  ASRTGO(NIMP(ra), badarg);
 loop:
  switch TYP7(ra) {
  default: badarg: wta(ra, (char *)ARG1, s_array_prot);
  case tc7_smob: ASRTGO(ARRAYP(ra), badarg);
    if (enclosed++) return UNSPECIFIED;
    ra = ARRAY_V(ra);
    goto loop;
  case tc7_vector: return EOL;
  case tc7_Vbool: return BOOL_T;
  case tc7_string: return MAKICHR('a');
  case tc7_VfixN64: return MAKINUM(64L);
  case tc7_VfixZ64: return MAKINUM(-64L);
  case tc7_VfixN32: return MAKINUM(32L);
  case tc7_VfixZ32: return MAKINUM(-32L);
  case tc7_VfixN16: return MAKINUM(16L);
  case tc7_VfixZ16: return MAKINUM(-16L);
  case tc7_VfixN8: return MAKINUM(8L);
  case tc7_VfixZ8: return MAKINUM(-8L);
# ifdef FLOATS
  case tc7_VfloR32: return makflo(32.0);
  case tc7_VfloC32: return makdbl(0.0, 32.0);
  case tc7_VfloR64: return makdbl(64.0, 0.0);
  case tc7_VfloC64: return makdbl(0.0, 64.0);
# endif
  }
}

/* Looks like ARRAY-REF, if just enough indices are provided,
   If one extra is provided then the last index specifies bit
   position in an integer element.
*/
static char s_logaref[] = "logaref";
SCM scm_logaref(args)
     SCM args;
{
  SCM ra, inds, ibit;
  int  i, rank = 1;
  ASRTER(NIMP(args), UNDEFINED, WNA, s_logaref);
  ra = CAR(args);
  ASRTER(NIMP(ra), ra, ARG1, s_logaref);
  if (ARRAYP(ra)) rank = ARRAY_NDIM(ra);
  inds = args = CDR(args);
  for (i = rank; i; i--) {
    ASRTER(NIMP(args) && CONSP(args), UNDEFINED, WNA, s_logaref);
    args = CDR(args);
  }
  if (NULLP(args)) return aref(ra, inds);
  ASRTER(NIMP(args) && CONSP(args) && NULLP(CDR(args)),
	 inds, WNA, s_logaref);
  ASRTER(INUMP(CAR(args)), CAR(args), ARGn, s_logaref);
  ibit = CAR(args);
  if (1==rank)
    inds = CAR(inds);
  else {			/* Destructively modify arglist */
    args = inds;
    for (i = rank-1; i; i--) {
      ASRTER(NIMP(args) && CONSP(args), UNDEFINED, WNA, s_logaref);
      args = CDR(args);
    }
    CDR(args) = EOL;
  }
  args = aref(ra, inds);
  return INUMP(args) ?
    ((1<<INUM(ibit)) & INUM(args) ? BOOL_T : BOOL_F) :
    scm_logbitp(ibit, args);
}

static char s_logaset[] = "logaset!";
SCM scm_logaset(ra, obj, args)
     SCM ra, obj, args;
{
  SCM oval, inds, ibit;
  int  i, rank = 1;
  ASRTER(NIMP(ra), ra, ARG1, s_logaset);
  if (ARRAYP(ra)) rank = ARRAY_NDIM(ra);
  inds = args;
  for (i = rank; i; i--) {
    ASRTER(NIMP(args) && CONSP(args), UNDEFINED, WNA, s_logaset);
    args = CDR(args);
  }
  if (NNULLP(args)) {
    ASRTER(NIMP(args) && CONSP(args) && NULLP(CDR(args)),
	   inds, WNA, s_logaset);
    ASRTER(INUMP(CAR(args)), CAR(args), ARGn, s_logaset);
    ibit = CAR(args);
    if (1==rank) inds = CAR(inds);
    else {			/* Destructively modify arglist */
      args = inds;
      for (i = rank-1; i; i--) {
	ASRTER(NIMP(args) && CONSP(args), UNDEFINED, WNA, s_logaset);
	args = CDR(args);
      }
      CDR(args) = EOL;
    }
    oval = aref(ra, inds);
    ASRTER(INUMP(ibit), ibit, ARGn, s_logaset);
    if (BOOL_T==obj)
      obj = INUMP(oval) ? MAKINUM(INUM(oval) | (1<<INUM(ibit))) :
	scm_logior(oval, MAKINUM(1<<INUM(ibit)));
    else if (BOOL_F==obj)
      obj = INUMP(oval) ? MAKINUM(INUM(oval) & (~(1<<INUM(ibit)))) :
	scm_logand(oval, MAKINUM(~(1<<INUM(ibit))));
# ifndef RECKLESS
    else wta(obj, (char *)ARG2, s_logaset);
# endif
  }
  return aset(ra, obj, inds);
}

static iproc subr3s[] = {
	{s_uve_pos, bit_position},
	{s_bit_set, bit_set},
	{s_bit_count, bit_count},
	{s_list2ura, list2ura},
	{0, 0}};

static iproc subr2s[] = {
	{s_resizuve, resizuve},
	{s_count, lcount},
	{0, 0}};

static iproc subr1s[] = {
	{"array-rank", array_rank},
	{s_array_dims, array_dims},
	{s_array2list, array2list},
	{s_bit_inv, bit_inv},
	{s_strdown, strdown},
	{s_strcap, strcap},
	{s_strup, strup},
	{s_array_prot, array_prot},
	{0, 0}};

static iproc lsubrs[] = {
  {s_aref, scm_array_ref},
  {s_trans_array, trans_array},
  {s_encl_array, encl_array},
  {s_array_inbp, array_inbp},
  {s_logaref, scm_logaref},
  {0, 0}};

static iproc lsubr2s[] = {
  {s_make_sh_array, make_sh_array},
  {s_dims2ura, dims2ura},
  {s_aset, aset},
  {s_logaset, scm_logaset},
  {0, 0}};

static iproc subr2os[] = {
  {"array?", arrayp},
  {s_array_contents, array_contents},
  {0, 0}};

static SCM markra(ptr)
     SCM ptr;
{
  return ARRAY_V(ptr);
}
static sizet freera(ptr)
     CELLPTR ptr;
{
  must_free(CHARS((SCM)ptr),
	    sizeof(array) + ARRAY_NDIM((SCM)ptr)*sizeof(array_dim));
  return 0;
}
static smobfuns rasmob = {markra, freera, raprin1, 0};
				/* 0 replaced by raequal in init_ramap() */

/* This must be done after init_scl() */
void init_unif()
{
  init_iprocs(subr3s, tc7_subr_3);
  init_iprocs(subr2s, tc7_subr_2);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(lsubrs, tc7_lsubr);
  init_iprocs(lsubr2s, tc7_lsubr_2);
  init_iprocs(subr2os, tc7_subr_2o);
  tc16_array = newsmob(&rasmob);
  add_feature(s_array);
}

#else /* ARRAYS */

int raprin1(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  return 0;
}

SCM istr2bve(str, len)
     char *str;
     long len;
{
  return BOOL_F;
}

SCM array_equal(ra0, ra1)
     SCM ra0, ra1;
{
  return BOOL_F;
}

void init_unif()
{
  make_subr(s_resizuve, tc7_subr_2, resizuve);
}

#endif /* ARRAYS */
