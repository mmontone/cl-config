/* "scl.c" non-IEEE utility functions and non-integer arithmetic.
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1997, 2005, 2006, 2013, 2014 Free Software Foundation, Inc.
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

/* Authors: Jerry D. Hedden and Aubrey Jaffer */

#include "scm.h"

#ifdef FLOATS
# ifndef PLAN9
#  include <math.h>
# endif

static SCM bigdblop P((int op, SCM b, double re, double im));
static SCM inex_divintbig P((SCM a, SCM b));
# ifndef BIGDIG
static int apx_log10 P((double x));
static double lpow10 P((double x, int n));
# endif
static sizet idbl2str P((double f, char *a));
static sizet pdbl2str P((double f, char *a, sizet ch));
static sizet iflo2str P((SCM flt, char *str));
static void safe_add_1 P((double f, double *fsum));
static long scm_twos_power P((SCM n));
static double mantexp2dbl P((SCM manstr, int expo));
static double pmantexp2dbl P((SCM bmant, int expo));
static SCM ex2in P((SCM z));
static SCM ilog P((unsigned long m, SCM b, SCM k, unsigned long *n));
static double dpows5[23];

static char s_makrect[] = "make-rectangular", s_makpolar[] = "make-polar",
	    s_magnitude[] = "magnitude", s_angle[] = "angle",
	    s_real_part[] = "real-part", s_imag_part[] = "imag-part",
	    s_in2ex[] = "inexact->exact",s_ex2in[] = "exact->inexact";

static char s_expt[] = "real-expt", s_atan2[] = "$atan2";
#endif
static char s_memv[] = "memv", s_assv[] = "assv";

SCM sys_protects[NUM_PROTECTS];
sizet num_protects = NUM_PROTECTS;

char		s_inexactp[] = "inexact?";
static char     s_zerop[] = "zero?", s_abs[] = "abs",
		s_positivep[] = "positive?", s_negativep[] = "negative?";
static char     s_lessp[] = "<", s_grp[] = ">";
static char     s_leqp[] = "<=", s_greqp[] = ">=";
#define s_eqp (&s_leqp[1])
static char     s_max[] = "max", s_min[] = "min";
char		s_sum[] = "+", s_difference[] = "-", s_product[] = "*",
		s_divide[] = "/";
static char     s_number2string[] = "number->string",
		s_str2number[] = "string->number";

static char s_list_tail[] = "list-tail";
static char s_str2list[] = "string->list";
static char s_st_copy[] = "string-copy", s_st_fill[] = "string-fill!";
static char s_vect2list[] = "vector->list", s_ve_fill[] = "vector-fill!";
static char str_inf0[] = "inf.0", str_nan0[] = "nan.0", str_0[] = "0.";
static char s_intexpt[] = "integer-expt", s_cintlog[] = "ceiling-integer-log";
static char s_dfloat_parts[] = "double-float-parts";
#define s_intlog (&s_cintlog[8])

/*** NUMBERS -> STRINGS ***/
#ifdef FLOATS
static int dbl_mant_dig = 0;
static double max_dbl_int;       /* Integers less than or equal to max_dbl_int
				    are representable exactly as doubles. */

int inf2str(f, a)
     double f;
     char *a;
{
  sizet ch = 0;
  if (f < 0.0) a[ch++] = '-';
  else if (f > 0.0) a[ch++] = '+';
  else {
    a[ch++] = '+';
    strcpy(&a[ch], str_nan0);
    return ch+sizeof(str_nan0)-1;
  }
  strcpy(&a[ch], str_inf0);
  return ch+sizeof(str_inf0)-1;
}

/* idbl2str() is the top level double-to-string conversion */
static sizet idbl2str(f, a)
     double f;
     char *a;
{
  sizet ch = 0;
  if (f==0.0) {strcpy(a, str_0); return sizeof(str_0)-1;}
  if (f==2*f) return inf2str(f, a);
  if (f < 0.0) {f = -f;a[ch++]='-';}
  else if (f > 0.0) ;
  else return inf2str(f, a);
  return pdbl2str(f, a, ch);
}

static double llog2 = .30102999566398114; /* log10(2) */

/* There are also temporary strings used in number->string conversion. */
void strrecy(str)
     SCM str;
{
  if (IMP(str) || !STRINGP(str)) return;
  DEFER_INTS;
  must_free(CHARS(str), (sizet)LENGTH(str));
  CAR(str) = MAKINUM(1);
  CDR(str) = INUM0;
  ALLOW_INTS;
}

# ifdef BIGDIG

/* The useful extent of bignums used in floating-point conversions is */
/* limited.  Recycle them explicitly, rather than waiting for GC. */

void bigrecy(bgnm)
     SCM bgnm;
{
  if (IMP(bgnm) || !BIGP(bgnm)) return;
  DEFER_INTS;
  must_free(CHARS(bgnm), (sizet)NUMDIGS(bgnm)*sizeof(BIGDIG));
  CAR(bgnm) = INUM0;
  CDR(bgnm) = INUM0;
  ALLOW_INTS;
}

/* can convert to string accurately with bignums */
/* f > 0 */
/* DBL_MIN_EXP = -1021 */
/* dbl_mant_dig = 53 */
static sizet pdbl2str(f, a, ch)
     double f;
     char *a;
     sizet ch;
{
  SCM mant, quo, num;
  sizet dp = ch;
  int e2, point, ndig = dbl_mant_dig;
  /* code from scm_dfloat_parts() */
  double dman = frexp(f, &e2);
#  ifdef DBL_MIN_EXP
  if (e2 < DBL_MIN_EXP)
    ndig -= DBL_MIN_EXP - e2;
#  endif
  e2 -= ndig;
  mant = dbl2big(ldexp(dman, ndig));
  point = ceil(e2*llog2);
  /* if (scm_verbose > 1) printf("mantissa = %g -> #x%s; e2 = %d -> %d; point = %d; ndig = %d -> %d\n", dman, CHARS(number2string(mant, MAKINUM(16))), e2+ndig, e2, point, dbl_mant_dig, ndig); */
  if (e2 >= 0) {
    /* try first with starved precision */
    {
      num = scm_ash(mant, MAKINUM(e2 - point));
      quo = scm_round_quotient(num, VELTS(pows5)[(long) point]);
      if (pmantexp2dbl(quo, point) != f) {
	int po2 = MAKINUM(1)==scm_logcount(mant);
	if (quo != num) { bigrecy(quo); quo = num; }
	num = scm_ash(quo, MAKINUM(1L));
	if (quo != num) bigrecy(quo);
	quo = scm_round_quotient(num, VELTS(pows5)[(long) --point]);
	if (po2 && pmantexp2dbl(quo,point) != f) {
	  SCM quo1 = sum(quo, MAKINUM(1L));
	  if (quo != quo1) { bigrecy(quo); quo = quo1; }
	  if (pmantexp2dbl(quo,point) != f) {
	    if (quo != num) { bigrecy(quo); quo = num; };
	    num = scm_ash(quo,MAKINUM(1L));
	    quo = scm_round_quotient(num, VELTS(pows5)[(long) --point]);
	  }
	}
      }
      if (num != quo) bigrecy(num);
    }
  } else {   /* e2 <= 0 */
    /* try first with starved precision */
    {
      SCM den = scm_ash(MAKINUM(1L), MAKINUM(point - e2));
      num = product(mant, VELTS(pows5)[- (long) point]);
      quo = scm_round_quotient(num, den);
      if (pmantexp2dbl(quo, point) != f) {
	int po2 = MAKINUM(1L)==scm_logcount(mant);
	if (quo != num) { bigrecy(quo); quo = num; }
	point--;
	num = product(quo, MAKINUM(10));
	/* if (quo != num) bigrecy(quo); */
	quo = scm_round_quotient(num, den);
	if (po2 && pmantexp2dbl(quo,point) != f) {
	  SCM quo1 = sum(quo, MAKINUM(1L));
	  if (quo != quo1) { bigrecy(quo); quo = quo1; }
	  if (pmantexp2dbl(quo,point) != f) {
	    if (quo != num) { bigrecy(quo); quo = num; }
	    point--;
	    num = product(quo, MAKINUM(10));
	    if (quo != num) bigrecy(quo);
	    quo = scm_round_quotient(num, den);
	  }
	}
      } else if (quo != num) bigrecy(num);
      bigrecy(den);
    }
  }
  if (mant != quo) bigrecy(mant);
  a[ch++] = '.';
  /* if (sizeof(UBIGLONG)>=sizeof(double)) /\* Is ulong larger than mantissa? *\/ */
  /*   ch += iulong2str(num2ulong(quo, (char *)ARG1, s_number2string), 10, &a[ch]); */
  /* else */ {
    SCM str = number2string(quo, MAKINUM(10));
    int len = LENGTH(str), i = 0;
    bigrecy(quo);
    point += len - 1;
    while (i < len) a[ch++] = CHARS(str)[i++];
    strrecy(str);
  }
  a[dp] = a[dp+1]; a[++dp] = '.';
#  ifdef ENGNOT
  while ((dp+1 < ch) && (point+9999)%3) { a[dp] = a[dp+1]; a[++dp] = '.'; point--; }
#  endif	/* ENGNOT */
  while ('0'==a[--ch]); ch++;
  if (point != 0) {
    a[ch++] = 'e';
    return ch + ilong2str(point, 10, &a[ch]);
  } else return ch;
}
# else	/* ~BIGDIG */

/* DBL2STR_FUZZ is a somewhat arbitrary guard against
   round off error in scaling f and fprec. */
#  define DBL2STR_FUZZ 0.9
int dblprec;
/* static double dbl_eps; */
double dbl_prec(x)
     double x;
{
  int expt;
  double frac = frexp(x, &expt);
#  ifdef DBL_MIN_EXP
  if (0.0==x || expt < DBL_MIN_EXP) /* gradual underflow */
    return ldexp(1.0, - dbl_mant_dig) * ldexp(1.0, DBL_MIN_EXP);
#  endif
  if (1.0==frac) return ldexp(1.0, expt - dbl_mant_dig + 1);
  return ldexp(1.0, expt - dbl_mant_dig);
}

static int apx_log10(x)
     double x;
{
  int expt;
  frexp(x, &expt);
  expt -= 1;
  if (expt >= 0)
    return (int)(expt * llog2);
  return -((int)(-expt * llog2));
}

static double p10[] = {1.0, 1e1, 1e2, 1e3, 1e4, 1e5, 1e6, 1e7};
static double lpow10(x, n)
     double x;
     int n;
{
  if (n >= 0) {
    while (n > 7) {
      x *= 1e8;
      n -= 8;
    }
    return x*p10[n];
  }
  while (n < -7) {
    x /= 1e8;
    n += 8;
  }
  return x/p10[-n];
}

/* f is finite and positive */
static sizet pdbl2str(f, a, ch)
     double f;
     char *a;
     sizet ch;
{
  double fprec = dbl_prec(f);
  int efmt, dpt, d, i, exp = apx_log10(f);
  f = lpow10(f, -exp);
  fprec = lpow10(fprec, -exp);
#  ifdef DBL_MIN_10_EXP	/* Prevent random unnormalized values, as from
			   make-uniform-vector, from causing infinite loops,
			   but try to print gradually underflowing numbers. */
  while (f < 1.0) {
    f *= 10.0;
    fprec *= 10.0;
    if (exp-- < DBL_MIN_10_EXP - DBL_DIG - 1) return inf2str(f, a);
  }
  while (f > 10.0) {
    f /= 10.0;
    fprec /= 10.0;
    if (exp++ > DBL_MAX_10_EXP) return inf2str(f, a);
  }
#  else
  while (f < 1.0) {f *= 10.0; fprec *= 10.0; exp--;}
  while (f > 10.0) {f /= 10.0; fprec /= 10.0; exp++;}
#  endif
  fprec *= 0.5;
  if (f+fprec >= 10.0) {f = 1.0; exp++;}
 /* zero: */
#  ifdef ENGNOT
  dpt = (exp+9999)%3;
  exp -= dpt++;
  efmt = 1;
#  else
  efmt = (exp < -3) || (exp > dblprec+2);
  if (!efmt)
    if (exp < 0) {
      a[ch++] = '0';
      a[ch++] = '.';
      dpt = exp;
      while (++dpt)  a[ch++] = '0';
    } else
      dpt = exp+1;
  else
    dpt = 1;
#  endif

  for (i = 30; i--;) {
    /* printf("  f = %.20g, fprec = %.20g, i = %d\n", f, fprec, i); */
    d = f;
    f -= d;
    a[ch++] = d+'0';
    if (f < fprec && f < DBL2STR_FUZZ*fprec) break;
    if ((f + fprec) >= 1.0 && (f + DBL2STR_FUZZ*fprec) >= 1.0) {
      a[ch-1]++;
      break;
    }
    f *= 10.0;
    fprec *= 10.0;
    if (!(--dpt))  a[ch++] = '.';
  }

  if (dpt > 0)
#  ifndef ENGNOT
    if ((dpt > 4) && (exp > 6)) {
      d = (a[0]=='-'?2:1);
      for (i = ch++; i > d; i--)
	a[i] = a[i-1];
      a[d] = '.';
      efmt = 1;
    } else
#  endif
      {
	while (--dpt)  a[ch++] = '0';
	a[ch++] = '.';
      }
  if (a[ch-1]=='.')  a[ch++]='0'; /* trailing zero */
  if (efmt && exp) {
    a[ch++] = 'e';
    if (exp < 0) {
      exp = -exp;
      a[ch++] = '-';
    }
    for (i = 10; i <= exp; i *= 10);
    for (i /= 10; i; i /= 10) {
      a[ch++] = exp/i + '0';
      exp %= i;
    }
  }
  return ch;
}
# endif	/* ~BIGDIG */

static sizet iflo2str(flt, str)
     SCM flt;
     char *str;
{
  sizet i;
# ifdef SINGLES
  if (SINGP(flt)) i = idbl2str(FLO(flt), str);
  else
# endif
    i = idbl2str(REAL(flt), str);
  if (scm_narn==flt) return i;
  if (CPLXP(flt)) {
    if (!(0 > IMAG(flt))) str[i++] = '+';
    i += idbl2str(IMAG(flt), &str[i]);
    str[i++] = 'i';
  }
  return i;
}
#endif				/* FLOATS */

sizet iulong2str(num, rad, p)
     unsigned long num;
     int rad;
     char *p;
{
  sizet j;
  register int i = 1, d;
  register unsigned long n = num;
  for (n /= rad;n > 0;n /= rad) i++;
  j = i;
  n = num;
  while (i--) {
    d = n % rad;
    n /= rad;
    p[i] = d + ((d < 10) ? '0' : 'a' - 10);
  }
  return j;
}
sizet ilong2str(num, rad, p)
     long num;
     int rad;
     char *p;
{
  if ((num < 0) && !(rad < 0)) {
    *p++ = '-';
    return 1 + iulong2str((unsigned long) -num, rad, p);
  }
  return iulong2str((unsigned long) num, rad < 0 ? -rad : rad, p);
}
#ifdef BIGDIG
static SCM big2str(b, radix)
     SCM b;
     register unsigned int radix;
{
  SCM t = copybig(b, 0);	/* sign of temp doesn't matter */
  register BIGDIG *ds = BDIGITS(t);
  sizet i = NUMDIGS(t);
  sizet j = radix==16 ? (BITSPERDIG*i)/4+2
    : radix >= 10 ? (BITSPERDIG*i*241L)/800+2
    : (BITSPERDIG*i)+2;
  sizet k = 0;
  sizet radct = 0;
  sizet ch; /* jeh */
  BIGDIG radpow = 1, radmod = 0;
  SCM ss = makstr((long)j);
  char *s = CHARS(ss), c;
  scm_protect_temp(&t);
  while ((long) radpow * radix < BIGRAD) {
    radpow *= radix;
    radct++;
  }
  s[0] = tc16_bigneg==TYP16(b) ? '-' : '+';
  while ((i || radmod) && j) {
    if (k==0) {
      radmod = (BIGDIG)divbigdig(ds, i, radpow);
      k = radct;
      if (!ds[i-1]) i--;
    }
    c = radmod % radix; radmod /= radix; k--;
    s[--j] = c < 10 ? c + '0' : c + 'a' - 10;
  }
  ch = s[0]=='-' ? 1 : 0; /* jeh */
  if (ch < j) { /* jeh */
    for (i = j;j < LENGTH(ss);j++) s[ch+j-i] = s[j]; /* jeh */
    resizuve(ss, (SCM)MAKINUM(ch+LENGTH(ss)-i)); /* jeh */
  }
  bigrecy(t);
  return ss;
}
#endif
SCM number2string(x, radix)
     SCM x, radix;
{
  if (UNBNDP(radix)) radix=MAKINUM(10L);
  else ASRTER(INUMP(radix), radix, ARG2, s_number2string);
#ifdef FLOATS
  if (NINUMP(x)) {
    char num_buf[FLOBUFLEN];
# ifdef BIGDIG
    ASRTGO(NIMP(x), badx);
    if (BIGP(x)) return big2str(x, (unsigned int)INUM(radix));
#  ifndef RECKLESS
    if (!(INEXP(x)))
      badx: wta(x, (char *)ARG1, s_number2string);
#  endif
# else
    ASRTER(NIMP(x) && INEXP(x), x, ARG1, s_number2string);
# endif
    return makfromstr(num_buf, iflo2str(x, num_buf));
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_number2string);
    return big2str(x, (unsigned int)INUM(radix));
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_number2string);
# endif
#endif
  {
    char num_buf[INTBUFLEN];
    return makfromstr(num_buf, ilong2str(INUM(x), (int)INUM(radix), num_buf));
  }
}
/* These print routines are stubbed here so that repl.c doesn't need
   FLOATS or BIGDIGs conditionals */
int floprint(sexp, port, writing)
     SCM sexp;
     SCM port;
     int writing;
{
#ifdef FLOATS
  if (!errjmp_bad) {
    char num_buf[FLOBUFLEN];
    lfwrite(num_buf, (sizet)sizeof(char), iflo2str(sexp, num_buf), port);
    return !0;
  } else
#endif
  scm_ipruk("float", sexp, port);
  return !0;
}
int bigprint(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
#ifdef BIGDIG
  if (!errjmp_bad) {
    exp = big2str(exp, (unsigned int)10);
    lfwrite(CHARS(exp), (sizet)sizeof(char), (sizet)LENGTH(exp), port);
    return !0;
  } else
#endif
  scm_ipruk("bignum", exp, port);
  return !0;
}
/*** END nums->strs ***/

/*** STRINGS -> NUMBERS ***/
#ifdef BIGDIG
SCM istr2int(str, len, radix)
     char *str;
     long len;
     register int radix;
{
  register sizet k, blen = 1;
  sizet i = 0, j;
  int c;
  SCM res;
  register BIGDIG *ds;
  register UBIGLONG t2;

  if (0 >= len) return BOOL_F;	/* zero length */
  /* Estimate number of digits; will trim during finish */
  if (10==radix) j = 1+(84*len)/(BITSPERDIG*25);
  else j = (8 < radix) ? 1+(4*len)/BITSPERDIG : 1+(3*len)/BITSPERDIG;
  switch (str[0]) {		/* leading sign */
  case '-':
  case '+': if (++i==len) return BOOL_F; /* bad if lone `+' or `-' */
  }
  res = mkbig(j, '-'==str[0]);
  ds = BDIGITS(res);
  /* clear allocated digits */
  for (k = j;k--;) ds[k] = 0;
  do {
    switch (c = str[i++]) {
    case DIGITS:
      c = c - '0';
      goto accumulate;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      c = c-'A'+10;
      goto accumulate;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      c = c-'a'+10;
    accumulate:
      if (c >= radix) return BOOL_F; /* bad digit for radix */
      k = 0;
      t2 = c;
    moretodo:
      /* Add t2 into temp bignum */
      while (k < blen) {
	t2 += ((UBIGLONG)ds[k])*radix;
	ds[k++] = BIGLO(t2);
	t2 = BIGDN(t2);
      }
      ASRTER(blen <= j, (SCM)MAKINUM(blen), OVFLOW, "bignum");
      if (t2) {blen++; goto moretodo;}
      break;
    default:
      return BOOL_F;		/* not a digit */
    }
  } while (i < len);
  if (blen * BITSPERDIG/CHAR_BIT <= sizeof(SCM))
    if (INUMP(res = big2inum(res, blen))) return res;
  if (j==blen) return res;
  return adjbig(res, blen);
}
#else
SCM istr2int(str, len, radix)
     register char *str;
     long len;
     register int radix;
{
  register long n = 0, ln;
  register int c;
  register int i = 0;
  int lead_neg = 0;
  if (0 >= len) return BOOL_F;	/* zero length */
  switch (*str) {		/* leading sign */
  case '-': lead_neg = 1;
  case '+': if (++i==len) return BOOL_F; /* bad if lone `+' or `-' */
  }

  do {
    switch (c = str[i++]) {
    case DIGITS:
      c = c - '0';
      goto accumulate;
    case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
      c = c-'A'+10;
      goto accumulate;
    case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
      c = c-'a'+10;
    accumulate:
      if (c >= radix) return BOOL_F; /* bad digit for radix */
      ln = n;
      n = n * radix - c;
      if (n > ln
# ifdef hpux
	  || (-n > -MOST_NEGATIVE_FIXNUM) /* workaround for HP700 cc bug */
# endif
	  ) goto ovfl;
      break;
    default:
      return BOOL_F;		/* not a digit */
    }
  } while (i < len);
  if (lead_neg) {
    if (n < MOST_NEGATIVE_FIXNUM) goto ovfl;
  }
  else {
    if (n < -MOST_POSITIVE_FIXNUM) goto ovfl;
    n = -n;
  }
  return MAKINUM(n);
 ovfl:				/* overflow scheme integer */
  return BOOL_F;
}
#endif

#ifdef FLOATS
# ifdef BIGDIG

/* [Jaffer] */
/* In Clinger's "How to Read Floating-Point Numbers Accurately" */
/* the key issue is that successive rounding does not have the */
/* same effect as one rounding operation.  With bignums it is */
/* a simple matter to accumulate digits without rounding. */
/* The approach here is to compute the binary value without rounding, */
/* then round explicitly when scaling the bigint to fit into the */
/* mantissa. */

/* manstr is the mantissa with the decimal point removed and point
   (the exponent) adjusted appropriately */
double mantexp2dbl(manstr, point)
     SCM manstr;
     int point;
{
  SCM bmant = istr2int(CHARS(manstr), LENGTH(manstr), 10L);
  double val = pmantexp2dbl(bmant, point);
  bigrecy(bmant);
  return val;
}
double pmantexp2dbl(bmant, point)
     SCM bmant;
     int point;
{
  double ans;
  if (BOOL_F == bmant) return REAL(scm_narn);
  if (point >= 0) {
    if (point < 23 && INUM(scm_intlength(bmant)) <= dbl_mant_dig)
      return ldexp(num2dbl(bmant,ARG1,s_str2number) * dpows5[point], point);
    {
      SCM quo, num = product(bmant, VELTS(pows5)[(long) point]);
      int bex = INUM(scm_intlength(num)) - dbl_mant_dig;
      if (bex > 0) {
	SCM den = scm_ash(MAKINUM(1L), MAKINUM(bex));
	quo = scm_round_quotient(num, den);
	bigrecy(den);
      }
      else
	quo = scm_ash(num, MAKINUM(- bex));
      /* quo may not be a bignum */
      if (INUMP(quo)) ans = ldexp((double)(INUM(quo)), bex + point);
      else {
	sizet i = NUMDIGS(quo);
	sizet j = (dbl_mant_dig + BITSPERDIG - 1)/BITSPERDIG;
	BIGDIG *digits = BDIGITS(quo);
	if (j < i) j = i - j;
	else j = 0;
	ans = 0.0;
	while (i-- > j) ans = digits[i] + ldexp(ans, BITSPERDIG);
	bex += j * BITSPERDIG;
	ans = ldexp(ans, bex + point);
      }
      if (num != quo) bigrecy(quo);
      if ((num != bmant) && (bmant != MAKINUM(1L))) bigrecy(num);
      return ans;
    }
  }
  if (-point < 23 && INUM(scm_intlength(bmant)) <= dbl_mant_dig)
    return ldexp(num2dbl(bmant,ARG1,s_str2number) / dpows5[-point], point);
  {
    int maxpow = LENGTH(pows5) - 1;
    SCM num, quo, scl = (-point <= maxpow) ?
      VELTS(pows5)[(long) -point] :
      product(VELTS(pows5)[(long)maxpow], VELTS(pows5)[(long)-point-maxpow]);
    int mantlen = dbl_mant_dig;
    int bex =			/* bex < 0 */
      INUM(scm_intlength(bmant)) - INUM(scm_intlength(scl)) - mantlen;
    int tmp = bex + point + 1021 + mantlen;
    if (tmp < 0) {
      bex -= tmp + 1;
      mantlen += tmp;
    }
    num = scm_ash(bmant, MAKINUM(-bex));
    quo = scm_round_quotient(num, scl);
    if (INUM(scm_intlength(quo)) > mantlen) {
      bex++;			/* too many bits of quotient */
      quo = scm_round_quotient(num, scm_ash(scl, MAKINUM(1L)));
    }
    if (-point > maxpow) bigrecy(scl);
    if (num != quo) bigrecy(num);
    ans = ldexp(int2dbl(quo), bex + point);
    bigrecy(quo);
    return ans;
  }
}

# else /* def BIGDIG */

double mantexp2dbl(manstr, point)
     SCM manstr;
     int point;
{
  register int c, i = 0;
  double res = 0.0;
  char *str = CHARS(manstr);
  int len = LENGTH(manstr);
  do {				/* check initial digits */
    switch (c = str[i]) {
    case DIGITS:
      c = c - '0';
      res = res * 10 + c;
      break;
    case 'D': case 'E': case 'F':
    case 'd': case 'e': case 'f':
    default:
      goto out1;
    }
  } while (++i < len);
 out1:
  if (point >= 0)
    while (point--)  res *= 10.0;
  else
    while (point++) {
#  ifdef _UNICOS
      res *= 0.1;
#  else
      res /= 10.0;
#  endif
    }
  return res;
}

# endif /* def BIGDIG */

SCM istr2flo(str, len, radix)
     register char *str;
     register long len;
     register long radix;
{
  register int c, i = 0, j = 0;
  int lead_sgn = 0;
  double res = 0.0, tmp = 0.0;
  int flg = 0;
  int point = 0;
  int shrtp = 0;
  SCM second, manstr;
  char *mant;

  if (i >= len) return BOOL_F;	/* zero length */

  switch (*str) {		/* leading sign */
  case '-': lead_sgn = -1; i++; break;
  case '+': lead_sgn = 1; i++; break;
  }
  if (i==len) return BOOL_F;	/* bad if lone `+' or `-' */

  if (6==len && ('+'==str[0] || '-'==str[0])) {
    if (0==strcasecmp(str_inf0, &str[1]))
      return makdbl(1./0. * ('+'==str[0] ? 1 : -1), 0.0);
    else if (0==strcasecmp(str_nan0, &str[1]))
      return scm_narn;
  }
  if (str[i]=='i' || str[i]=='I') { /* handle `+i' and `-i'   */
    if (lead_sgn==0) return BOOL_F; /* must have leading sign */
    if (++i < len) return BOOL_F; /* `i' not last character */
    return makdbl(0.0, (double)lead_sgn);
  }

  manstr = makstr(len);
  mant = CHARS(manstr);

  do {				/* check initial digits */
    switch (c = str[i]) {
    case DIGITS:
      c = c - '0';
      goto accum1;
    case 'D': case 'E': case 'F':
      if (radix==10) goto out1; /* must be exponent */
    case 'A': case 'B': case 'C':
      c = c-'A'+10;
      goto accum1;
    case 'd': case 'e': case 'f':
      if (radix==10) goto out1;
    case 'a': case 'b': case 'c':
      c = c-'a'+10;
    accum1:
      if (c >= radix) return BOOL_F; /* bad digit for radix */
      mant[j++] = str[i];
      res = res * radix + c;
      flg = 1;			/* res is valid */
      break;
    default:
      goto out1;
    }
  } while (++i < len);
 out1:

  /* if true, then we did see a digit above, and res is valid */
  if (i==len) goto done;

  /* By here, must have seen a digit,
     or must have next char be a `.' with radix==10 */
  if ((!flg) && (!(str[i]=='.' && radix==10))) return BOOL_F;

  while (str[i]=='#') {		/* optional sharps */
    res *= radix;
    mant[j++] = '0';
    if (++i==len) goto done;
  }

  if (str[i]=='/' && i+1 < len) {
    while (++i < len) {
      switch (c = str[i]) {
      case DIGITS:
	c = c - '0';
	goto accum2;
      case 'A': case 'B': case 'C': case 'D': case 'E': case 'F':
	c = c-'A'+10;
	goto accum2;
      case 'a': case 'b': case 'c': case 'd': case 'e': case 'f':
	c = c-'a'+10;
      accum2:
	if (c >= radix) return BOOL_F;
	tmp = tmp * radix + c;
	break;
      default:
	goto out2;
      }
    }
  out2:
    /*     if (tmp==0.0) return BOOL_F; /\* `slash zero' not allowed *\/ */
    if (i < len)
      while (str[i]=='#') {	/* optional sharps */
	tmp *= radix;
	mant[j++] = '0';
	if (++i==len) break;
      }
    res /= tmp;
    goto done;
  }

  if (str[i]=='.') {		/* decimal point notation */
    if (radix != 10) return BOOL_F; /* must be radix 10 */
    while (++i < len) {
      switch (c = str[i]) {
      case DIGITS:
	point--;
	res = res*10.0 + c-'0';
	flg = 1;
	mant[j++] = str[i];
	break;
      default:
	goto out3;
      }
    }
  out3:
    if (!flg) return BOOL_F;	/* no digits before or after decimal point */
    if (i==len) goto adjust;
    while (str[i]=='#') {	/* ignore remaining sharps */
      if (++i==len) goto adjust;
    }
  }

  switch (str[i]) {		/* exponent */
  case 'f': case 'F':
  case 's': case 'S':
    shrtp = !0;
  case 'd': case 'D':
  case 'e': case 'E':
  case 'l': case 'L': {
      int expsgn = 1, expon = 0;
      if (radix != 10) return BOOL_F; /* only in radix 10 */
      if (++i==len) return BOOL_F; /* bad exponent */
      switch (str[i]) {
      case '-':  expsgn=(-1);
      case '+':  if (++i==len) return BOOL_F; /* bad exponent */
      }
      if (str[i] < '0' || str[i] > '9') return BOOL_F; /* bad exponent */
      do {
	switch (c = str[i]) {
	case DIGITS:
	  expon = expon*10 + c-'0';
	  /*	if (expon > MAXEXP) */
	  /*	  if (1==expsgn || expon > (MAXEXP + dblprec + 1)) */
	  /*	    return BOOL_F; /\* exponent too large *\/ */
	  break;
	default:
	  goto out4;
	}
      } while (++i < len);
    out4:
      point += expsgn*expon;
    }
  }

 adjust:
  mant[j] = 0;
  manstr = resizuve(manstr, MAKINUM(j));
  if (radix == 10) res = mantexp2dbl(manstr, point);

 done:
  /* at this point, we have a legitimate floating point result */
  if (lead_sgn==-1)  res = -res;
  if (i==len) return shrtp ? makflo(res) : makdbl(res, 0.0);

  if (str[i]=='i' || str[i]=='I') { /* pure imaginary number  */
    if (lead_sgn==0) return BOOL_F; /* must have leading sign */
    if (++i < len) return BOOL_F; /* `i' not last character */
    return makdbl(0.0, res);
  }

  switch (str[i++]) {
  case '-':  lead_sgn = -1; break;
  case '+':  lead_sgn = 1;  break;
  case '@': {			/* polar input for complex number */
    /* get a `real' for angle */
    second = istr2flo(&str[i], (long)(len-i), radix);
    if (IMP(second)) return BOOL_F;
    if (!(INEXP(second))) return BOOL_F; /* not `real' */
    if (CPLXP(second))    return BOOL_F; /* not `real' */
    tmp = REALPART(second);
    return makdbl(res*cos(tmp), res*sin(tmp));
  }
  default: return BOOL_F;
  }

  /* at this point, last char must be `i' */
  if (str[len-1] != 'i' && str[len-1] != 'I') return BOOL_F;
  /* handles `x+i' and `x-i' */
  if (i==(len-1))  return makdbl(res, (double)lead_sgn);
  /* get a `ureal' for complex part */
  second = istr2flo(&str[i], (long)((len-i)-1), radix);
  if (IMP(second)) return BOOL_F;
  if (!(INEXP(second))) return BOOL_F; /* not `ureal' */
  if (CPLXP(second))    return BOOL_F; /* not `ureal' */
  tmp = REALPART(second);
  if (tmp < 0.0)	return BOOL_F; /* not `ureal' */
  return makdbl(res, (lead_sgn*tmp));
}
#endif				/* FLOATS */


SCM istring2number(str, len, radix)
     char *str;
     long len;
     long radix;
{
  int i = 0;
  char ex = 0;
  char ex_p = 0, rx_p = 0;	/* Only allow 1 exactness and 1 radix prefix */
  SCM res;
  if (len==1)
    if (*str=='+' || *str=='-') /* Catches lone `+' and `-' for speed */
      return BOOL_F;

  while ((len-i) >= 2  &&  str[i]=='#' && ++i)
    switch (str[i++]) {
    case 'b': case 'B':  if (rx_p++) return BOOL_F; radix = 2;  break;
    case 'o': case 'O':  if (rx_p++) return BOOL_F; radix = 8;  break;
    case 'd': case 'D':  if (rx_p++) return BOOL_F; radix = 10; break;
    case 'x': case 'X':  if (rx_p++) return BOOL_F; radix = 16; break;
    case 'i': case 'I':  if (ex_p++) return BOOL_F; ex = 2;     break;
    case 'e': case 'E':  if (ex_p++) return BOOL_F; ex = 1;     break;
    default:  return BOOL_F;
    }

  switch (ex) {
  case 1:
    return istr2int(&str[i], len-i, radix);
  case 0:
    res = istr2int(&str[i], len-i, radix);
    if (NFALSEP(res)) return res;
#ifdef FLOATS
  case 2: return istr2flo(&str[i], len-i, radix);
#endif
  }
  return BOOL_F;
}


SCM string2number(str, radix)
     SCM str, radix;
{
  if (UNBNDP(radix)) radix=MAKINUM(10L);
  else ASRTER(INUMP(radix), radix, ARG2, s_str2number);
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_str2number);
  return istring2number(CHARS(str), LENGTH(str), INUM(radix));
}
/*** END strs->nums ***/

#ifdef FLOATS
SCM makdbl (x, y)
     double x, y;
{
  SCM z;
  if ((y==0.0) && (x==0.0)) return flo0;
# ifndef _MSC_VER
#  ifndef SINGLESONLY
  if ((y != y) || (x != x) || (y==(2 * y) && (y != 0.0))) return scm_narn;
  if ((x==(2 * x)) && (x != 0.0)) y = 0.0;
#  endif
# endif
  DEFER_INTS;
  if (y==0.0) {
# ifdef SINGLES
    float fx = x;        /* David Yeh <theyeh@uclink.berkeley.edu>
			    changed this so that MSVC works */
#  ifndef SINGLESONLY
    if ((-FLTMAX < x) && (x < FLTMAX) && ( (double)fx == x) )
#  endif
      {
	NEWCELL(z);
	CAR(z) = tc_flo;
	FLO(z) = x;
	ALLOW_INTS;
	return z;
      }
# endif				/* def SINGLES */
    z = must_malloc_cell(1L*sizeof(double), (SCM)tc_dblr, "real");
  }
  else {
    z = must_malloc_cell(2L*sizeof(double), (SCM)tc_dblc, "complex");
    IMAG(z) = y;
  }
  REAL(z) = x;
  ALLOW_INTS;
  return z;
}
#endif				/* FLOATS */

#ifndef INUMS_ONLY
SCM eqv(x, y)
     SCM x, y;
{
  if (x==y) return BOOL_T;
  if (IMP(x)) return BOOL_F;
  if (IMP(y)) return BOOL_F;
  /* this ensures that types and length are the same. */
  if (CAR(x) != CAR(y)) return BOOL_F;
  if (NUMP(x)) {
# ifdef BIGDIG
    if (BIGP(x)) return (0==bigcomp(x, y)) ? BOOL_T : BOOL_F;
# endif
# ifdef FLOATS
    return floequal(x, y);
# endif
  }
  return BOOL_F;
}
SCM memv(x, lst)			/* m.borza  12.2.91 */
SCM x, lst;
{
  for (;NIMP(lst);lst = CDR(lst)) {
    ASRTGO(CONSP(lst), badlst);
    if (NFALSEP(eqv(CAR(lst), x))) return lst;
  }
# ifndef RECKLESS
  if (!(NULLP(lst)))
    badlst: wta(lst, (char *)ARG2, s_memv);
# endif
  return BOOL_F;
}
SCM assv(x, alist)		/* m.borza  12.2.91 */
SCM x, alist;
{
  SCM tmp;
  for (;NIMP(alist);alist = CDR(alist)) {
    ASRTGO(CONSP(alist), badlst);
    tmp = CAR(alist);
    ASRTGO(NIMP(tmp) && CONSP(tmp), badlst);
    if (NFALSEP(eqv(CAR(tmp), x))) return tmp;
  }
# ifndef RECKLESS
  if (!(NULLP(alist)))
    badlst: wta(alist, (char *)ARG2, s_assv);
# endif
  return BOOL_F;
}
#endif

SCM list_tail(lst, k)
     SCM lst, k;
{
  register long i;
  ASRTER(INUMP(k), k, ARG2, s_list_tail);
  i = INUM(k);
  while (i-- > 0) {
    ASRTER(NIMP(lst) && CONSP(lst), lst, ARG1, s_list_tail);
    lst = CDR(lst);
  }
  return lst;
}

SCM string2list(str)
     SCM str;
{
  long i;
  SCM res = EOL;
  unsigned char *src;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_str2list);
  src = UCHARS(str);
  for (i = LENGTH(str)-1;i >= 0;i--) res = cons((SCM)MAKICHR(src[i]), res);
  return res;
}
SCM string_copy(str)
     SCM str;
{
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_st_copy);
  return makfromstr(CHARS(str), (sizet)LENGTH(str));
}
SCM string_fill(str, chr)
     SCM str, chr;
{
  register char *dst, c;
  register long k;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_st_fill);
  ASRTER(ICHRP(chr), chr, ARG2, s_st_fill);
  c = ICHR(chr);
  dst = CHARS(str);
  for (k = LENGTH(str)-1;k >= 0;k--) dst[k] = c;
  return UNSPECIFIED;
}
SCM vector2list(v)
     SCM v;
{
  SCM res = EOL;
  long i;
  SCM *data;
  ASRTER(NIMP(v) && VECTORP(v), v, ARG1, s_vect2list);
  data = VELTS(v);
  for (i = LENGTH(v)-1;i >= 0;i--) res = cons(data[i], res);
  return res;
}
SCM vector_fill(v, fill)
     SCM v, fill;
{
  register long i;
  register SCM *data;
  ASRTER(NIMP(v) && VECTORP(v), v, ARG1, s_ve_fill);
  data = VELTS(v);
  for (i = LENGTH(v)-1;i >= 0;i--) data[i] = fill;
  return UNSPECIFIED;
}
static SCM vector_equal(x, y)
     SCM x, y;
{
  long i;
  for (i = LENGTH(x)-1;i >= 0;i--)
    if (FALSEP(equal(VELTS(x)[i], VELTS(y)[i]))) return BOOL_F;
  return BOOL_T;
}
#ifdef BIGDIG
SCM bigequal(x, y)
     SCM x, y;
{
  if (0==bigcomp(x, y)) return BOOL_T;
  return BOOL_F;
}
#endif
#ifdef FLOATS
SCM floequal(x, y)
     SCM x, y;
{
  if ((REALPART(x) != REALPART(y))) return BOOL_F;
  if (CPLXP(x))
    return (CPLXP(y) && (IMAG(x)==IMAG(y))) ? BOOL_T : BOOL_F;
  return CPLXP(y) ? BOOL_F : BOOL_T;
}
#endif
SCM equal(x, y)
     SCM x, y;
{
  CHECK_STACK;
 tailrecurse: POLL;
  if (x==y) return BOOL_T;
  if (IMP(x)) return BOOL_F;
  if (IMP(y)) return BOOL_F;
  if (CONSP(x) && CONSP(y)) {
    if (FALSEP(equal(CAR(x), CAR(y)))) return BOOL_F;
    x = CDR(x);
    y = CDR(y);
    goto tailrecurse;
  }
  /* this ensures that types and length are the same. */
  if (CAR(x) != CAR(y)) return BOOL_F;
  switch (TYP7(x)) {
  default: return BOOL_F;
  case tc7_string: return st_equal(x, y);
  case tc7_vector: return vector_equal(x, y);
  case tc7_smob: {
    int i = SMOBNUM(x);
    if (!(i < numsmob)) return BOOL_F;
    if (smobs[i].equalp) return (smobs[i].equalp)(x, y);
    else return BOOL_F;
  }
  case tc7_VfixN8: case tc7_VfixZ8: case tc7_VfixN16: case tc7_VfixZ16:
  case tc7_VfixN32: case tc7_VfixZ32: case tc7_VfixN64: case tc7_VfixZ64:
  case tc7_VfloR32: case tc7_VfloC32: case tc7_VfloC64: case tc7_VfloR64:
  case tc7_Vbool: {
    SCM (*pred)() = smobs[0x0ff & (tc16_array>>8)].equalp;
    if (pred) return (*pred)(x, y);
    else return BOOL_F;
  }
  }
}

SCM numberp(obj)
     SCM obj;
{
  if (INUMP(obj)) return BOOL_T;
#ifdef FLOATS
  if (NIMP(obj) && NUMP(obj)) return BOOL_T;
#else
# ifdef BIGDIG
  if (NIMP(obj) && NUMP(obj)) return BOOL_T;
# endif
#endif
  return BOOL_F;
}
#ifdef FLOATS
SCM scm_complex_p(obj)
     SCM obj;
{
  if (obj==scm_narn) return BOOL_F;
  return numberp(obj);
}

# ifdef BIGDIG
static char twostab[] = {4, 0, 1, 0, 2, 0, 1, 0, 3, 0, 1, 0, 2, 0, 1, 0};
static long scm_twos_power(n)
     SCM n;
{
  long d, c = 0;
  int d4;
#  ifdef BIGDIG
  if (NINUMP(n)) {
    BIGDIG *ds;
    int i = 0;
    ds = BDIGITS(n);
    while (0==(d = ds[i++])) c += BITSPERDIG;
    goto out;
  }
#  endif
  d = INUM(n);
  if (0==d) return 0;
 out:
  do {
    d4 = 15 & d;
    c += twostab[d4];
    d >>= 4;
  } while (0==d4);
  return c;
}

int scm_bigdblcomp(b, d)
     SCM b;
     double d;
{
  int dlen = 99;
  sizet blen = 0;
  int dneg = d < 0 ? 1 : 0;
  int bneg = BIGSIGN(b) ? 1 : 0;
  if (bneg < dneg) return -1;
  if (bneg > dneg) return 1;
  if (!(d==2*d && d != 0.0)) {
    frexp(d, &dlen);
    blen = INUM(scm_intlength(b));
  }
  if (blen > dlen) return dneg ? 1 : -1;
  if (blen < dlen) return dneg ? -1 : 1;
  if ((blen <= dbl_mant_dig) || (blen - scm_twos_power(b)) <= dbl_mant_dig) {
    double bd = int2dbl(b);
    if (bd > d) return -1;
    if (bd < d) return 1;
    return 0;
  }
  return bigcomp(b, dbl2big(d));
}
# endif
SCM realp(x)
     SCM x;
{
  if (INUMP(x)) return BOOL_T;
  if (IMP(x)) return BOOL_F;
  if (REALP(x)) return BOOL_T;
# ifdef BIGDIG
  if (BIGP(x)) return BOOL_T;
# endif
  return BOOL_F;
}
SCM scm_rationalp(x)
     SCM x;
{
  if (INUMP(x)) return BOOL_T;
  if (IMP(x)) return BOOL_F;
  if (REALP(x)) {
    float y = REALPART(x);
    if (y==2*y && y != 0.0) return BOOL_F;
    return BOOL_T;
  }
# ifdef BIGDIG
  if (BIGP(x)) return BOOL_T;
# endif
  return BOOL_F;
}
SCM intp(x)
     SCM x;
{
  double r;
  if (INUMP(x)) return BOOL_T;
  if (IMP(x)) return BOOL_F;
# ifdef BIGDIG
  if (BIGP(x)) return BOOL_T;
# endif
  if (!INEXP(x)) return BOOL_F;
  if (CPLXP(x)) return BOOL_F;
  r = REALPART(x);
  if (r != floor(r)) return BOOL_F;
  if (r==2*r && r != 0.0) return BOOL_F;
  return BOOL_T;
}
#endif				/* FLOATS */

SCM inexactp(x)
     SCM x;
{
#ifdef FLOATS
  if (NIMP(x) && INEXP(x)) return BOOL_T;
#endif
  return BOOL_F;
}

SCM eqp(x, y)
     SCM x, y;
{
#ifdef FLOATS
  SCM t;
  if (NINUMP(x)) {
# ifdef BIGDIG
#  ifndef RECKLESS
    if (!(NIMP(x)))
      badx: wta(x, (char *)ARG1, s_eqp);
#  endif
    if (BIGP(x)) {
      if (INUMP(y)) return BOOL_F;
      ASRTGO(NIMP(y), bady);
      if (BIGP(y)) return (0==bigcomp(x, y)) ? BOOL_T : BOOL_F;
      ASRTGO(INEXP(y), bady);
    bigreal:
      return (REALP(y) && (0==scm_bigdblcomp(x, REALPART(y)))) ?
	BOOL_T : BOOL_F;
    }
    ASRTGO(INEXP(x), badx);
# else
    ASRTER(NIMP(x) && INEXP(x), x, ARG1, s_eqp);
# endif
    if (INUMP(y)) {t = x; x = y; y = t; goto realint;}
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) {t = x; x = y; y = t; goto bigreal;}
    ASRTGO(INEXP(y), bady);
# else
    ASRTGO(NIMP(y) && INEXP(y), bady);
# endif
    if (x==y) return BOOL_T;
    return floequal(x, y);
  }
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) return BOOL_F;
#  ifndef RECKLESS
    if (!(INEXP(y)))
      bady: wta(y, (char *)ARG2, s_eqp);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && INEXP(y)))
      bady: wta(y, (char *)ARG2, s_eqp);
#  endif
# endif
  realint:
    return (REALP(y) && (((double)INUM(x))==REALPART(y))) ? BOOL_T : BOOL_F;
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_eqp);
    if (INUMP(y)) return BOOL_F;
    ASRTGO(NIMP(y) && BIGP(y), bady);
    return (0==bigcomp(x, y)) ? BOOL_T : BOOL_F;
  }
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
      bady: wta(y, (char *)ARG2, s_eqp);
#  endif
    return BOOL_F;
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_eqp);
  ASRTER(INUMP(y), y, ARG2, s_eqp);
# endif
#endif
  return ((long)x==(long)y) ? BOOL_T : BOOL_F;
}
SCM lessp(x, y)
     SCM x, y;
{
#ifdef FLOATS
  if (NINUMP(x)) {
# ifdef BIGDIG
#  ifndef RECKLESS
    if (!(NIMP(x)))
      badx: wta(x, (char *)ARG1, s_lessp);
#  endif
    if (BIGP(x)) {
      if (INUMP(y)) return BIGSIGN(x) ? BOOL_T : BOOL_F;
      ASRTGO(NIMP(y), bady);
      if (BIGP(y)) return (1==bigcomp(x, y)) ? BOOL_T : BOOL_F;
      ASRTGO(REALP(y), bady);
      return (1==scm_bigdblcomp(x, REALPART(y))) ? BOOL_T : BOOL_F;
    }
    ASRTGO(REALP(x), badx);
# else
    ASRTER(NIMP(x) && REALP(x), x, ARG1, s_lessp);
# endif
    if (INUMP(y)) return (REALPART(x) < ((double)INUM(y))) ? BOOL_T : BOOL_F;
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) return (-1==scm_bigdblcomp(y, REALPART(x))) ? BOOL_T : BOOL_F;
    ASRTGO(REALP(y), bady);
# else
    ASRTGO(NIMP(y) && REALP(y), bady);
# endif
    return (REALPART(x) < REALPART(y)) ? BOOL_T : BOOL_F;
  }
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) return BIGSIGN(y) ? BOOL_F : BOOL_T;
#  ifndef RECKLESS
    if (!(REALP(y)))
      bady: wta(y, (char *)ARG2, s_lessp);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && REALP(y)))
      bady: wta(y, (char *)ARG2, s_lessp);
#  endif
# endif
    return (((double)INUM(x)) < REALPART(y)) ? BOOL_T : BOOL_F;
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_lessp);
    if (INUMP(y)) return BIGSIGN(x) ? BOOL_T : BOOL_F;
    ASRTGO(NIMP(y) && BIGP(y), bady);
    return (1==bigcomp(x, y)) ? BOOL_T : BOOL_F;
  }
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
      bady: wta(y, (char *)ARG2, s_lessp);
#  endif
    return BIGSIGN(y) ? BOOL_F : BOOL_T;
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_lessp);
  ASRTER(INUMP(y), y, ARG2, s_lessp);
# endif
#endif
  return ((long)x < (long)y) ? BOOL_T : BOOL_F;
}
SCM greaterp(x, y)
     SCM x, y;
{
  return lessp(y, x);
}
SCM leqp(x, y)
     SCM x, y;
{
  return BOOL_NOT(lessp(y, x));
}
SCM greqp(x, y)
     SCM x, y;
{
  return BOOL_NOT(lessp(x, y));
}
SCM zerop(z)
     SCM z;
{
#ifdef FLOATS
  if (NINUMP(z)) {
# ifdef BIGDIG
    ASRTGO(NIMP(z), badz);
    if (BIGP(z)) return BOOL_F;
#  ifndef RECKLESS
    if (!(INEXP(z)))
      badz: wta(z, (char *)ARG1, s_zerop);
#  endif
# else
    ASRTER(NIMP(z) && INEXP(z), z, ARG1, s_zerop);
# endif
    return (z==flo0) ? BOOL_T : BOOL_F;
  }
#else
# ifdef BIGDIG
  if (NINUMP(z)) {
    ASRTER(NIMP(z) && BIGP(z), z, ARG1, s_zerop);
    return BOOL_F;
  }
# else
  ASRTER(INUMP(z), z, ARG1, s_zerop);
# endif
#endif
  return (z==INUM0) ? BOOL_T: BOOL_F;
}
SCM positivep(x)
     SCM x;
{
#ifdef FLOATS
  if (NINUMP(x)) {
# ifdef BIGDIG
    ASRTGO(NIMP(x), badx);
    if (BIGP(x)) return TYP16(x)==tc16_bigpos ? BOOL_T : BOOL_F;
#  ifndef RECKLESS
    if (!(REALP(x)))
      badx: wta(x, (char *)ARG1, s_positivep);
#  endif
# else
    ASRTER(NIMP(x) && REALP(x), x, ARG1, s_positivep);
# endif
    return (REALPART(x) > 0.0) ? BOOL_T : BOOL_F;
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_positivep);
    return TYP16(x)==tc16_bigpos ? BOOL_T : BOOL_F;
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_positivep);
# endif
#endif
  return (x > INUM0) ? BOOL_T : BOOL_F;
}
SCM negativep(x)
     SCM x;
{
#ifdef FLOATS
  if (NINUMP(x)) {
# ifdef BIGDIG
    ASRTGO(NIMP(x), badx);
    if (BIGP(x)) return TYP16(x)==tc16_bigpos ? BOOL_F : BOOL_T;
#  ifndef RECKLESS
    if (!(REALP(x)))
      badx: wta(x, (char *)ARG1, s_negativep);
#  endif
# else
    ASRTER(NIMP(x) && REALP(x), x, ARG1, s_negativep);
# endif
    return (REALPART(x) < 0.0) ? BOOL_T : BOOL_F;
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_negativep);
    return (TYP16(x)==tc16_bigneg) ? BOOL_T : BOOL_F;
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_negativep);
# endif
#endif
  return (x < INUM0) ? BOOL_T : BOOL_F;
}

static char s_exactprob[] = "not representable as inexact";
SCM scm_max(x, y)
     SCM x, y;
{
#ifdef FLOATS
  SCM t;
  double z;
#endif
  if (UNBNDP(y)) {
#ifndef RECKLESS
    if (!(NUMBERP(x)))
      badx: wta(x, (char *)ARG1, s_max);
#endif
    return x;
  }
#ifdef FLOATS
  if (NINUMP(x)) {
# ifdef BIGDIG
    ASRTGO(NIMP(x), badx);
    if (BIGP(x)) {
      if (INUMP(y)) return BIGSIGN(x) ? y : x;
      ASRTGO(NIMP(y), bady);
      if (BIGP(y)) return (1==bigcomp(x, y)) ? y : x;
      ASRTGO(REALP(y), bady);
    big_dbl:
      if (-1 != scm_bigdblcomp(x, REALPART(y))) return y;
      z = int2dbl(x);
      ASRTER(0==scm_bigdblcomp(x, z), x, s_exactprob, s_max);
      return makdbl(z, 0.0);
    }
    ASRTGO(REALP(x), badx);
# else
    ASRTER(NIMP(x) && REALP(x), x, ARG1, s_max);
# endif
    if (INUMP(y)) return (REALPART(x) < (z = INUM(y))) ? makdbl(z, 0.0) : x;
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) {
      t = y; y = x; x = t; goto big_dbl;
    }
    ASRTGO(REALP(y), bady);
# else
    ASRTGO(NIMP(y) && REALP(y), bady);
# endif
    return (REALPART(x) < REALPART(y)) ? y : x;
  }
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) return BIGSIGN(y) ? x : y;
#  ifndef RECKLESS
    if (!(REALP(y)))
      bady: wta(y, (char *)ARG2, s_max);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && REALP(y)))
      bady: wta(y, (char *)ARG2, s_max);
#  endif
# endif
    return ((z = INUM(x)) < REALPART(y)) ? y : makdbl(z, 0.0);
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_max);
    if (INUMP(y)) return BIGSIGN(x) ? y : x;
    ASRTGO(NIMP(y) && BIGP(y), bady);
    return (1==bigcomp(x, y)) ? y : x;
  }
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
      bady: wta(y, (char *)ARG2, s_max);
#  endif
    return BIGSIGN(y) ? x : y;
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_max);
  ASRTER(INUMP(y), y, ARG2, s_max);
# endif
#endif
  return ((long)x < (long)y) ? y : x;
}

SCM scm_min(x, y)
     SCM x, y;
{
#ifdef FLOATS
  SCM t;
  double z;
#endif
  if (UNBNDP(y)) {
#ifndef RECKLESS
    if (!(NUMBERP(x)))
      badx: wta(x, (char *)ARG1, s_min);
#endif
    return x;
  }
#ifdef FLOATS
  if (NINUMP(x)) {
# ifdef BIGDIG
    ASRTGO(NIMP(x), badx);
    if (BIGP(x)) {
      if (INUMP(y)) return BIGSIGN(x) ? x : y;
      ASRTGO(NIMP(y), bady);
      if (BIGP(y)) return (-1==bigcomp(x, y)) ? y : x;
      ASRTGO(REALP(y), bady);
    big_dbl:
      if (1 != scm_bigdblcomp(x, REALPART(y))) return y;
      z = int2dbl(x);
      ASRTER(0==scm_bigdblcomp(x, z), x, s_exactprob, s_min);
      return makdbl(z, 0.0);
    }
    ASRTGO(REALP(x), badx);
# else
    ASRTER(NIMP(x) && REALP(x), x, ARG1, s_min);
# endif
    if (INUMP(y)) return (REALPART(x) > (z = INUM(y))) ? makdbl(z, 0.0) : x;
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) {
      t = y; y = x; x = t; goto big_dbl;
    }
    ASRTGO(REALP(y), bady);
# else
    ASRTGO(NIMP(y) && REALP(y), bady);
# endif
    return (REALPART(x) > REALPART(y)) ? y : x;
  }
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) return BIGSIGN(y) ? y : x;
#  ifndef RECKLESS
    if (!(REALP(y)))
      bady: wta(y, (char *)ARG2, s_min);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && REALP(y)))
      bady: wta(y, (char *)ARG2, s_min);
#  endif
# endif
    return ((z = INUM(x)) > REALPART(y)) ? y : makdbl(z, 0.0);
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_min);
    if (INUMP(y)) return BIGSIGN(x) ? x : y;
    ASRTGO(NIMP(y) && BIGP(y), bady);
    return (-1==bigcomp(x, y)) ? y : x;
  }
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
      bady: wta(y, (char *)ARG2, s_min);
#  endif
    return BIGSIGN(y) ? y : x;
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_min);
  ASRTER(INUMP(y), y, ARG2, s_min);
# endif
#endif
  return ((long)x > (long)y) ? y : x;
}

SCM sum(x, y)
     SCM x, y;
{
  if (UNBNDP(y)) {
    if (UNBNDP(x)) return INUM0;
#ifndef RECKLESS
    if (!(NUMBERP(x)))
      badx: wta(x, (char *)ARG1, s_sum);
#endif
    return x;
  }
#ifdef FLOATS
  if (NINUMP(x)) {
    SCM t;
# ifdef BIGDIG
    ASRTGO(NIMP(x), badx);
    if (BIGP(x)) {
      if (INUMP(y)) {t = x; x = y; y = t; goto intbig;}
      ASRTGO(NIMP(y), bady);
      if (BIGP(y)) {
	if (NUMDIGS(x) > NUMDIGS(y)) {t = x; x = y; y = t;}
	return addbig(BDIGITS(x), NUMDIGS(x), BIGSIGN(x), y, 0);
      }
      ASRTGO(INEXP(y), bady);
    bigreal: return makdbl(int2dbl(x)+REALPART(y), CPLXP(y)?IMAG(y):0.0);
    }
    ASRTGO(INEXP(x), badx);
# else
    ASRTGO(NIMP(x) && INEXP(x), badx);
# endif
    if (INUMP(y)) {t = x; x = y; y = t; goto intreal;}
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) {t = x; x = y; y = t; goto bigreal;}
#  ifndef RECKLESS
    else if (!(INEXP(y)))
      bady: wta(y, (char *)ARG2, s_sum);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && INEXP(y)))
      bady: wta(y, (char *)ARG2, s_sum);
#  endif
# endif
    {
      double i = 0.0;
      if (CPLXP(x)) i = IMAG(x);
      if (CPLXP(y)) i += IMAG(y);
      return makdbl(REALPART(x)+REALPART(y), i);
    }
  }
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y))
      intbig: {
#  ifndef DIGSTOOBIG
      long z = pseudolong(INUM(x));
      return addbig((BIGDIG *)&z, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  else
      BIGDIG zdigs[DIGSPERLONG];
      longdigs(INUM(x), zdigs);
      return addbig(zdigs, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  endif
    }
    ASRTGO(INEXP(y), bady);
# else
    ASRTGO(NIMP(y) && INEXP(y), bady);
# endif
  intreal: return makdbl(INUM(x)+REALPART(y), CPLXP(y)?IMAG(y):0.0);
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    SCM t;
    ASRTGO(NIMP(x) && BIGP(x), badx);
    if (INUMP(y)) {t = x; x = y; y = t; goto intbig;}
    ASRTGO(NIMP(y) && BIGP(y), bady);
    if (NUMDIGS(x) > NUMDIGS(y)) {t = x; x = y; y = t;}
    return addbig(BDIGITS(x), NUMDIGS(x), BIGSIGN(x), y, 0);
  }
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
      bady: wta(y, (char *)ARG2, s_sum);
#  endif
  intbig: {
#  ifndef DIGSTOOBIG
      long z = pseudolong(INUM(x));
      return addbig((BIGDIG *)&z, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  else
      BIGDIG zdigs[DIGSPERLONG];
      longdigs(INUM(x), zdigs);
      return addbig(zdigs, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0);
#  endif
    }
  }
# else
  ASRTGO(INUMP(x), badx);
  ASRTER(INUMP(y), y, ARG2, s_sum);
# endif
#endif
  x = INUM(x)+INUM(y);
  if (FIXABLE(x)) return MAKINUM(x);
#ifdef BIGDIG
  return long2big(x);
#else
# ifdef FLOATS
  return makdbl((double)x, 0.0);
# else
  wta(y, (char *)OVFLOW, s_sum);
# endif
#endif
}

SCM difference(x, y)
     SCM x, y;
{
#ifdef FLOATS
  if (NINUMP(x)) {
# ifndef RECKLESS
    if (!(NIMP(x)))
    badx: wta(x, (char *)ARG1, s_difference);
# endif
    if (UNBNDP(y)) {
# ifdef BIGDIG
      if (BIGP(x)) {
	x = copybig(x, !BIGSIGN(x));
	return NUMDIGS(x) * BITSPERDIG/CHAR_BIT <= sizeof(SCM) ?
	  big2inum(x, NUMDIGS(x)) : x;
      }
# endif
      ASRTGO(INEXP(x), badx);
      return makdbl(-REALPART(x), CPLXP(x)?-IMAG(x):0.0);
    }
    if (INUMP(y)) return sum(x, MAKINUM(-INUM(y)));
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(x)) {
      if (BIGP(y)) return (NUMDIGS(x) < NUMDIGS(y)) ?
		     addbig(BDIGITS(x), NUMDIGS(x), BIGSIGN(x), y, 0x0100) :
		     addbig(BDIGITS(y), NUMDIGS(y), BIGSIGN(y) ^ 0x0100, x, 0);
      ASRTGO(INEXP(y), bady);
      return makdbl(int2dbl(x)-REALPART(y), CPLXP(y)?-IMAG(y):0.0);
    }
    ASRTGO(INEXP(x), badx);
    if (BIGP(y)) return makdbl(REALPART(x)-int2dbl(y), CPLXP(x)?IMAG(x):0.0);
    ASRTGO(INEXP(y), bady);
# else
    ASRTGO(INEXP(x), badx);
    ASRTGO(NIMP(y) && INEXP(y), bady);
# endif
    if (CPLXP(x)) {
      if (CPLXP(y))
	return makdbl(REAL(x)-REAL(y), IMAG(x)-IMAG(y));
      else
	return makdbl(REAL(x)-REALPART(y), IMAG(x));
    }
    return makdbl(REALPART(x)-REALPART(y), CPLXP(y)?-IMAG(y):0.0);
  }
  if (UNBNDP(y)) {x = -INUM(x); goto checkx;}
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) {
#  ifndef DIGSTOOBIG
      long z = pseudolong(INUM(x));
      return addbig((BIGDIG *)&z, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  else
      BIGDIG zdigs[DIGSPERLONG];
      longdigs(INUM(x), zdigs);
      return addbig(zdigs, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  endif
    }
#  ifndef RECKLESS
    if (!(INEXP(y)))
    bady: wta(y, (char *)ARG2, s_difference);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && INEXP(y)))
    bady: wta(y, (char *)ARG2, s_difference);
#  endif
# endif
    return makdbl(INUM(x)-REALPART(y), CPLXP(y)?-IMAG(y):0.0);
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_difference);
    if (UNBNDP(y)) {
      x = copybig(x, !BIGSIGN(x));
      return NUMDIGS(x) * BITSPERDIG/CHAR_BIT <= sizeof(SCM) ?
	big2inum(x, NUMDIGS(x)) : x;
    }
    if (INUMP(y)) {
#  ifndef DIGSTOOBIG
      long z = pseudolong(INUM(y));
      return addbig((BIGDIG *)&z, DIGSPERLONG, (y < 0) ? 0 : 0x0100, x, 0);
#  else
      BIGDIG zdigs[DIGSPERLONG];
      longdigs(INUM(x), zdigs);
      return addbig(zdigs, DIGSPERLONG, (y < 0) ? 0 : 0x0100, x, 0);
#  endif
    }
    ASRTGO(NIMP(y) && BIGP(y), bady);
    return (NUMDIGS(x) < NUMDIGS(y)) ?
      addbig(BDIGITS(x), NUMDIGS(x), BIGSIGN(x), y, 0x0100) :
      addbig(BDIGITS(y), NUMDIGS(y), BIGSIGN(y) ^ 0x0100, x, 0);
  }
  if (UNBNDP(y)) {x = -INUM(x); goto checkx;}
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
    bady: wta(y, (char *)ARG2, s_difference);
#  endif
    {
#  ifndef DIGSTOOBIG
      long z = pseudolong(INUM(x));
      return addbig((BIGDIG *)&z, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  else
      BIGDIG zdigs[DIGSPERLONG];
      longdigs(INUM(x), zdigs);
      return addbig(zdigs, DIGSPERLONG, (x < 0) ? 0x0100 : 0, y, 0x0100);
#  endif
    }
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_difference);
  if (UNBNDP(y)) {x = -INUM(x); goto checkx;}
  ASRTER(INUMP(y), y, ARG2, s_difference);
# endif
#endif
  x = INUM(x)-INUM(y);
 checkx:
  if (FIXABLE(x)) return MAKINUM(x);
#ifdef BIGDIG
  return long2big(x);
#else
# ifdef FLOATS
  return makdbl((double)x, 0.0);
# else
  wta(y, (char *)OVFLOW, s_difference);
# endif
#endif
}

SCM product(x, y)
     SCM x, y;
{
  if (UNBNDP(y)) {
    if (UNBNDP(x)) return MAKINUM(1L);
#ifndef RECKLESS
    if (!(NUMBERP(x)))
      badx: wta(x, (char *)ARG1, s_product);
#endif
    return x;
  }
#ifdef FLOATS
  if (NINUMP(x)) {
    SCM t;
# ifdef BIGDIG
    ASRTGO(NIMP(x), badx);
    if (BIGP(x)) {
      if (INUMP(y)) {t = x; x = y; y = t; goto intbig;}
      ASRTGO(NIMP(y), bady);
      if (BIGP(y)) return mulbig(BDIGITS(x), NUMDIGS(x), BDIGITS(y), NUMDIGS(y),
				 BIGSIGN(x) ^ BIGSIGN(y));
      ASRTGO(INEXP(y), bady);
    bigreal:
      return bigdblop('*', x, REALPART(y), CPLXP(y) ? IMAG(y) : 0.0);
    }
    ASRTGO(INEXP(x), badx);
# else
    ASRTGO(NIMP(x) && INEXP(x), badx);
# endif
    if (INUMP(y)) {t = x; x = y; y = t; goto intreal;}
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) {t = x; x = y; y = t; goto bigreal;}
#  ifndef RECKLESS
    else if (!(INEXP(y)))
      bady: wta(y, (char *)ARG2, s_product);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && INEXP(y)))
      bady: wta(y, (char *)ARG2, s_product);
#  endif
# endif
    if (CPLXP(x)) {
      if (CPLXP(y))
	return makdbl(REAL(x)*REAL(y)-IMAG(x)*IMAG(y),
		      REAL(x)*IMAG(y)+IMAG(x)*REAL(y));
      else
	return makdbl(REAL(x)*REALPART(y), IMAG(x)*REALPART(y));
    }
    return makdbl(REALPART(x)*REALPART(y),
		  CPLXP(y)?REALPART(x)*IMAG(y):0.0);
  }
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) {
    intbig: if (INUM0==x) return x; if (MAKINUM(1L)==x) return y;
    {
#  ifndef DIGSTOOBIG
      long z = pseudolong(INUM(x));
      return mulbig((BIGDIG *)&z, DIGSPERLONG, BDIGITS(y), NUMDIGS(y),
		    BIGSIGN(y) ? (x>0) : (x<0));
#  else
      BIGDIG zdigs[DIGSPERLONG];
      longdigs(INUM(x), zdigs);
      return mulbig(zdigs, DIGSPERLONG, BDIGITS(y), NUMDIGS(y),
		    BIGSIGN(y) ? (x>0) : (x<0));
#  endif
    }
    }
    ASRTGO(INEXP(y), bady);
# else
    ASRTGO(NIMP(y) && INEXP(y), bady);
# endif
  intreal: return makdbl(INUM(x)*REALPART(y), CPLXP(y)?INUM(x)*IMAG(y):0.0);
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTGO(NIMP(x) && BIGP(x), badx);
    if (INUMP(y)) {SCM t = x; x = y; y = t; goto intbig;}
    ASRTGO(NIMP(y) && BIGP(y), bady);
    return mulbig(BDIGITS(x), NUMDIGS(x), BDIGITS(y), NUMDIGS(y),
		  BIGSIGN(x) ^ BIGSIGN(y));
  }
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
      bady: wta(y, (char *)ARG2, s_product);
#  endif
  intbig: if (INUM0==x) return x; if (MAKINUM(1L)==x) return y;
  {
#  ifndef DIGSTOOBIG
    long z = pseudolong(INUM(x));
    return mulbig((BIGDIG *)&z, DIGSPERLONG, BDIGITS(y), NUMDIGS(y),
		  BIGSIGN(y) ? (x>0) : (x<0));
#  else
    BIGDIG zdigs[DIGSPERLONG];
    longdigs(INUM(x), zdigs);
    return mulbig(zdigs, DIGSPERLONG, BDIGITS(y), NUMDIGS(y),
		  BIGSIGN(y) ? (x>0) : (x<0));
#  endif
  }
  }
# else
  ASRTGO(INUMP(x), badx);
  ASRTER(INUMP(y), y, ARG2, s_product);
# endif
#endif
  {
    long i, j, k;
    i = INUM(x);
    if (0==i) return x;
    j = INUM(y);
    k = i * j;
    y = MAKINUM(k);
    if (k != INUM(y) || k/i != j)
#ifdef BIGDIG
      {
	int sgn = (i < 0) ^ (j < 0);
# ifndef DIGSTOOBIG
	i = pseudolong(i);
	j = pseudolong(j);
	return mulbig((BIGDIG *)&i, DIGSPERLONG,
		      (BIGDIG *)&j, DIGSPERLONG, sgn);
# else /* DIGSTOOBIG */
	BIGDIG idigs[DIGSPERLONG];
	BIGDIG jdigs[DIGSPERLONG];
	longdigs(i, idigs);
	longdigs(j, jdigs);
	return mulbig(idigs, DIGSPERLONG, jdigs, DIGSPERLONG, sgn);
# endif
      }
#else
# ifdef FLOATS
    return makdbl(((double)i)*((double)j), 0.0);
# else
    wta(y, (char *)OVFLOW, s_product);
# endif
#endif
    return y;
  }
}
  /* Use "Smith's formula" to extend dynamic range */
  /* David Goldberg
     What Every Computer Scientist Should Know About Floating-Point Arithmetic
     http://cch.loria.fr/documentation/IEEE754/ACM/goldberg.pdf */
SCM divide(x, y)
     SCM x, y;
{
#ifdef FLOATS
  double den, a = 1.0;
  if (NINUMP(x)) {
# ifndef RECKLESS
    if (!(NIMP(x)))
      badx: wta(x, (char *)ARG1, s_divide);
# endif
    if (UNBNDP(y)) {
# ifdef BIGDIG
      if (BIGP(x)) return inex_divintbig(MAKINUM(1L), x);
# endif
      /* reciprocal */
      ASRTGO(INEXP(x), badx);
      if (REALP(x)) return makdbl(1.0/REALPART(x), 0.0);
      {
	y = x;
	a = 1.0;
	goto real_over_complex;
      }
    }
# ifdef BIGDIG
    if (BIGP(x)) {
      SCM z;
      if (INUMP(y)) {
        z = INUM(y);
        ASRTER(z, y, OVFLOW, s_divide);
	if (1==z) return x;
        if (z < 0) z = -z;
        if (z < BIGRAD) {
          SCM w = copybig(x, BIGSIGN(x) ? (y>0) : (y<0));
	  int sts = divbigdig(BDIGITS(w), NUMDIGS(w), (BIGDIG)z);
	  if (sts) {
	    bigrecy(w);
	    return bigdblop('/', x, INUM(y), 0.0);
	  }
	  else return normbig(w);
	}
#  ifndef DIGSTOOBIG
        z = pseudolong(z);
        z = divbigbig(BDIGITS(x), NUMDIGS(x), (BIGDIG *)&z, DIGSPERLONG,
                      BIGSIGN(x) ? (y>0) : (y<0), 4);
#  else
	{
	  BIGDIG zdigs[DIGSPERLONG];
	  longdigs(z, zdigs);
	  z = divbigbig(BDIGITS(x), NUMDIGS(x), zdigs, DIGSPERLONG,
			BIGSIGN(x) ? (y>0) : (y<0), 4);
	}
#  endif
        return z ? z : bigdblop('/', x, INUM(y), 0.0);
      }
      ASRTGO(NIMP(y), bady);
      if (BIGP(y)) {
	z = divbigbig(BDIGITS(x), NUMDIGS(x), BDIGITS(y), NUMDIGS(y),
		      BIGSIGN(x) ^ BIGSIGN(y), 4);
	return z ? z : inex_divintbig(x, y);
      }
      ASRTGO(INEXP(y), bady);
      return bigdblop('/', x, REALPART(y), CPLXP(y) ? IMAG(y) : 0.0);
    }
# endif
    ASRTGO(INEXP(x), badx);
    if (INUMP(y)) {den = INUM(y); goto basic_div;}
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) return bigdblop('\\', y, REALPART(x), CPLXP(x) ? IMAG(x) : 0.0);
    ASRTGO(INEXP(y), bady);
# else
    ASRTGO(NIMP(y) && INEXP(y), bady);
# endif
    if (REALP(y)) {
      den = REALPART(y);
    basic_div: return makdbl(REALPART(x)/den, CPLXP(x)?IMAG(x)/den:0.0);
    }
    a = REALPART(x);
    if (REALP(x)) goto real_over_complex;
    /* Both x and y are complex */
    /* Use "Smith's formula" to extend dynamic range */
    {
      double b = IMAG(x);
      double c = REALPART(y);
      double d = IMAG(y);
      if ((d > 0 ? d : -d) < (c > 0 ? c : -c)) {
	double r = d/c;
	double i = c + d*r;
	return makdbl((a + b*r)/i, (b - a*r)/i);
      }
      {
	double r = c/d;
	double i = d + c*r;
	return makdbl((b + a*r)/i, (-a + b*r)/i);
      }
    }
  }
  if (UNBNDP(y)) {
    if ((MAKINUM(1L)==x) || (MAKINUM(-1L)==x)) return x;
    return makdbl(1.0/((double)INUM(x)), 0.0);
  }
  if (NINUMP(y)) {
# ifdef BIGDIG
    ASRTGO(NIMP(y), bady);
    if (BIGP(y)) return inex_divintbig(x, y); /* bigdblop('\\', y, INUM(x), 0.0); */
#  ifndef RECKLESS
    if (!(INEXP(y)))
      bady: wta(y, (char *)ARG2, s_divide);
#  endif
# else
#  ifndef RECKLESS
    if (!(NIMP(y) && INEXP(y)))
      bady: wta(y, (char *)ARG2, s_divide);
#  endif
# endif
    if (REALP(y)) return makdbl(INUM(x)/REALPART(y), 0.0);
    a = INUM(x);
  real_over_complex:
    /* Both x and y are complex */
    /* Use "Smith's formula" to extend dynamic range */
    {
      double c = REALPART(y);
      double d = IMAG(y);
      if ((d > 0 ? d : -d) < (c > 0 ? c : -c)) {
	double r = d/c;
	double i = c + d*r;
	return makdbl((a)/i, (- a*r)/i);
      }
      {
	double r = c/d;
	double i = d + c*r;
	return makdbl((a*r)/i, (-a)/i);
      }
    }
  }
#else
# ifdef BIGDIG
  if (NINUMP(x)) {
    SCM z;
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_divide);
    if (UNBNDP(y)) goto ov;
    if (INUMP(y)) {
      z = INUM(y);
      if (!z) goto ov;
      if (1==z) return x;
      if (z < 0) z = -z;
      if (z < BIGRAD) {
        SCM w = copybig(x, BIGSIGN(x) ? (y>0) : (y<0));
	int sts = divbigdig(BDIGITS(w), NUMDIGS(w), (BIGDIG)z);
        if (sts) {
	  bigrecy(w);
	  goto ov;
	}
        return w;
      }
#  ifndef DIGSTOOBIG
      z = pseudolong(z);
      z = divbigbig(BDIGITS(x), NUMDIGS(x), (BIGDIG *)&z, DIGSPERLONG,
		    BIGSIGN(x) ? (y>0) : (y<0), 4);
#  else
      {
	BIGDIG zdigs[DIGSPERLONG];
	longdigs(z, zdigs);
	z = divbigbig(BDIGITS(x), NUMDIGS(x), zdigs, DIGSPERLONG,
		      BIGSIGN(x) ? (y>0) : (y<0), 4);
      }
#  endif
    } else {
      ASRTGO(NIMP(y) && BIGP(y), bady);
      z = divbigbig(BDIGITS(x), NUMDIGS(x), BDIGITS(y), NUMDIGS(y),
		    BIGSIGN(x) ^ BIGSIGN(y), 4);
    }
    if (!z) goto ov;
    return z;
  }
  if (UNBNDP(y)) {
    if ((MAKINUM(1L)==x) || (MAKINUM(-1L)==x)) return x;
    goto ov;
  }
  if (NINUMP(y)) {
#  ifndef RECKLESS
    if (!(NIMP(y) && BIGP(y)))
      bady: wta(y, (char *)ARG2, s_divide);
#  endif
    goto ov;
  }
# else
  ASRTER(INUMP(x), x, ARG1, s_divide);
  if (UNBNDP(y)) {
    if ((MAKINUM(1L)==x) || (MAKINUM(-1L)==x)) return x;
    goto ov;
  }
  ASRTER(INUMP(y), y, ARG2, s_divide);
# endif
#endif
  {
    long z = INUM(y);
    if ((0==z) || INUM(x)%z) goto ov;
    z = INUM(x)/z;
    if (FIXABLE(z)) return MAKINUM(z);
#ifdef BIGDIG
    return long2big(z);
#endif
#ifdef FLOATS
  ov: return makdbl(((double)INUM(x))/((double)INUM(y)), 0.0);
#else
  ov: wta(x, (char *)OVFLOW, s_divide);
#endif
  }
}

SCM ilog(m, b, k, n)
     unsigned long m;
     SCM b, k;
     unsigned long *n;
{
  /* printf("call ilog %ld ", m); scm_iprin1(b, cur_outp, 0); printf(" "); scm_iprin1(k, cur_outp, 0); printf("\n"); */
  if (BOOL_T==lessp(k, b)) return k;
  *n += m;
  {
    SCM q = ilog(2*m, product(b, b), lquotient(k, b), n);
    /* printf("return ilog "); scm_iprin1(q, cur_outp, 0); printf("\n"); */
    if (BOOL_T==lessp(q, b)) return q;
    *n += m;
    return lquotient(q, b);
  }
}

SCM scm_intlog(base, k)
     SCM base, k;
{
  unsigned long n = 1;
  ASRTER(INUMP(base) || (NIMP(base) && BIGP(base)), base, ARG1, s_intlog);
  ASRTER(BOOL_T==lessp(MAKINUM(1L), base), base, OUTOFRANGE, s_intlog);
  ASRTER((INUMP(k) && k > 0) || (NIMP(k) && TYP16(k)==tc16_bigpos), k, ARG2, s_intlog);
  if (BOOL_T==lessp(k, base)) return INUM0;
  ilog(1, base, lquotient(k, base), &n);
  return MAKINUM(n);
}

#ifdef INUMS_ONLY
# define eqv eqp
#endif
SCM scm_cintlog(base, k)
     SCM base, k;
{
  SCM il = scm_intlog(base, k);
  return (BOOL_T==eqv(k, scm_intexpt(base, il))) ? il : sum(MAKINUM(1L), il);
}

SCM scm_intexpt(z1, z2)
     SCM z1, z2;
{
  SCM acc = MAKINUM(1L);
  long iz2;
#ifdef FLOATS
  double dacc, dz1;
#endif
  if (INUM0==z2) return sum(acc, product(z1, INUM0));
  ASRTER(INUMP(z2), z2, ARG2, s_intexpt);
  if (acc==z1) return z1;
  if (MAKINUM(-1L)==z1) return BOOL_F==evenp(z2)?z1:acc;
  iz2 = INUM(z2);
  if (iz2 < 0L) {
    iz2 = -iz2;
    z1 = divide(z1, UNDEFINED);
  }
  if (INUMP(z1)) {
    long tmp, iacc = 1L, iz1 = INUM(z1);
    while (1) {
      if (0L==iz2) {
	acc = long2num(iacc);
	break;
      }
      if (0L==iz1) return z1;
      if (1L==iz2) {
	tmp = iacc*iz1;
	if (tmp/iacc != iz1) {
	overflow:
	  z1 = long2num(iz1);
	  acc = long2num(iacc);
	  ASRTGO(NFALSEP(z1) && NFALSEP(acc), errout);
	  goto gencase;
	}
	acc = long2num(tmp);
	break;
      }
      if (iz2 & 1) {
	tmp = iacc*iz1;
	if (tmp/iacc != iz1) goto overflow;
	iacc = tmp;
	iz2 = iz2 - 1L;		/* so jumping to gencase works  */
      }
      tmp = iz1*iz1;
      if (tmp/iz1 != iz1) goto overflow;
      iz1 = tmp;
      iz2 >>= 1;
    }
#ifndef RECKLESS
    if (FALSEP(acc))
    errout: wta(UNDEFINED, (char *)OVFLOW, s_intexpt);
#endif
    goto ret;
  }
  ASRTER(NIMP(z1), z1, ARG1, s_intexpt);
#ifdef FLOATS
  if (REALP(z1)) {
    dz1 = REALPART(z1);
    dacc = 1.0;
    while(1) {
      if (0L==iz2) break;
      if (1L==iz2) {dacc = dacc*dz1; break;}
      if (iz2 & 1) dacc = dacc*dz1;
      dz1 = dz1*dz1;
      iz2 >>= 1;
    }
    return makdbl(dacc, 0.0);
  }
#endif
 gencase:
  {
    SCM tz1 = z1;
    while(1) {
      SCM tmp;
      if (0L==iz2) break;
      if (1L==iz2) {
	tmp = acc;
	acc = product(tmp, tz1);
#ifdef BIGDIG
	if (acc != tmp) bigrecy(tmp);
#endif
	break;
      }
      if (iz2 & 1) {
	tmp = acc;
	acc = product(tmp, tz1);
#ifdef BIGDIG
	if (acc != tmp) bigrecy(tmp);
#endif
      }
      tmp = tz1;
      tz1 = product(tmp, tmp);
#ifdef BIGDIG
      if (tmp != z1 && tmp != acc) bigrecy(tmp);
#endif
      iz2 >>= 1;
    }
#ifdef BIGDIG
    if (tz1 != acc && tz1 != z1) bigrecy(tz1);
#endif
  }
 ret: return acc;
}

#ifdef FLOATS
# ifndef HAVE_ATANH
double asinh(x)
     double x;
{
  return log(x+sqrt(x*x+1));
}

double acosh(x)
     double x;
{
  return log(x+sqrt(x*x-1));
}

double atanh(x)
     double x;
{
  return 0.5*log((1+x)/(1-x));
}
# endif

double scm_truncate(x)
     double x;
{
  if (x < 0.0) return -floor(-x);
  return floor(x);
}

double scm_round(x)
     double x;
{
  double plus_half = x + 0.5;
  double result = floor(plus_half);
  /* Adjust so that the round is towards even.  */
  return (plus_half==result && plus_half / 2 != floor(plus_half / 2))
    ? result - 1 : result;
}

struct dpair {double x, y;};

void two_doubles(z1, z2, sstring, xy)
     SCM z1, z2;
     char *sstring;
     struct dpair *xy;
{
  if (INUMP(z1)) xy->x = INUM(z1);
  else {
# ifdef BIGDIG
    ASRTGO(NIMP(z1), badz1);
    if (BIGP(z1)) xy->x = int2dbl(z1);
    else {
#  ifndef RECKLESS
      if (!(REALP(z1)))
	badz1: wta(z1, (char *)ARG1, sstring);
#  endif
      xy->x = REALPART(z1);}
# else
    {ASRTER(NIMP(z1) && REALP(z1), z1, ARG1, sstring);
    xy->x = REALPART(z1);}
# endif
  }
  if (INUMP(z2)) xy->y = INUM(z2);
  else {
# ifdef BIGDIG
    ASRTGO(NIMP(z2), badz2);
    if (BIGP(z2)) xy->y = int2dbl(z2);
    else {
#  ifndef RECKLESS
      if (!(REALP(z2)))
	badz2: wta(z2, (char *)ARG2, sstring);
#  endif
      xy->y = REALPART(z2);}
# else
    {
      ASRTER(NIMP(z2) && REALP(z2), z2, ARG2, sstring);
      xy->y = REALPART(z2);
    }
# endif
  }
}

SCM expt(z1, z2)
     SCM z1, z2;
{
  struct dpair xy;
  two_doubles(z1, z2, s_expt, &xy);
  return makdbl(pow(xy.x, xy.y), 0.0);
}
SCM latan2(z1, z2)
     SCM z1, z2;
{
  struct dpair xy;
  two_doubles(z1, z2, s_atan2, &xy);
  return makdbl(atan2(xy.x, xy.y), 0.0);
}
SCM makrect(z1, z2)
     SCM z1, z2;
{
  struct dpair xy;
  two_doubles(z1, z2, s_makrect, &xy);
  return makdbl(xy.x, xy.y);
}
SCM makpolar(z1, z2)
     SCM z1, z2;
{
  struct dpair xy;
  two_doubles(z1, z2, s_makpolar, &xy);
  return makdbl(xy.x*cos(xy.y), xy.x*sin(xy.y));
}

SCM real_part(z)
     SCM z;
{
  if (NINUMP(z)) {
# ifdef BIGDIG
    ASRTGO(NIMP(z), badz);
    if (BIGP(z)) return z;
#  ifndef RECKLESS
    if (!(INEXP(z)))
      badz: wta(z, (char *)ARG1, s_real_part);
#  endif
# else
    ASRTER(NIMP(z) && INEXP(z), z, ARG1, s_real_part);
# endif
    if (CPLXP(z)) return makdbl(REAL(z), 0.0);
  }
  return z;
}
SCM imag_part(z)
     SCM z;
{
  if (INUMP(z)) return INUM0;
# ifdef BIGDIG
  ASRTGO(NIMP(z), badz);
  if (BIGP(z)) return INUM0;
#  ifndef RECKLESS
  if (!(INEXP(z)))
    badz: wta(z, (char *)ARG1, s_imag_part);
#  endif
# else
  ASRTER(NIMP(z) && INEXP(z), z, ARG1, s_imag_part);
# endif
  if (CPLXP(z)) return makdbl(IMAG(z), 0.0);
  return flo0;
}

SCM scm_abs(z)
     SCM z;
{
  if (INUMP(z)) return scm_iabs(z);
  ASRTGO(NIMP(z), badz);
# ifdef BIGDIG
  if (BIGP(z)) return scm_iabs(z);
# endif
  if (!REALP(z))
    badz: wta(z, (char *)ARG1, s_abs);
  return makdbl(fabs(REALPART(z)), 0.0);
}

SCM scm_magnitude(z)
     SCM z;
{
  if (INUMP(z)) return scm_iabs(z);
  ASRTGO(NIMP(z), badz);
# ifdef BIGDIG
  if (BIGP(z)) return scm_iabs(z);
# endif
  if (!INEXP(z))
    badz: wta(z, (char *)ARG1, s_magnitude);
  if (CPLXP(z))
    {
      double i = IMAG(z), r = REAL(z);
      if (i < 0) i = -i;
      if (r < 0) r = -r;
      if (i < r) {
	double q = i / r;
	return makdbl(r * sqrt(1 + q * q), 0.0);
      }
      if (0.0==i) return i;
      {
	double q = r / i;
	return makdbl(i * sqrt(1 + q * q), 0.0);
      }
    }
  return makdbl(fabs(REALPART(z)), 0.0);
}

SCM angle(z)
     SCM z;
{
  double x, y = 0.0;
  if (INUMP(z)) {x = (z>=INUM0) ? 1.0 : -1.0; goto do_angle;}
# ifdef BIGDIG
  ASRTGO(NIMP(z), badz);
  if (BIGP(z)) {x = (TYP16(z)==tc16_bigpos) ? 1.0 : -1.0; goto do_angle;}
#  ifndef RECKLESS
  if (!(INEXP(z))) {
    badz: wta(z, (char *)ARG1, s_angle);}
#  endif
# else
  ASRTER(NIMP(z) && INEXP(z), z, ARG1, s_angle);
# endif
  if (REALP(z)) {x = REALPART(z); goto do_angle;}
  x = REAL(z); y = IMAG(z);
do_angle:
  return makdbl(atan2(y, x), 0.0);
}


SCM ex2in(z)
     SCM z;
{
  if (INUMP(z)) return makdbl((double)INUM(z), 0.0);
  ASRTGO(NIMP(z), badz);
  if (INEXP(z)) return z;
# ifdef BIGDIG
  if (BIGP(z)) return makdbl(int2dbl(z), 0.0);
# endif
 badz: wta(z, (char *)ARG1, s_ex2in);
}
SCM in2ex(z)
     SCM z;
{
  if (INUMP(z)) return z;
# ifdef BIGDIG
  ASRTGO(NIMP(z), badz);
  if (BIGP(z)) return z;
#  ifndef RECKLESS
  if (!(REALP(z)))
    badz: wta(z, (char *)ARG1, s_in2ex);
#  endif
# else
  ASRTER(NIMP(z) && REALP(z), z, ARG1, s_in2ex);
# endif
# ifdef BIGDIG
  {
    double u = floor(REALPART(z)+0.5);
    if ((u <= MOST_POSITIVE_FIXNUM)
#  ifdef hpux
	&& (-u <= -MOST_NEGATIVE_FIXNUM) /* workaround for HP700 cc bug */
#  endif
	) {
      SCM ans = MAKINUM((long)u);
      if (INUM(ans)==(long)u) return ans;
    }
    ASRTGO(!((u==2*u) || (u)!=(u)), badz); /* problem? */
    return dbl2big(u);
  }
# else
  return MAKINUM((long)floor(REALPART(z)+0.5));
# endif
}
#else				/* ~FLOATS */
static char s_trunc[] = "truncate";
SCM numident(x)
     SCM x;
{
# ifdef BIGDIG
  ASRTER(INUMP(x) || (NIMP(x) && BIGP(x)), x, ARG1, s_trunc);
# else
  ASRTER(INUMP(x), x, ARG1, s_trunc);
# endif
  return x;
}
#endif				/* FLOATS */

SCM scm_iabs(x)
     SCM x;
{
#ifdef BIGDIG
  if (NINUMP(x)) {
    ASRTER(NIMP(x) && BIGP(x), x, ARG1, s_abs);
    if (TYP16(x)==tc16_bigpos) return x;
    return copybig(x, 0);
  }
#else
  ASRTER(INUMP(x), x, ARG1, s_abs);
#endif
  if (INUM(x) >= 0) return x;
  x = -INUM(x);
  if (!POSFIXABLE(x))
#ifdef BIGDIG
    return long2big(x);
#else
    wta(MAKINUM(-x), (char *)OVFLOW, s_abs);
#endif
  return MAKINUM(x);
}

#ifdef BIGDIG
# ifdef FLOATS
SCM dbl2big(d)
     double d;			/* must be integer */
{
  sizet i = 0;
  long c;
  BIGDIG *digits;
  SCM ans;
  double u = (d < 0)?-d:d;
  ASRTER(u == u && u != u/2, makdbl(d, 0.0), OVFLOW, "dbl2big");
  while (0 != floor(u)) {u /= BIGRAD;i++;}
  ans = mkbig(i, d < 0);
  digits = BDIGITS(ans);
  while (i--) {
    u = ldexp(u, BITSPERDIG);
    c = floor(u);
    u -= c;
    digits[i] = c;
  }
  ASRTER(0==u, makdbl(d, 0.0), OVFLOW, "dbl2big");
  return normbig(ans);
}
/* This turns out to need explicit rounding for bignums */
double int2dbl(b)
     SCM b;
{
  if (INUMP(b)) return (double)(INUM(b));
  {
    SCM num = scm_iabs(b), quo = num;
    int bex = INUM(scm_intlength(num)) - dbl_mant_dig;
    double ans = 0.0;
    if (bex > 0) {
      SCM den = scm_ash(MAKINUM(1L), MAKINUM(bex));
      quo = scm_round_quotient(num, den);
      bigrecy(den);
    }
    /* quo may not be a bignum */
    if (INUMP(quo))
      ans = ldexp((double)(INUM(quo)), bex);
    else {
      sizet i = NUMDIGS(quo);
      sizet j = (dbl_mant_dig + BITSPERDIG - 1)/BITSPERDIG;
      BIGDIG *digits = BDIGITS(quo);
      if (j < i) j = i - j;
      else j = 0;
      while (i-- > j) ans = digits[i] + ldexp(ans, BITSPERDIG);
      bex += j * BITSPERDIG;
      if (bex > 0) ans = ldexp(ans, bex);
    }
    if (quo != num) bigrecy(quo);
    if (num != b) bigrecy(num);
    if (tc16_bigneg==TYP16(b)) return -ans;
    return ans;
  }
}
static SCM bigdblop(op, b, re, im)
     int op;
     SCM b;
     double re, im;
{
  double bm = 0.0;
  int i = 0;
  if (NUMDIGS(b)*BITSPERDIG < DBL_MAX_EXP) {
    bm = int2dbl(b);
  }
  else {
    i = INUM(scm_intlength(b));
    if (i < DBL_MAX_EXP) {
      i = 0;
      bm = int2dbl(b);
    }
    else {
      i = i + 1 - DBL_MAX_EXP;
      bm = ldexp(int2dbl(b), -i);
    }
  }
  switch (op) {
  case '*':
    return makdbl(ldexp(bm*re, i), 0.0==im ? 0.0 : ldexp(bm*im, i));
  case '/':
    if (0.0==im) return makdbl(bm/re, 0.0);
    {
      double d = re*re + im*im;
      return makdbl(ldexp(bm*(re/d), i), ldexp(-bm*(im/d), i));
    }
  case '\\':
    return makdbl(ldexp(re/bm, -i), 0.0==im ? 0.0 : ldexp(im/bm, -i));
  default:
    return UNSPECIFIED;
  }
}
/* now able to return unnomalized doubles. */
static SCM inex_divintbig(a, b)
     SCM a, b;
{
  double r;
  {
    int sgn = (((INUMP(a) ? (INUM(a) < 0):BIGSIGN(a))==0) ^
	       (BIGSIGN(b)==0)) ? -1 : 1;
    SCM ma = scm_abs(a);
    SCM mb = scm_abs(b);
    int la = INUM(scm_intlength(ma));
    int lb = INUM(scm_intlength(mb));
    if (la <= DBL_MAX_EXP && lb <= DBL_MAX_EXP) {
      r = int2dbl(a) / int2dbl(b);
    }
    else if (la > DBL_MAX_EXP && lb > DBL_MAX_EXP) {
      int k = (la > lb ? la : lb) - DBL_MAX_EXP;
      r = sgn *
	int2dbl(scm_ash(ma, MAKINUM(-k))) /
	int2dbl(scm_ash(mb, MAKINUM(-k)));
    } else if (la > lb) {
      int k = la - DBL_MAX_EXP;
      r = sgn * ldexp(int2dbl(scm_ash(ma, MAKINUM(-k))) / int2dbl(mb), k);
    } else {
      int k = lb - DBL_MAX_EXP;
      r = sgn * ldexp(int2dbl(ma) / int2dbl(scm_ash(mb, MAKINUM(-k))), -k);
    }
  }
  return makdbl(r, 0.0);
}

SCM scm_dfloat_parts(f)
     SCM f;
{
  int expt, ndig = dbl_mant_dig;
  double mant = frexp(num2dbl(f, (char *)ARG1, s_dfloat_parts), &expt);
#  ifdef DBL_MIN_EXP
  if (expt < DBL_MIN_EXP)
    ndig -= DBL_MIN_EXP - expt;
#  endif
  mant *= ldexp(1.0, ndig);
  expt -= ndig;
  return scm_values(dbl2big(mant), MAKINUM(expt), EOL, s_dfloat_parts);
}
static char s_make_dfloat[] = "make-double-float";
SCM scm_make_dfloat(mant, expt)
     SCM mant, expt;
{
  double dmant = num2dbl(mant, (char *)ARG1, s_make_dfloat);
  int e = INUM(expt);
  ASRTER(INUMP(expt), expt, ARG2, s_make_dfloat);
  ASRTER((dmant < 0 ? -dmant : dmant) <= max_dbl_int, mant,
	 OUTOFRANGE, s_make_dfloat);
  return makdbl(ldexp(dmant, e), 0.0);
}
# endif
#endif

unsigned long hasher(obj, n, d)
     SCM obj;
     unsigned long n;
     sizet d;
{
  switch (7 & PTR2INT(obj)) {
  case 2: case 6:		/* INUMP(obj) */
    return INUM(obj) % n;
  case 4:
    if (ICHRP(obj))
      return (unsigned)(downcase[ICHR(obj)]) % n;
    switch ((int) obj) {
#ifndef SICP
    case (int) EOL: d = 256; break;
#endif
    case (int) BOOL_T: d = 257; break;
    case (int) BOOL_F: d = 258; break;
    case (int) EOF_VAL: d = 259; break;
    default: d = 263;		/* perhaps should be error */
    }
    return d % n;
  default: return 263 % n;	/* perhaps should be error */
  case 0:
    switch TYP7(obj) {
    default: return 263 % n;
    case tc7_smob:
      switch TYP16(obj) {
      case tcs_bignums:
      bighash: return INUM(modulo(obj, MAKINUM(n)));
      default: return 263 % n;
#ifdef FLOATS
      case tc16_flo:
	if (REALP(obj)) {
	  double r = REALPART(obj);
	  if (floor(r)==r) {
	    obj = in2ex(obj);
	    if (IMP(obj)) return INUM(obj) % n;
	    goto bighash;
	  }
	}
	obj = number2string(obj, MAKINUM(10));
#endif
      }
    case tcs_symbols: case tc7_string:
      return strhash(UCHARS(obj), (sizet) LENGTH(obj), n);
    case tc7_vector: {
      sizet len = LENGTH(obj);
      SCM *data = VELTS(obj);
      if (len>5) {
	sizet i = d/2;
	unsigned long h = 1;
	while (i--) h = ((h<<8) + (hasher(data[h % len], n, 2))) % n;
	return h;
      }
      else {
	sizet i = len;
	unsigned long h = (n)-1;
	while (i--) h = ((h<<8) + (hasher(data[i], n, d/len))) % n;
	return h;
      }
    }
    case tcs_cons_imcar: case tcs_cons_nimcar:
      if (d) return (hasher(CAR(obj), n, d/2)+hasher(CDR(obj), n, d/2)) % n;
      else return 1;
    case tc7_port:
      return ((RDNG & CAR(obj)) ? 260 : 261) % n;
    case tcs_closures: case tc7_contin: case tcs_subrs:
      return 262 % n;
    }
  }
}

static char s_hashv[] = "hashv", s_hashq[] = "hashq";
extern char s_obunhash[];
#define s_hash (&s_obunhash[9])

SCM hash(obj, n)
     SCM obj;
     SCM n;
{
  ASRTER(INUMP(n) && 0 <= n, n, ARG2, s_hash);
  return MAKINUM(hasher(obj, INUM(n), 10));
}

SCM hashv(obj, n)
     SCM obj;
     SCM n;
{
  ASRTER(INUMP(n) && 0 <= n, n, ARG2, s_hashv);
  if (ICHRP(obj)) return MAKINUM((unsigned)(downcase[ICHR(obj)]) % INUM(n));
  if (NIMP(obj) && NUMP(obj)) return MAKINUM(hasher(obj, INUM(n), 10));
  else return MAKINUM(obj % INUM(n));
}

SCM hashq(obj, n)
     SCM obj;
     SCM n;
{
  ASRTER(INUMP(n) && 0 <= n, n, ARG2, s_hashq);
  return MAKINUM((((unsigned) obj) >> 1) % INUM(n));
}

static iproc subr1s[] = {
	{"number?", numberp},
	{s_inexactp, inexactp},
#ifdef FLOATS
	{"complex?", scm_complex_p},
	{"real?", realp},
	{"rational?", scm_rationalp},
	{"integer?", intp},
	{s_real_part, real_part},
	{s_imag_part, imag_part},
	{s_magnitude, scm_magnitude},
	{s_angle, angle},
	{s_in2ex, in2ex},
	{s_ex2in, ex2in},
	{s_abs, scm_abs},
# ifdef BIGDIG
	{s_dfloat_parts, scm_dfloat_parts},
# endif
#else
	{"complex?", numberp},
	{"real?", numberp},
	{"rational?", numberp},
	{"integer?", exactp},
	{"floor", numident},
	{"ceiling", numident},
	{s_trunc, numident},
	{"round", numident},
	{s_abs, scm_iabs},
#endif
	{s_zerop, zerop},
	{s_positivep, positivep},
	{s_negativep, negativep},
	{s_str2list, string2list},
	{"list->string", string},
	{s_st_copy, string_copy},
	{"list->vector", vector},
	{s_vect2list, vector2list},
	{0, 0}};

static iproc asubrs[] = {
	{s_difference, difference},
	{s_divide, divide},
	{s_max, scm_max},
	{s_min, scm_min},
	{s_sum, sum},
	{s_product, product},
	{0, 0}};

static iproc subr2s[] = {
#ifdef FLOATS
	{s_makrect, makrect},
	{s_makpolar, makpolar},
	{s_atan2, latan2},
	{s_expt, expt},
# ifdef BIGDIG
	{s_make_dfloat, scm_make_dfloat},
# endif
#endif
#ifdef INUMS_ONLY
	{s_memv, memq},
	{s_assv, assq},
#else
	{s_memv, memv},
	{s_assv, assv},
#endif
	{s_intexpt, scm_intexpt},
	{s_intlog, scm_intlog},
	{s_cintlog, scm_cintlog},
	{s_list_tail, list_tail},
	{s_ve_fill, vector_fill},
	{s_st_fill, string_fill},
	{s_hash, hash},
	{s_hashv, hashv},
	{s_hashq, hashq},
	{0, 0}};

static iproc subr2os[] = {
	{s_str2number, string2number},
	{s_number2string, number2string},
	{0, 0}};

static iproc rpsubrs[] = {
#ifdef INUMS_ONLY
	{"eqv?", eq},
#else
	{"eqv?", eqv},
#endif
	{s_eqp, eqp},
	{s_lessp, lessp},
	{s_grp, greaterp},
	{s_leqp, leqp},
	{s_greqp, greqp},
	{0, 0}};

#ifdef FLOATS
static dblproc cxrs[] = {
	{"floor", floor},
	{"ceiling", ceil},
	{"truncate", scm_truncate},
	{"round", scm_round},
	{"$abs", fabs},
	{"real-sqrt", sqrt},
	{"real-exp", exp},
	{"real-ln", log},
	{"real-log10", log10},
	{"real-sin", sin},
	{"real-cos", cos},
	{"real-tan", tan},
	{"real-asin", asin},
	{"real-acos", acos},
	{"real-atan", atan},
	{"real-sinh", sinh},
	{"real-cosh", cosh},
	{"real-tanh", tanh},
	{"real-asinh", asinh},
	{"real-acosh", acosh},
	{"real-atanh", atanh},
	{0, 0}};
#endif

#ifdef FLOATS
static void safe_add_1(f, fsum)
     double f, *fsum;
{
  *fsum = f + 1.0;
}
#endif

void init_scl()
{
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2os, tc7_subr_2o);
  init_iprocs(subr2s, tc7_subr_2);
  init_iprocs(asubrs, tc7_asubr);
  init_iprocs(rpsubrs, tc7_rpsubr);
#ifdef SICP
  add_feature("sicp");
#endif
#ifdef FLOATS
  init_iprocs((iproc *)cxrs, tc7_cxr);
# ifdef SINGLES
  NEWCELL(flo0);
  CAR(flo0) = tc_flo;
  FLO(flo0) = 0.0;
# else
  DEFER_INTS;
  flo0 = must_malloc_cell(1L*sizeof(double), (SCM)tc_dblr, "real");
  REAL(flo0) = 0.0;
  ALLOW_INTS;
# endif
# ifndef _MSC_VER
  DEFER_INTS;
  scm_narn = must_malloc_cell(2L*sizeof(double), (SCM)tc_dblc, "complex");
  REAL(scm_narn) = 0.0/0.0;
  IMAG(scm_narn) = 0.0/0.0;
  ALLOW_INTS;
# endif
# ifndef BIGDIG
#  ifdef DBL_DIG
  dblprec = (DBL_DIG > 20) ? 20 : DBL_DIG;
#  else
  {				/* determine floating point precision */
    double f = 0.1;
    volatile double fsum = 1.0+f;
    while (fsum != 1.0) {
      f /= 10.0;
      if (++dblprec > 20) break;
      safe_add_1(f, &fsum);
    }
    dblprec = dblprec-1;
  }
#  endif /* DBL_DIG */
# else	 /* !BIGDIG */
  {
    int idx;
    dpows5[0] = 1.0;
    for (idx = 1; idx < 23; idx++) {
      dpows5[idx] = 5*dpows5[idx-1];
    }
  }
# endif	/* !BIGDIG */
# ifdef DBL_MANT_DIG
  dbl_mant_dig = DBL_MANT_DIG;
# else
  {				/* means we #defined it. */
    volatile double fsum = 0.0;
    double eps = 1.0;
    int i = 0;
    while (fsum != 1.0) {
      eps = 0.5 * eps;
      safe_add_1(eps, &fsum);
      i++;
    }
    dbl_mant_dig = i;
  }
# endif /* DBL_MANT_DIG */
  max_dbl_int = pow(2.0, dbl_mant_dig - 1.0);
  max_dbl_int = max_dbl_int + (max_dbl_int - 1.0);
  /* dbl_eps = ldexp(1.0, - dbl_mant_dig); */
  sysintern("double-float-mantissa-length", MAKINUM(dbl_mant_dig));
  sysintern("bigdbl:powers-of-5",
	    pows5 = make_vector(MAKINUM(326), MAKINUM(1)));
#endif
}
