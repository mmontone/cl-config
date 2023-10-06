/* "differ.c" Linear-space O(NP) sequence comparison.
 * Copyright (C) 2003, 2004 Free Software Foundation, Inc.
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

#include <stdlib.h>
/* #include <stdio.h> */

#include "scm.h"

#ifdef __x86_64
# define I32 int
#else
# define I32 long
#endif
/* Currently A:fixZ32b are actually A:fixZ64b.  Remove next line when
   this gets fixed. */
#define I32 long

SCM_EXPORT SCM  array_dims P((SCM ra));

typedef int (*int_function) ();

typedef struct {
  void* (*subarray) ();
  int_function array_refsEql_P;
  int_function array_refs_revEql_P;
} fp_procs;

int fp_compare(I32 *fp,int fpoff,I32 *cc,void *a,int m,void *b,int n,int_function array_refsEql_P,int p_lim);

int fp_run(I32 *fp,int fpoff,int k,void *a,int m,void *b,int n,int_function array_refsEql_P,I32 *cc,int p);

int diff_mid_split(int n,I32 *rr,I32 *cc,int cost);

void fp_init(I32 *fp,int fpoff,int fill,int mindx,int maxdx);

int diff_divide_and_conquer(I32 *fp,int fpoff,I32 *ccrr,void *a,int start_a,int end_a,void *b,int start_b,int end_b,I32 *edits,int edx,int epo,fp_procs *procs,int p_lim);

int diff2et(I32 *fp,int fpoff,I32 *ccrr,void *a,int start_a,int end_a,void *b,int start_b,int end_b,I32 *edits,int edx,int epo,fp_procs *procs,int p_lim);

int diff2ez(I32 *fp,int fpoff,I32 *ccrr,void *a,int start_a,int end_a,void *b,int start_b,int end_b,I32 *edits,int edx,int epo,fp_procs *procs,int p_lim);

void check_cost(unsigned char *name,int est,int cost);

SCM_EXPORT SCM  diff2edits P((SCM Edits, SCM Fp, SCM Args));

SCM_EXPORT SCM  diff2editlen P((SCM Fp, SCM A, SCM Args));

#define MAX(a,b)		(a<b ? b : a)
#define MIN(a,b)		(a>b ? b : a)

I32 *long_subarray(ra, start, end)
     I32 *ra; int start, end;
{
  return &(ra[start]);
}
short *short_subarray(ra, start, end)
     short *ra; int start, end;
{
  return &(ra[start]);
}
char *char_subarray(ra, start, end)
     char *ra; int start, end;
{
  return &(ra[start]);
}

int long_array_refsEql_P(a, x, m, b, y, n)
     I32 *a; int x, m; I32 *b; int y, n;
{
  return (a[x])==(b[y]);
}
int long_array_refs_revEql_P(a, x, m, b, y, n)
     I32 *a; int x, m; I32 *b; int y, n;
{
/*   if (x > m) printf("long x(%d) > m(%d)\n", x, m); */
/*   if (y > n) printf("long y(%d) > n(%d)\n", y, n); */
  return a[(m)-(x)-1]==b[(n)-(y)-1];
}
int short_array_refsEql_P(a, x, m, b, y, n)
     short *a; int x, m; short *b; int y, n;
{
  return (a[x])==(b[y]);
}
int short_array_refs_revEql_P(a, x, m, b, y, n)
     short *a; int x, m; short *b; int y, n;
{
/*   if (x > m) printf("short x(%d) > m(%d)\n", x, m); */
/*   if (y > n) printf("short y(%d) > n(%d)\n", y, n); */
  return a[(m)-(x)-1]==b[(n)-(y)-1];
}
int char_array_refsEql_P(a, x, m, b, y, n)
     char *a; int x, m; char *b; int y, n;
{
  return (a[x])==(b[y]);
}
int char_array_refs_revEql_P(a, x, m, b, y, n)
     char *a; int x, m; char *b; int y, n;
{
/*   if (x > m) printf("char x(%d) > m(%d)\n", x, m); */
/*   if (y > n) printf("char y(%d) > n(%d)\n", y, n); */
  return a[(m)-(x)-1]==b[(n)-(y)-1];
}

fp_procs long_procs =
  {long_subarray,
   long_array_refsEql_P,
   long_array_refs_revEql_P};
fp_procs short_procs =
  {short_subarray,
  short_array_refsEql_P,
  short_array_refs_revEql_P};
fp_procs char_procs =
  {char_subarray,
  char_array_refsEql_P,
  char_array_refs_revEql_P};

int fp_compare(fp, fpoff, cc, a, m, b, n, array_refsEql_P, p_lim)
     I32 *fp;
     int fpoff;
     I32 *cc;
     void *a;
     int m;
     void *b;
     int n;
     int_function array_refsEql_P;
     int p_lim;
{
  int delta = (n)-(m);
  {
    int p = 0;
L_loop:
    {
      int k = -(p);
      while (!((k)>=(delta))) {
	fp_run(fp, fpoff, k, a, m, b, n, array_refsEql_P, cc, p);
	{
	  k = 1+(k);
	}
      }
    }
    {
      int k = (delta)+(p);
      while (!((k)<=(delta))) {
	fp_run(fp, fpoff, k, a, m, b, n, array_refsEql_P, cc, p);
	{
	  k =  -1+(k);
	}
      }
    }
    {
      int fpval = fp_run(fp, fpoff, delta, a, m, b, n, array_refsEql_P, cc, p);
      if ((!(cc))
	  && ((n)<=(fpval)))
	return (delta)+(2*(p));
      else if ((!(0 > (p_lim)))
	  && ((p)>=(p_lim)))
	return -1;
      else {
	p = 1+(p);
	goto L_loop;
      }
    }
  }
}

/*  Traces runs of matches until they end; then set fp[k]=y. */
/*  If CC is supplied, set each CC[y] = MIN(CC[y], cost) for run. */
/*  Returns furthest y reached. */

int fp_run(fp, fpoff, k, a, m, b, n, array_refsEql_P, cc, p)
     I32 *fp;
     int fpoff;
     int k;
     void *a;
     int m;
     void *b;
     int n;
     int_function array_refsEql_P;
     I32 *cc;
     int p;
{
  int cost = (k)+(p)+(p);
  {
    int y = MAX((fp[ -1+(k)+(fpoff)])+1, fp[1+(k)+(fpoff)]);
L_snloop:
    {
      int x = (y)-(k);
      if ((cc)
	  && ((y)<=(n)))
	{
	  int xcst = (m)-(x);
	  if (0 > (xcst))
	    ;
	  else cc[y] = MIN((xcst)+(cost), cc[y]);
	}
      if (((x)<(m))
	  && ((y)<(n))
	  && (array_refsEql_P(a, x, m, b, y, n)))
	{
	  y = 1+(y);
	  goto L_snloop;
	}
      else {
	fp[(fpoff)+(k)] = y;
	return y;
      }
    }
  }
}

int diff_mid_split(n, rr, cc, cost)
     int n;
     I32 *rr;
     I32 *cc;
     int cost;
{
  {
    int cdx = 1+((n)/2);
    int rdx = (n)/2;
L_loop:
    if ((cost)==((cc[rdx])+(rr[(n)-(rdx)])))
      return rdx;
    else if ((cost)==((cc[cdx])+(rr[(n)-(cdx)])))
      return cdx;
    else {
      cdx = 1+(cdx);
      rdx =  -1+(rdx);
      goto L_loop;
    }
  }
}


void fp_init(fp, fpoff, fill, mindx, maxdx)
     I32 *fp;
     int fpoff;
     int fill;
     int mindx;
     int maxdx;
{
  int mlim = (fpoff)+(mindx);
  {
    int idx = (fpoff)+(maxdx);
    while (!((idx)<(mlim))) {
      fp[idx] = fill;
      {
	idx =  -1+(idx);
      }
    }
  }
}

/*  Split A[start-a..end-a] (shorter array) into smaller and smaller chunks. */
/*  EDX is index into EDITS. */
/*  EPO is insert/delete polarity (+1 or -1) */

int diff_divide_and_conquer(fp, fpoff, ccrr, a, start_a, end_a, b, start_b, end_b, edits, edx, epo, procs, p_lim)
     I32 *fp;
     int fpoff;
     I32 *ccrr;
     void *a;
     int start_a;
     int end_a;
     void *b;
     int start_b;
     int end_b;
     I32 *edits;
     int edx;
     int epo;
     fp_procs *procs;
     int p_lim;
{
  int mid_a = ((start_a)+(end_a))/2;
  int len_b = (end_b)-(start_b);
  int len_a = (end_a)-(start_a);
  {
    int tcst = (p_lim)+(p_lim)+((len_b)-(len_a));
    I32 *cc = &(ccrr[0]);
    I32 *rr = &(ccrr[(len_b)+1]);
    int m2 = (end_a)-(mid_a);
    int m1 = (mid_a)-(start_a);
    fp_init(cc, 0, (len_a)+(len_b), 0, len_b);
    fp_init(fp, fpoff,  -1, -(1+(p_lim)), 1+(p_lim)+((len_b)-(m1)));
    fp_compare(fp, fpoff, cc, procs->subarray(a, start_a, mid_a), m1, procs->subarray(b, start_b, end_b), len_b, procs->array_refsEql_P, MIN(p_lim, len_a));
    fp_init(rr, 0, (len_a)+(len_b), 0, len_b);
    fp_init(fp, fpoff,  -1, -(1+(p_lim)), 1+(p_lim)+((len_b)-(m2)));
    fp_compare(fp, fpoff, rr, procs->subarray(a, mid_a, end_a), m2, procs->subarray(b, start_b, end_b), len_b, procs->array_refs_revEql_P, MIN(p_lim, len_a));
    {
      int b_splt = diff_mid_split(len_b, rr, cc, tcst);
      I32 est_c = cc[b_splt];
      I32 est_r = rr[(len_b)-(b_splt)];
      check_cost("cc", est_c, diff2et(fp, fpoff, ccrr, a, start_a, mid_a, b, start_b, (start_b)+(b_splt), edits, edx, epo, procs, ((est_c)-((b_splt)-((mid_a)-(start_a))))/2));
      check_cost("rr", est_r, diff2et(fp, fpoff, ccrr, a, mid_a, end_a, b, (start_b)+(b_splt), end_b, edits, (est_c)+(edx), epo, procs, ((est_r)-(((len_b)-(b_splt))-((end_a)-(mid_a))))/2));
      return (est_c)+(est_r);
    }
  }
}

/*  Trim; then diff sub-arrays; either one longer.  Returns edit-length */

int diff2et(fp, fpoff, ccrr, a, start_a, end_a, b, start_b, end_b, edits, edx, epo, procs, p_lim)
     I32 *fp;
     int fpoff;
     I32 *ccrr;
     void *a;
     int start_a;
     int end_a;
     void *b;
     int start_b;
     int end_b;
     I32 *edits;
     int edx;
     int epo;
     fp_procs *procs;
     int p_lim;
{
  {
    int bdx =  -1+(end_b);
    int adx =  -1+(end_a);
    while (((start_b)<=(bdx))
	 && ((start_a)<=(adx))
	 && (procs->array_refsEql_P(a, adx, 0, b, bdx, 0))) {
      {
	bdx =  -1+(bdx);
	adx =  -1+(adx);
      }
    }
    {
      int bsx = start_b;
      int asx = start_a;
      while (((bsx)<(bdx))
	   && ((asx)<(adx))
	   && (procs->array_refsEql_P(a, asx, 0, b, bsx, 0))) {
	{
	  bsx = 1+(bsx);
	  asx = 1+(asx);
	}
      }
      {
	int delta = ((bdx)-(bsx))-((adx)-(asx));
	if (0 > (delta))
	  return diff2ez(fp, fpoff, ccrr, b, bsx, 1+(bdx), a, asx, 1+(adx), edits, edx, -(epo), procs, (delta)+(p_lim));
	else return diff2ez(fp, fpoff, ccrr, a, asx, 1+(adx), b, bsx, 1+(bdx), edits, edx, epo, procs, p_lim);
      }
    }
  }
}

/*  Diff sub-arrays, A not longer than B.  Returns edit-length */

int diff2ez(fp, fpoff, ccrr, a, start_a, end_a, b, start_b, end_b, edits, edx, epo, procs, p_lim)
     I32 *fp;
     int fpoff;
     I32 *ccrr;
     void *a;
     int start_a;
     int end_a;
     void *b;
     int start_b;
     int end_b;
     I32 *edits;
     int edx;
     int epo;
     fp_procs *procs;
     int p_lim;
{
  int len_a = (end_a)-(start_a);
  int len_b = (end_b)-(start_b);
  if (!(p_lim))
    if ((len_b)==(len_a))
      return 0;
    else {
      int T_edx = edx;
      int adx = start_a;
      int bdx = start_b;
      int edx = T_edx;
L_loop:
      if ((bdx)>=(end_b))
	return (len_b)-(len_a);
      else if ((adx)>=(end_a))
	{
	  int T_edx = edx;
	  int idx = bdx;
	  int edx = T_edx;
	  while (!((idx)>=(end_b))) {
	    edits[edx] = (epo)*(1+(idx));
	    {
	      idx = 1+(idx);
	      edx = 1+(edx);
	    }
	  }
	  return (len_b)-(len_a);
	}
      else if (procs->array_refsEql_P(a, adx, 0, b, bdx, 0))
	{
	  adx = 1+(adx);
	  bdx = 1+(bdx);
	  goto L_loop;
	}
      else {
	edits[edx] = (epo)*(1+(bdx));
	{
	  bdx = 1+(bdx);
	  edx = 1+(edx);
	  goto L_loop;
	}
      }
    }
  else if ((len_a)<=(p_lim))
    {
      int idx = start_a;
      int jdx = start_b;
      while (!(((idx)>=(end_a))
	     && ((jdx)>=(end_b)))) {
	if ((jdx)<(end_b))
	  {
	    edits[edx] = (epo)*(1+(jdx));
	    edx = 1+(edx);
	  }
	if ((idx)<(end_a))
	  {
	    edits[edx] = (epo)*( -1-(idx));
	    edx = 1+(edx);
	  }
	{
	  idx = 1+(idx);
	  jdx = 1+(jdx);
	}
      }
      return (len_a)+(len_b);
    }
  else return diff_divide_and_conquer(fp, fpoff, ccrr, a, start_a, end_a, b, start_b, end_b, edits, edx, epo, procs, p_lim);
}

void check_cost(name, est, cost)
     unsigned char *name;
     int est;
     int cost;
{
  if ((est)!=(cost)) {
/*     fprintf(stderr, "%s: cost check failed %d != %d\\n", name, est, cost); */
    wta(MAKINUM(cost), "cost check failed", name);
  }
}

/*  Routines interfacing API layer to algorithms. */

/* Return the fp_procs appropriate for SCM array prototype */
fp_procs *raprot2procs(prot, s_name)
     SCM prot;
     char *s_name;
{
  fp_procs *procs;
  if (ICHRP(prot)) procs = &char_procs;
  else if (MAKINUM(16L)==prot) procs = &short_procs;
  else if (MAKINUM(-16L)==prot) procs = &short_procs;
  else if (MAKINUM(32L)==prot) procs = &long_procs;
  else if (MAKINUM(-32L)==prot) procs = &long_procs;
  else if (EOL==prot) procs = &long_procs;
  else wta(prot, (char *)ARG3, s_name);
  return procs;
}

static SCM list_of_0;

void* array2addr(RA, prot, pos, s_name)
     SCM RA, prot;
     char *pos;
     char s_name[];
{
  ASRTER(BOOL_T==arrayp(RA, UNDEFINED) && array_prot(RA)==prot, RA,
	 pos, s_name);
  return scm_addr(cons(RA, list_of_0), s_name);
}

/*  A not longer than B (M <= N) */
static char s_d2es[] = "diff2edits!";
static char s_incomp[] = "incompatible array types";
SCM diff2edits(Edits, Fp, Args)
     SCM Edits, Fp, Args;	/* Ccrr, A, B; */
{
  SCM aprot, bprot;
  I32 *edits;
  int est;
  I32 *fp;
  I32 *ccrr;
  void *a, *b;
  int m, n;
  fp_procs *procs;
  ASRTER(3==ilength(Args), Args, WNA, s_d2es);
  edits = array2addr(Edits, MAKINUM(-32), ARG1, s_d2es);
  fp = array2addr(Fp, MAKINUM(-32), ARG2, s_d2es);
  ccrr = array2addr(CAR(Args), MAKINUM(-32), ARG3, s_d2es);
  Args = CDR(Args);
  aprot = array_prot(CAR(Args));
  a = array2addr(CAR(Args), aprot, ARG4, s_d2es);
  ASRTER(NFALSEP(aprot), aprot, ARG4, s_d2es);
  m = INUM(CAR(array_dims(CAR(Args))));
  Args = CDR(Args);
  bprot = array_prot(CAR(Args));
  b = array2addr(CAR(Args), bprot, ARG5, s_d2es);
  ASRTER(NFALSEP(bprot), bprot, ARG5, s_d2es);
  n = INUM(CAR(array_dims(CAR(Args))));
  ASRTER(aprot==bprot, bprot, s_incomp, s_d2es);
  procs = raprot2procs(aprot, s_d2es);
  est = INUM(CAR(array_dims(Edits)));
  {
    int p_lim = ((est)-((n)-(m)))/2;
    check_cost(s_d2es, est,
	       diff2et(fp, 1+(p_lim),
		       ccrr, a, 0, m, b, 0, n, edits, 0, 1, procs, p_lim));
    return UNSPECIFIED;
  }
}

/*  A not longer than B (M <= N) */

static char s_d2el[] = "diff2editlen";
SCM diff2editlen(Fp, A, Args)
     SCM Fp, A, Args;		/* B, P_lim */
{
  SCM aprot, bprot;
  fp_procs *procs;
  int p_lim;
  int m, n;
  I32 *fp;
  void *a, *b;
  ASRTER(2==ilength(Args), Args, WNA, s_d2el);
  fp = array2addr(Fp, MAKINUM(-32), ARG1, s_d2el);
  aprot = array_prot(A);
  a = array2addr(A, aprot, ARG2, s_d2el);
  ASRTER(NFALSEP(aprot), aprot, ARG2, s_d2el);
  m = INUM(CAR(array_dims(A)));
  bprot = array_prot(CAR(Args));
  b = array2addr(CAR(Args), bprot, ARG3, s_d2el);
  ASRTER(NFALSEP(bprot), bprot, ARG3, s_d2el);
  n = INUM(CAR(array_dims(CAR(Args))));
  Args = CDR(Args);
  ASRTER(INUMP(CAR(Args)), CAR(Args), ARG4, s_d2el);
  p_lim = INUM(CAR(Args));
  ASRTER(aprot==bprot, bprot, s_incomp, s_d2el);
  procs = raprot2procs(aprot, s_d2el);
  {
    int maxdx = 0 > (p_lim)
      ?1+(n)
      :1+(p_lim)+((n)-(m));
    int mindx = 0 > (p_lim)
      ?-(1+(m))
      :-(1+(p_lim));
    int res;
    fp_init(fp, -(mindx), -1, mindx, maxdx);
    res = fp_compare(fp, -(mindx), 0, a, m, b, n,
		     procs->array_refsEql_P, p_lim);
    return (-1==res) ? BOOL_F : MAKINUM(res);
  }
}

static char s_Idiffer[] = "Idiffer.scm";
void init_differ()
{
  list_of_0 = cons(INUM0, EOL);
  scm_gc_protect(list_of_0);
  make_subr(s_d2es, tc7_lsubr_2, diff2edits);
  make_subr(s_d2el, tc7_lsubr_2, diff2editlen);
  if (scm_ldprog(s_Idiffer))
    wta(*loc_errobj, "couldn't init", s_Idiffer);
}
