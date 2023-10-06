/*  "bytenumb.scm" Byte integer and IEEE floating-point conversions.
 *  Copyright (C) 2007 Free Software Foundation, Inc.
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

/* For documentation see:
   http://cvs.savannah.gnu.org/viewcvs/slib/slib/bytenumb.scm?view=markup */

#include <stdlib.h>
#include <math.h>

#include "scm.h"

int get_bytes_length(obj)
     SCM obj;
{
  array_dim *s;
  if (IMP(obj)) return -1;
  switch (TYP7(obj)) {
  case tc7_string:
  case tc7_VfixN8:
  case tc7_VfixZ8:
    return LENGTH(obj);
  case tc7_smob:
    if (!ARRAYP(obj)) return -1;
    if (1 != ARRAY_NDIM(obj)) return -1;
    s = ARRAY_DIMS(obj);
    if (1 != s[0].inc) return -1;
    return s[0].ubnd - s[0].lbnd;
  default: return -1;
  }
}

static char s_wrong_length[] = "wrong length";
static SCM list_of_0;

char * get_bytes(obj, minlen, s_name)
     SCM obj;
     int minlen;
     const char *s_name;
{
  ASRTER(NIMP(obj) && (TYP7(obj)==tc7_string ||
		       TYP7(obj)==tc7_VfixN8 ||
		       TYP7(obj)==tc7_VfixZ8),
	 obj, ARG1, s_name);
  {
    int byvlen = get_bytes_length(obj);
    ASRTER((minlen < 0) ? byvlen >= -minlen : byvlen == minlen,
	   MAKINUM(byvlen), s_wrong_length, s_name);
    return (char*)scm_addr(cons(obj, list_of_0), s_name);
  }
}

static char s_bytes_to_integer[] = "bytes->integer";
SCM scm_bytes_to_integer(sbyts, sn)
     SCM sbyts;
     SCM sn;
{
  long n = INUM(sn);
  if (!(n)) return INUM0;
  {
    int cnt = abs(n);
    char *byts = get_bytes(sbyts, -cnt, s_bytes_to_integer);
    int iu = 0, id = cnt - sizeof(BIGDIG);
    sizet ndigs = (cnt + sizeof(BIGDIG) - 1) / sizeof(BIGDIG);
    int negp = (0x80 & byts[0]) && (0 > n);
    SCM retval = mkbig(ndigs, negp);
    BIGDIG *digs = BDIGITS(retval), carry = 1;
    if (negp)
      for (; iu < ndigs; iu++) {
	int j = 0;
	UBIGLONG dig = 0;
	for (; j < sizeof(BIGDIG); j++) {
	  dig = (dig<<8) +
	    (0xFF ^ ((id + j >= 0) ? (((unsigned char *)byts)[id + j]) : 255));
	  /* printf("byts[%d + %d] = %lx\n", id, j, 0xFF & dig); */
	}
	dig = dig + carry;
	digs[iu] = dig;
	carry = dig >> (8 * sizeof(BIGDIG));
	/* printf("id = %d; iu = %d; dig = %04lx\n", id, iu, dig); */
	id = id - sizeof(BIGDIG);
      } else
      for (; iu < ndigs; iu++) {
	int j = 0;
	BIGDIG dig = 0;
	for (; j < sizeof(BIGDIG); j++) {
	  dig = (dig<<8) +
	    ((id + j >= 0) ? (((unsigned char *)byts)[id + j]) : 0);
	}
	digs[iu] = dig;
	/* printf("id = %d; iu = %d; dig = %04x\n", id, iu, dig); */
	id = id - sizeof(BIGDIG);
      }
    return normbig(retval);
  }
}

static char s_integer_to_bytes[] = "integer->bytes";
SCM scm_integer_to_bytes(sn, slen)
     SCM sn;
     SCM slen;
{
  ASRTER(INUMP(slen), slen, ARG2, s_integer_to_bytes);
  {
    int len = INUM(slen);
    SCM sbyts = make_string(scm_iabs(slen), MAKICHR(0));
    char *byts = CHARS(sbyts);
    if (INUMP(sn)) {
      int idx = -1 + (abs(len));
      long n = num2long(sn, (char *)ARG1, s_integer_to_bytes);
      if ((0 > n) && (0 > len)) {
	long res = -1 - n;
	while (!(0 > idx)) {
	  byts[idx--] = 0xFF ^ (res % 0x100);
	  res = res>>8;
	}
      }
      else {
	UBIGLONG res = n;
	while (!(0 > idx)) {
	  byts[idx--] = res % 0x100;
	  res = res>>8;
	}
      }
    } else {
      ASRTER(NIMP(sn) && BIGP(sn), sn, ARG1, s_integer_to_bytes);
      {
	BIGDIG *digs = BDIGITS(sn), borrow = 1;
	sizet ndigs = NUMDIGS(sn);
	int iu = 0, id = abs(len) - 1;
	UBIGLONG dig;
	if ((0 > len) && (TYP16(sn)==tc16_bigneg))
	  for (; 0 <= id ; iu++) {
	    sizet j = sizeof(BIGDIG);
	    dig = (iu < ndigs) ? digs[iu] : 0;
	    dig = dig ^ ((1 << (8 * sizeof(BIGDIG))) - 1);
	    /* printf("j = %d; id = %d; iu = %d; dig = %04x; borrow = %d\n", j, id, iu, dig, borrow); */
	    for (; 0 < j-- && 0 <= id;) {
	      /* printf("byts[%d] = %02x\n", id, 0xFF & dig); */
	      int dg = (0xFF & dig) + borrow;
	      borrow = dg >> 8;
	      ((unsigned char *)byts)[id--] = dg;
	      dig = (dig)>>8;
	    }
	  }
	else
	  for (; 0 <= id ; iu++) {
	    BIGDIG dig = (iu < ndigs) ? digs[iu] : 0;
	    sizet j = sizeof(BIGDIG);
	    /* printf("j = %d; id = %d; iu = %d; dig = %04x\n", j, id, iu, dig); */
	    for (; 0 < j-- && 0 <= id;) {
	      /* printf("byts[%d] = %02x\n", id, 0xFF & dig); */
	      ((unsigned char *)byts)[id--] = 0xFF & dig;
	      dig = (dig>>8);
	    }
	  }
      }
    }
    return sbyts;
  }
}

static char s_bytes_to_ieee_float[] = "bytes->ieee-float";
SCM scm_bytes_to_ieee_float(sbyts)
     SCM sbyts;
{
  char *byts = get_bytes(sbyts, 4, s_bytes_to_ieee_float);
  int len = LENGTH(sbyts);
  int s = (1<<(7)) & ((((unsigned char*)(byts))[0]));
  int e = ((0x7f&((((unsigned char*)(byts))[0])))<<1)
    + ((0x80&((((unsigned char*)(byts))[1])))>>7);
  float f = (((unsigned char*)(byts))[ -1 + (len)]);
  int idx = -2 + (len);
  while (!((idx)<=1)) {
    {
      int T_idx = -1 + (idx);
      f = ((((unsigned char*)(byts))[idx])) + ((f) / 0x100);
      idx = T_idx;
    }
  }
  f = ((0x7f&((((unsigned char*)(byts))[1]))) + ((f) / 0x100)) / 0x80;
  if ((0<(e))
      && ((e)<0xff))
    return makdbl(ldexpf((s ? -1 : 1) * (1 + (f)), (e) - 0x7f), 0.0);
  else if (!(e))
    if (!(f)) return flo0;
    else return makdbl(ldexpf((s ? -1 : 1) * (f), -126), 0.0);
  else if (f)
    return scm_narn;
  else return makdbl((s ? -(1.0) : 1.0) / 0.0, 0.0);
}

static char s_bytes_to_ieee_double[] = "bytes->ieee-double";
SCM scm_bytes_to_ieee_double(sbyts)
     SCM sbyts;
{
  char *byts = get_bytes(sbyts, 8, s_bytes_to_ieee_double);
  int len = LENGTH(sbyts);
  int s = (1<<(7)) & ((((unsigned char*)(byts))[0]));
  int e = ((0x7f&((((unsigned char*)(byts))[0])))<<4)
    + ((0xf0&((((unsigned char*)(byts))[1])))>>4);
  double f = (((unsigned char*)(byts))[ -1 + (len)]);
  int idx = -2 + (len);
  while (!((idx)<=1)) {
    {
      int T_idx = -1 + (idx);
      f = ((((unsigned char*)(byts))[idx])) + ((f) / 0x100);
      idx = T_idx;
    }
  }
  f = ((0xf&((((unsigned char*)(byts))[1]))) + ((f) / 0x100)) / 0x10;
  if ((0<(e))
      && ((e)<0x7ff))
    return makdbl(ldexp((s ? -1 : 1) * (1 + (f)), (e) - 0x3ff), 0.0);
  else if (!(e))
    if (!(f)) return flo0;
    else return makdbl(ldexp((s ? -1 : 1) * (f), -1022), 0.0);
  else if (f)
    return scm_narn;
  else return makdbl((s ? -(1.0) : 1.0) / 0.0, 0.0);
}

static char s_ieee_float_to_bytes[] = "ieee-float->bytes";
SCM scm_ieee_float_to_bytes(in_flt)
     SCM in_flt;
{
  double dbl = num2dbl(in_flt, (char *)ARG1, s_ieee_float_to_bytes);
  float flt = (float) dbl;
  SCM sbyts = make_string(MAKINUM(4), MAKICHR(0));
  char *byts = CHARS(sbyts);
  int s = flt < 0.0;
  int scl = 0x7f;
  flt = fabs(flt);
  if (0.0==flt) {
    if (s)
      byts[0] = 0x80;
    return sbyts;
  }
  else if (flt != flt) {
    byts[0] = 0x7f;
    byts[1] = 0xc0;
    return sbyts;
  }
  else goto L_scale;
 L_out:
  {
    float T_flt = 0x80 * (flt);
    int val = (int)(floor(0x80 * (flt)));
    int idx = 1;
    float flt = T_flt;
    while (!((idx) > 3)) {
      byts[idx] = val;
      {
	float T_flt = 0x100 * ((flt) - (val));
	int T_val = (int)(floor(0x100 * ((flt) - (val))));
	idx = 1 + (idx);
	flt = T_flt;
	val = T_val;
      }
    }
    byts[1] = (0x80 & (scl<<7)) | (0x7f & (((unsigned char*)(byts))[1]));
    byts[0] = (s ? 0x80 : 0) + ((scl)>>1);
    return sbyts;
  }
 L_scale:
  if (!(scl)) {
    flt = (flt)/2;
    goto L_out;
  }
  else if ((flt)>=0x10) {
    float flt16 = (flt) / 0x10;
    if ((flt16)==(flt)) {
      byts[0] = s ? 0xff : 0x7f;
      byts[1] = 0x80;
      return sbyts;
    }
    else {
      flt = flt16;
      scl = (scl) + 4;
      goto L_scale;
    }
  }
  else if ((flt) >= 2) {
    flt = (flt) / 2;
    scl = (scl) + 1;
    goto L_scale;
  }
  else if (((scl) >= 4) && ((0x10 * (flt))<1)) {
    flt = (flt) * 0x10;
    scl = (scl)+ -4;
    goto L_scale;
  }
  else if ((flt)<1) {
    flt = (flt) * 2;
    scl = (scl) + -1;
    goto L_scale;
  }
  else {
    flt = -1+(flt);
    goto L_out;
  }
}

static char s_ieee_double_to_bytes[] = "ieee-double->bytes";
SCM scm_ieee_double_to_bytes(in_flt)
     SCM in_flt;
{
  double flt = num2dbl(in_flt, (char *)ARG1, s_ieee_double_to_bytes);
  SCM sbyts = make_string(MAKINUM(8), MAKICHR(0));
  char *byts = CHARS(sbyts);
  int s = flt < 0.0;
  int scl = 0x3ff;
  flt = fabs(flt);
  if (0.0==flt) {
    if (s)
      byts[0] = 0x80;
    return sbyts;
  }
  else if (flt != flt) {
    byts[0] = 0x7f;
    byts[1] = 0xf8;
    return sbyts;
  }
  else goto L_scale;
 L_out:
  {
    double T_flt = 0x10 * (flt);
    int val = (int)(floor(0x10 * (flt)));
    int idx = 1;
    double flt = T_flt;
    while (!((idx) > 7)) {
      byts[idx] = val;
      {
	double T_flt = 0x100 * (flt - val);
	int T_val = (int)floor(0x100 * (flt - val));
	idx = 1 + (idx);
	flt = T_flt;
	val = T_val;
      }
    }
    byts[1] = (0xf0 & (scl<<4)) | (0x0f & (((unsigned char*)(byts))[1]));
    byts[0] = (s ? 0x80 : 0) + ((scl)>>4);
    return sbyts;
  }
 L_scale:
  if (!(scl)) {
    flt = (flt) / 2;
    goto L_out;
  }
  else if ((flt) >= 0x10) {
    double flt16 = (flt) / 0x10;
    if ((flt16)==(flt)) {
      byts[0] = s ? 0xff : 0x7f;
      byts[1] = 0xf0;
      return sbyts;
    }
    else {
      flt = flt16;
      scl = (scl) + 4;
      goto L_scale;
    }
  }
  else if ((flt) >= 2) {
    flt = (flt) / 2;
    scl = (scl) + 1;
    goto L_scale;
  }
  else if (((scl) >= 4) && ((0x10 * flt) < 1)) {
    flt = (flt) * 0x10;
    scl = (scl) + -4;
    goto L_scale;
  }
  else if ((flt) < 1) {
    flt = (flt) * 2;
    scl = (scl) + -1;
    goto L_scale;
  }
  else {
    flt = -1 + (flt);
    goto L_out;
  }
}

static char s_integer_byte_collate_M[] = "integer-byte-collate!";
SCM scm_integer_byte_collate_M(byte_vector)
     SCM byte_vector;
{
  char* bv = get_bytes(byte_vector, -1, s_integer_byte_collate_M);
  bv[0] = 0x80^(bv[0]);
  return byte_vector;
}

static char s_ieee_byte_collate_M[] = "ieee-byte-collate!";
SCM scm_ieee_byte_collate_M(byte_vector)
     SCM byte_vector;
{
  char* byv = get_bytes(byte_vector, 4, s_ieee_byte_collate_M);
  int byvlen = get_bytes_length(byte_vector);
  if (0x80&(byv[0])) {
    int idx = -1 + byvlen;
    while (!(0 > (idx))) {
      byv[idx] = 0xff^(byv[idx]);
      idx = -1+(idx);
    }
  }
  else
    byv[0] = 0x80^(byv[0]);
  return byte_vector;
}

static char s_ieee_byte_decollate_M[] = "ieee-byte-decollate!";
SCM scm_ieee_byte_decollate_M(byte_vector)
     SCM byte_vector;
{
  char* byv = get_bytes(byte_vector, 4, s_ieee_byte_collate_M);
  int byvlen = get_bytes_length(byte_vector);
  if (!(0x80&(byv[0]))) {
    int idx = -1 + byvlen;
    while (!(0 > (idx))) {
      byv[idx] = 0xff^(byv[idx]);
      idx = -1+(idx);
    }
  }
  else
    byv[0] = 0x80^(byv[0]);
  return byte_vector;
}

static iproc subr1s[] = {
	{s_bytes_to_ieee_float, scm_bytes_to_ieee_float},
	{s_bytes_to_ieee_double, scm_bytes_to_ieee_double},
	{s_ieee_float_to_bytes, scm_ieee_float_to_bytes},
	{s_ieee_double_to_bytes, scm_ieee_double_to_bytes},
	{s_integer_byte_collate_M, scm_integer_byte_collate_M},
	{s_ieee_byte_collate_M, scm_ieee_byte_collate_M},
	{s_ieee_byte_decollate_M, scm_ieee_byte_decollate_M},
	{0, 0}};

void init_bytenumb()
{
  list_of_0 = cons(INUM0, EOL);
  scm_gc_protect(list_of_0);
  make_subr(s_bytes_to_integer, tc7_subr_2, scm_bytes_to_integer);
  make_subr(s_integer_to_bytes, tc7_subr_2, scm_integer_to_bytes);
  init_iprocs(subr1s, tc7_subr_1);
  scm_ldstr("\n\
(define (integer-byte-collate byte-vector)\n\
  (integer-byte-collate! (bytes-copy byte-vector)))\n\
(define (ieee-byte-collate byte-vector)\n\
  (ieee-byte-collate! (bytes-copy byte-vector)))\n\
(define (ieee-byte-decollate byte-vector)\n\
  (ieee-byte-decollate! (bytes-copy byte-vector)))\n\
");
  /* add_feature("byte-number"); */
}
