/* "byte.c" Strings as Bytes
 * Copyright (C) 2003, 2006 Free Software Foundation, Inc.
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

char s_make_bytes[] = "make-bytes";
SCM scm_make_bytes(k, n)
     SCM k, n;
{
  SCM res;
  register unsigned char *dst;
  register long i;
  ASRTER(INUMP(k) && (k >= 0), k, ARG1, s_make_bytes);
  i = INUM(k);
  res = makstr(i);
  dst = UCHARS(res);
  if (!UNBNDP(n)) {
    ASRTER(INUMP(n) && 0 <= n && n <= MAKINUM(255), n, ARG2, s_make_bytes);
    for (i--;i >= 0;i--) dst[i] = INUM(n);
  }
  return res;
}
#define s_bytes (s_make_bytes+5)
SCM scm_bytes(ints)
     SCM ints;
{
  SCM res;
  register unsigned char *data;
  long i = ilength(ints);
  ASRTER(i >= 0, ints, ARG1, s_bytes);
  res = makstr(i);
  data = UCHARS(res);
  for (;NNULLP(ints);ints = CDR(ints)) {
    int n = INUM(CAR(ints));
    ASRTER(INUMP(CAR(ints)) && 0 <= n && n <= 255, ints, ARG1, s_bytes);
    *data++ = n;
  }
  return res;
}
static char s_bt_ref[] = "byte-ref";
SCM scm_byte_ref(str, k)
     SCM str, k;
{
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_bt_ref);
  ASRTER(INUMP(k), k, ARG2, s_bt_ref);
  ASRTER(0 <= INUM(k) && INUM(k) < LENGTH(str), k, OUTOFRANGE, s_bt_ref);
  return MAKINUM(UCHARS(str)[INUM(k)]);
}
static char s_bt_set[] = "byte-set!";
SCM scm_byte_set(str, k, n)
     SCM str, k, n;
{
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_bt_set);
  ASRTER(INUMP(k), k, ARG2, s_bt_set);
  ASRTER(INUMP(n), n, ARG3, s_bt_set);
  ASRTER(0 <= INUM(k) && INUM(k) < LENGTH(str), k, OUTOFRANGE, s_bt_set);
  UCHARS(str)[INUM(k)] = INUM(n);
  return UNSPECIFIED;
}
static char s_bytes2list[] = "bytes->list";
SCM scm_bytes2list(str)
     SCM str;
{
  long i;
  SCM res = EOL;
  unsigned char *src;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_bytes2list);
  src = UCHARS(str);
  for (i = LENGTH(str)-1;i >= 0;i--) res = cons((SCM)MAKINUM(src[i]), res);
  return res;
}
static char s_bt_reverse[] = "bytes-reverse!";
SCM scm_bytes_reverse(str)
     SCM str;
{
  register char *dst;
  register long k, len;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_bt_reverse);
  len = LENGTH(str);
  dst = CHARS(str);
  for (k = (len - 1)/2;k >= 0;k--) {
    int tmp = dst[k];
    dst[k] = dst[len - k - 1];
    dst[len - k - 1] = tmp;
  }
  return str;
}
static char s_write_byte[] = "write-byte";
SCM scm_write_byte(chr, port)
     SCM chr, port;
{
  int k = INUM(chr);
  if (UNBNDP(port)) port = cur_outp;
  else ASRTER(NIMP(port) && OPOUTPORTP(port), port, ARG2, s_write_byte);
  ASRTER(INUMP(chr) && 0 <= k && k <= 255, chr, ARG1, s_write_byte);
  lputc(k, port);
  return UNSPECIFIED;
}
static char s_read_byte[] = "read-byte";
SCM scm_read_byte(port)
     SCM port;
{
  int c;
  if (UNBNDP(port)) port = cur_inp;
  ASRTER(NIMP(port) && OPINPORTP(port), port, ARG1, s_read_byte);
  c = lgetc(port);
  if (EOF==c) return EOF_VAL;
  return MAKINUM(c);
}

static char s_sub_rd[] = "subbytes-read!";
SCM scm_subbytes_read(sstr, start, args)
     SCM sstr, start, args;
{
  SCM end, port;
  long len;
  long alen = ilength(args);
  ASRTER(1 <= alen && alen <= 2, args, WNA, s_sub_rd);
  end = CAR(args);
  port = (2==alen) ? CAR(CDR(args)) : cur_inp;
  ASRTER(NIMP(sstr) && STRINGP(sstr), sstr, ARG1, s_sub_rd);
  ASRTER(INUMP(start), start, ARG2, s_sub_rd);
  ASRTER(INUMP(end), end, ARG3, s_sub_rd);
  ASRTER(NIMP(port) && OPINFPORTP(port), port, ARG4, s_sub_rd);
  len = LENGTH(sstr);
  start = INUM(start);
  end = INUM(end);
  ASRTER(0 <= start && start <= len, MAKINUM(start), OUTOFRANGE, s_sub_rd);
  ASRTER(0 <= end && end <= len, MAKINUM(end), OUTOFRANGE, s_sub_rd);
  if (start==end) return INUM0;
  if (start < end) {
    long ans = 0;
    /* An ungetc before an fread will not work on some systems if setbuf(0),
       so we read one element char by char. */
    if (CRDYP(port)) {
      CHARS(sstr)[start] = lgetc(port);
      start += 1;
      len -= 1;
      ans = 1;
    }
    SYSCALL(ans += fread(CHARS(sstr)+start,
			 (sizet)1,
			 (sizet)(end - start),
			 STREAM(port)););
    return MAKINUM(ans);
  }
  else {
    long idx = start;
    while (end < idx) {
      int chr = lgetc(port);
      if (EOF==chr) return MAKINUM(start - idx);
      CHARS(sstr)[--idx] = chr;
    }
    return MAKINUM(start - end);
  }
}

static char s_sub_wr[] = "subbytes-write";
SCM scm_subbytes_write(sstr, start, args)
     SCM sstr, start, args;
{
  SCM end, port;
  long len;
  long alen = ilength(args);
  ASRTER(1 <= alen && alen <= 2, args, WNA, s_sub_wr);
  end = CAR(args);
  port = (2==alen) ? CAR(CDR(args)) : cur_outp;
  ASRTER(NIMP(sstr) && STRINGP(sstr), sstr, ARG1, s_sub_wr);
  ASRTER(INUMP(start), start, ARG2, s_sub_wr);
  ASRTER(INUMP(end), end, ARG3, s_sub_wr);
  ASRTER(NIMP(port) && OPOUTFPORTP(port), port, ARG4, s_sub_wr);
  len = LENGTH(sstr);
  start = INUM(start);
  end = INUM(end);
  ASRTER(0 <= start && start <= len, MAKINUM(start), OUTOFRANGE, s_sub_wr);
  ASRTER(0 <= end && end <= len, MAKINUM(end), OUTOFRANGE, s_sub_wr);
  if (start==end) return INUM0;
  if (start < end) {
    long ans;
    SYSCALL(ans = lfwrite(CHARS(sstr)+start,
			  (sizet)1,
			  (sizet)(sizet)(end - start),
			  port););
    return MAKINUM(ans);
  }
  else {
    long idx = start;
    while (end <= --idx) {
      if (feof(STREAM(port))) return MAKINUM(start - idx - 1);
      lputc(CHARS(sstr)[idx], port);
    }
    return MAKINUM(start - end);
  }
}

static iproc subr1s[] = {
	{"list->bytes", scm_bytes},
	{s_bytes2list, scm_bytes2list},
	{s_bt_reverse, scm_bytes_reverse},
	{0, 0}};

static iproc subr2os[] = {
	{s_write_byte, scm_write_byte},
	{s_make_bytes, scm_make_bytes},
	{0, 0}};

static iproc lsubr2s[] = {
	{s_sub_rd, scm_subbytes_read},
	{s_sub_wr, scm_subbytes_write},
	{0, 0}};


void init_byte()
{
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2os, tc7_subr_2o);
  init_iprocs(lsubr2s, tc7_lsubr_2);
  make_subr(s_bytes, tc7_lsubr, scm_bytes);
  make_subr(s_read_byte, tc7_subr_1o, scm_read_byte);
  make_subr(s_bt_ref, tc7_subr_2, scm_byte_ref);
  make_subr(s_bt_set, tc7_subr_3, scm_byte_set);
  add_feature("byte");
  scm_ldstr("\n\
(define bytes-length string-length)\n\
(define bytes-copy string-copy)\n\
(define bytes-append string-append)\n\
(define subbytes substring)\n\
(define bytes->string cr)\n\
(define string->bytes cr)\n\
(define (bytes-reverse bytes)\n\
  (bytes-reverse! (bytes-copy bytes)))\n\
(define (read-bytes n . port)\n\
  (let* ((len (abs n))\n\
	 (byts (make-bytes len))\n\
	 (cnt (if (positive? n)\n\
		  (apply subbytes-read! byts 0 n port)\n\
		  (apply subbytes-read! byts (- n) 0 port))))\n\
    (if (= cnt len)\n\
	byts\n\
	(if (positive? n)\n\
	    (substring byts 0 cnt)\n\
	    (substring byts (- len cnt) len)))))\n\
(define (write-bytes bytes n . port)\n\
  (if (positive? n)\n\
      (apply subbytes-write bytes 0 n port)\n\
      (apply subbytes-write bytes (- n) 0 port)))\n\
(define substring-read! subbytes-read!)\n\
(define substring-write subbytes-write)\n\
");
}
