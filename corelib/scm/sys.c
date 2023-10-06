/* "sys.c" opening and closing files, storage, and GC.
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

#include <ctype.h>

#include "scm.h"
#include "setjump.h"

#ifdef POCKETCONSOLE
# include <io.h>
#endif

void	igc P((const char *what, SCM basecont));
SCM	*loc_open_file;		/* for open-file callback */
SCM	*loc_try_create_file;

/* ttyname() etc. should be defined in <unistd.h>.  But unistd.h is
   missing on many systems. */

#ifndef STDC_HEADERS
	char *ttyname P((int fd));
	char *tmpnam P((char *s));
# ifdef sun
#  ifndef __SVR4
        int fputs P((char *s, FILE* stream));
        int fputc P((char c, FILE* stream));
        int fflush P((FILE* stream));
#  endif
# else
	sizet fwrite ();
# endif
	int fgetc P((FILE* stream));
	int fclose P((FILE* stream));
	int pclose P((FILE* stream));
	int unlink P((const char *pathname));
	char *mktemp P((char *template));
#else
# ifdef linux
#  include <unistd.h>
# endif
# ifdef __NetBSD__
#  include <unistd.h>
# endif
# ifdef __OpenBSD__
#  include <unistd.h>
# endif
#endif

static void gc_sweep P((int contin_bad));

char	s_nogrow[] = "could not grow", s_heap[] = "heap",
  s_hplims[] = "hplims", s_try_create_file[] = "try-create-file";

static char s_segs[] = "segments", s_numheaps[] = "number of heaps";
static char	s_input_portp[] = "input-port?",
		s_output_portp[] = "output-port?";
#define s_portp (&s_input_portp[6])
static char	s_port_closedp[] = "port-closed?";
static char	s_try_open_file[] = "try-open-file";
#define	s_open_file (&s_try_open_file[4])
char	s_close_port[] = "close-port";

#ifdef __IBMC__
# include <io.h>
# include <direct.h>
# define ttyname(x) "CON:"
#else
# ifndef MSDOS
#  ifndef ultrix
#   ifndef vms
#    ifdef _DCC
#     include <ioctl.h>
#     define setbuf(stream, buf) setvbuf(stream, buf, _IONBF, 0)
#    else
#     ifdef MWC
#      include <sys/io.h>
#     else
#      ifndef macintosh
#       ifndef ARM_ULIB
#        ifndef PLAN9
#         include <sys/ioctl.h>
#        endif
#       endif
#      endif
#     endif
#    endif
#   endif
#  endif
# endif
#endif /* __IBMC__ */
SCM i_setbuf0(port)		/* should be called with DEFER_INTS active */
     SCM port;
{
  VERIFY_INTS("i_setbuf0", 0L);
#ifndef NOSETBUF
# ifndef MSDOS
#  ifdef FIONREAD
#   ifndef ultrix
  SYSCALL(setbuf(STREAM(port), 0L););
#   endif
#  endif
# endif
#endif
  return UNSPECIFIED;
}

/* The CRDY bit is overloaded to indicate that additional processing
   is needed when reading or writing, such as updating line and column
   numbers.  Returns 0 if cmodes is non-null and modes string is not
   valid. */
/* If nonnull, the CMODES argument receives a copy of all chars in MODES
   which are allowed by ANSI C. */
long mode_bits(modes, cmodes)
     char *modes, *cmodes;
{
  int iout = 0;
  long bits = OPN;
  for (; *modes; modes++)
    switch (*modes) {
    case 'r': bits |= RDNG; goto outc;
    case 'w': case 'a': bits |= WRTNG; goto outc;
    case '+': bits |= (RDNG | WRTNG); goto outc;
    case 'b': bits |= BINARY; goto outc;
    case '0': bits |= BUF0; break;
    case '?': bits |= (TRACKED | CRDY); break;
    case 'x': bits |= EXCLUSIVE; break;
    outc: if (cmodes && (iout < 3)) cmodes[iout++] = *modes; break;
    }
  if (!cmodes) return bits;
  cmodes[iout] = 0;
  switch (cmodes[0]) {
  default: return 0;
  case 'r': case 'w': case 'a': return bits;
  }
}

SCM try_open_file(filename, modes)
     SCM filename, modes;
{
  register SCM port;
  FILE *f;
  char cmodes[4];
  long flags;
  ASRTER(NIMP(filename) && STRINGP(filename), filename, ARG1, s_open_file);
  ASRTER(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_open_file);
  flags = mode_bits(CHARS(modes), cmodes);
  ASRTER(flags, modes, ARG2, s_open_file);
  if ((EXCLUSIVE & flags) && NIMP(*loc_try_create_file)) {
    port = apply(*loc_try_create_file, filename, cons(modes, listofnull));
    if (UNSPECIFIED != port) return port;
  }
  DEFER_INTS;
  SCM_OPENCALL((f = fopen(CHARS(filename), cmodes)));
  if (!f) {
    ALLOW_INTS;
    return BOOL_F;
  }
  port = scm_port_entry(f, tc16_fport, flags);
  if (BUF0 & flags) i_setbuf0(port);
  ALLOW_INTS;
  SCM_PORTDATA(port) = filename;
  return port;
}

				/* Callback to Scheme */
SCM open_file(filename, modes)
     SCM filename, modes;
{
  return apply(*loc_open_file,
	       filename,
	       cons(modes, listofnull));
}

long tc16_clport;
SCM close_port(port)
     SCM port;
{
	sizet i;
        SCM ret = UNSPECIFIED;
	ASRTER(NIMP(port) && PORTP(port), port, ARG1, s_close_port);
	if (CLOSEDP(port)) return UNSPECIFIED;
	i = PTOBNUM(port);
	DEFER_INTS;
	if (ptobs[i].fclose) {
          int r;
	  SYSCALL(r = (ptobs[i].fclose)(STREAM(port)););
          if (EOF == r)
            ret = BOOL_F;
          else
            ret = MAKINUM(r);
	}
	CAR(port) &= ~OPN;
	SCM_PORTFLAGS(port) &= ~OPN;
	/* Bash the old ptobnum with the closed port ptobnum.
	   This allows catching some errors cheaply. */
	SCM_SET_PTOBNUM(port, tc16_clport);
	ALLOW_INTS;
	return ret;
}
SCM scm_portp(x)
     SCM x;
{
	if (IMP(x)) return BOOL_F;
	return PORTP(x) ? BOOL_T : BOOL_F;
}
SCM input_portp(x)
     SCM x;
{
	if (IMP(x)) return BOOL_F;
	return INPORTP(x) ? BOOL_T : BOOL_F;
}
SCM output_portp(x)
     SCM x;
{
	if (IMP(x)) return BOOL_F;
	return OUTPORTP(x) ? BOOL_T : BOOL_F;
}
SCM port_closedp(port)
     SCM port;
{
  ASRTER(NIMP(port) && PORTP(port), port, ARG1, s_port_closedp);
  if (CLOSEDP(port)) return BOOL_T;
  return BOOL_F;
}
SCM scm_port_type(port)
     SCM port;
{
  int i;
  if (NIMP(port) && PORTP(port)) {
    i = PTOBNUM(port);
    if (ptobs[i].name) return CAR(sysintern(ptobs[i].name, UNDEFINED));
    return BOOL_T;
  }
  return BOOL_F;
}

#if (__TURBOC__==1)
# undef L_tmpnam		/* Not supported in TURBOC V1.0 */
#endif
#ifdef GO32
# undef L_tmpnam		/* Would put files in %TMPDIR% = %DJDIR%/tmp */
#endif
#ifdef MWC
# undef L_tmpnam
#endif

#ifdef L_tmpnam
SCM ltmpnam()
{
  char name[L_tmpnam];
  char* ret;
  SYSCALL(ret = tmpnam(name););
  if (! ret) return BOOL_F;
  return makfrom0str(name);
}
#else
/* TEMPTEMPLATE is used only if mktemp() is being used instead of
   tmpnam(). */

# ifdef AMIGA
#  define TEMPTEMPLATE "T:SchemeaaaXXXXXX";
# else
#  ifdef vms
#   define TEMPTEMPLATE "sys$scratch:aaaXXXXXX";
#  else /* vms */
#   ifdef __MSDOS__
#    ifdef GO32
#     define TEMPTEMPLATE "\\tmp\\TMPaaaXXXXXX";
#    else
#     define TEMPTEMPLATE "TMPaaaXXXXXX";
#    endif
#   else /* __MSDOS__ */
#    define TEMPTEMPLATE "/tmp/aaaXXXXXX";
#   endif /* __MSDOS__ */
#  endif /* vms */
# endif /* AMIGA */

char template[] = TEMPTEMPLATE;
# define TEMPLEN (sizeof template/sizeof(char) - 1)
SCM ltmpnam()
{
  SCM name;
  int temppos = TEMPLEN-9;
  name = makfromstr(template, (sizet)TEMPLEN);
  DEFER_INTS;
inclp:
  template[temppos]++;
  if (!isalpha(template[temppos])) {
    template[temppos++] = 'a';
    goto inclp;
  }
# ifndef AMIGA
#  ifndef __MSDOS__
  SYSCALL(temppos = !*mktemp(CHARS(name)););
  if (temppos) name = BOOL_F;
#  endif
# endif
  ALLOW_INTS;
  return name;
}
#endif /* L_tmpnam */

#ifdef M_SYSV
# define remove unlink
#endif
static char s_del_fil[] = "delete-file";
SCM del_fil(str)
     SCM str;
{
  int ans;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_del_fil);
#ifdef STDC_HEADERS
  SYSCALL(ans = remove(CHARS(str)););
#else
  SYSCALL(ans = unlink(CHARS(str)););
#endif
  return ans ? BOOL_F : BOOL_T;
}

void prinport(exp, port, type)
     SCM exp; SCM port; char *type;
{
  int filn = fileno(STREAM(exp));
  lputs("#<", port);
  if (CLOSEDP(exp)) lputs("closed-", port);
  else {
    if (RDNG & CAR(exp)) lputs("input-", port);
    if (WRTNG & CAR(exp)) lputs("output-", port);
  }
  lputs(type, port);
  lputc(' ', port);
#ifndef MSDOS
# ifndef __EMX__
#  ifndef _DCC
#   ifndef AMIGA
#    ifndef macintosh
#     ifndef PLAN9
  if (OPENP(exp) && tc16_fport==TYP16(exp) && filn >= 0 && isatty(filn)) {
    char *ttyn = ttyname(filn);
    if (ttyn) lputs(ttyn, port);
    else goto punt;
  }
  else
#     endif
#    endif
#   endif
#  endif
# endif
#endif
  punt:
    {
      SCM s = PORTP(exp) ? SCM_PORTDATA(exp) : UNDEFINED;
      if (NIMP(s) && STRINGP(s))
	scm_iprin1(s, port, 1);
      else if (OPFPORTP(exp))
	scm_intprint((long)filn, 10, port);
      else
	scm_intprint(CDR(exp), -16, port);
      if (TRACKED & SCM_PORTFLAGS(exp)) {
	lputs(" L", port);
	scm_intprint(scm_port_table[SCM_PORTNUM(exp)].line, 10, port);
	lputs(" C", port);
	scm_intprint(scm_port_table[SCM_PORTNUM(exp)].col+0L, 10, port);
      }
    }
  lputc('>', port);
}

static int stputc(c, p)
     int c; SCM p;
{
  sizet ind = INUM(CAR(p));
  if (ind >= LENGTH(CDR(p))) resizuve(CDR(p), MAKINUM(ind + (ind>>1)));
  CHARS(CDR(p))[ind] = c;
  CAR(p) = MAKINUM(ind + 1);
  return c;
}
sizet stwrite(str, siz, num, p)
     sizet siz, num;
     char *str; SCM p;
{
  sizet ind = INUM(CAR(p));
  sizet len = siz * num;
  char *dst;
  if (ind + len >= LENGTH(CDR(p)))
    resizuve(CDR(p), MAKINUM(ind + len + ((ind + len)>>1)));
  dst = &(CHARS(CDR(p))[ind]);
  while (len--) dst[len] = str[len];
  CAR(p) = MAKINUM(ind + siz*num);
  return num;
}
static int stputs(s, p)
     char *s; SCM p;
{
  stwrite(s, 1, strlen(s), p);
  return 0;
}
static int stgetc(p)
     SCM p;
{
  sizet ind = INUM(CAR(p));
  if (ind >= LENGTH(CDR(p))) return EOF;
  CAR(p) = MAKINUM(ind + 1);
  return UCHARS(CDR(p))[ind];
}
static int stclose(p)
     SCM p;
{
  SETCDR(p, nullstr);
  return 0;
}
static int stungetc(c, p)
     int c;
     SCM p;
{
  sizet ind;
  p = CDR(p);
  ind = INUM(CAR(p));
  if (ind == 0) return EOF;
  CAR(p) = MAKINUM(--ind);
  ASRTER(UCHARS(CDR(p))[ind] == c, MAKICHR(c), "stungetc", "");
  return c;
}
int noop0(stream)
     FILE *stream;
{
  return 0;
}
SCM mkstrport(pos, str, modes, caller)
     SCM pos;
     SCM str;
     long modes;
     char *caller;
{
  SCM z;
  ASRTER(INUMP(pos) && INUM(pos) >= 0, pos, ARG1, caller);
  ASRTER(NIMP(str) && (STRINGP(str) || SYMBOLP(str)), str, ARG1, caller);
  str = cons(pos, str);
  NEWCELL(z);
  DEFER_INTS;
  SETCHARS(z, str);
  CAR(z) = (modes | tc16_strport); /* port table entry 0 is scratch. */
  /*  z = scm_port_entry((FILE *)str, tc16_strport, modes); */
  ALLOW_INTS;
  return z;
}
static char s_cwos[] = "call-with-output-string";
static char s_cwis[] = "call-with-input-string";
SCM cwos(proc)
     SCM proc;
{
  SCM p = mkstrport(INUM0, make_string(MAKINUM(30), UNDEFINED),
		    OPN | WRTNG,
		    s_cwos);
  apply(proc, p, listofnull);
  return resizuve(CDR(CDR(p)), CAR(CDR(p)));
}
SCM cwis(str, proc)
     SCM str, proc;
{
  SCM p = mkstrport(INUM0, str, OPN | RDNG, s_cwis);
  return apply(proc, p, listofnull);
}
#ifdef vms
sizet pwrite(ptr, size, nitems, port)
     char *ptr;
     sizet size, nitems;
     FILE* port;
{
  sizet len = size * nitems;
  sizet i = 0;
  for (;i < len;i++) putc(ptr[i], port);
  return len;
}
# define ffwrite pwrite
#else
# define ffwrite fwrite
#endif

static ptobfuns fptob = {
  s_port_type,
  mark0,
  fclose,
  0,
  0,
  fputc,
#ifdef __MWERKS__
  (int (*)(char *, struct _FILE *))fputs,
  (unsigned long (*)(char *, unsigned long, unsigned long, struct _FILE *))ffwrite,
#else
  fputs,
  ffwrite,
#endif
  fflush,
  fgetc,
  fclose};

ptobfuns pipob = {
  0,
  mark0,
  0,				/* replaced by pclose in init_posix() */
  0,
  0,
  fputc,
#ifdef __MWERKS__
  (int (*)(char *, struct _FILE *))fputs,
  (unsigned long (*)(char *, unsigned long, unsigned long, struct _FILE *))ffwrite,
#else
  fputs,
  ffwrite,
#endif
  fflush,
  fgetc};

static ptobfuns stptob = {
  s_string,
  markcdr,
  noop0,
  0,
  0,
  stputc,
  stputs,
  stwrite,
  noop0,
  stgetc,
  stclose,
  stungetc};

				/* Soft ports */

/* fputc, fwrite, fputs, and fclose are called within a
   SYSCALL.  So we need to set errno to 0 before returning.  fflush
   may be called within a SYSCALL.  So we need to set errno to 0
   before returning. */

static int sfputc(c, p)
     int c; SCM p;
{
  SCM arg = MAKICHR(c);
  scm_cvapply(VELTS(p)[0], 1L, &arg);
  errno = 0;
  return c;
}
sizet sfwrite(str, siz, num, p)
     sizet siz, num;
     const void *str; SCM p;
{
  SCM sstr;
  sstr = makfromstr(str, siz * num);
  scm_cvapply(VELTS(p)[1], 1L, &sstr);
  errno = 0;
  return num;
}
static int sfputs(s, p)
     const char *s; SCM p;
{
  sfwrite(s, 1, strlen(s), p);
  return 0;
}
int sfflush(stream)
     SCM stream;
{
  SCM f = VELTS(stream)[2];
  if (BOOL_F==f) return 0;
  f = apply(f, EOL, EOL);
  errno = 0;
  return BOOL_F==f ? EOF : 0;
}
static int sfgetc(p)
     SCM p;
{
  SCM ans;
  ans = scm_cvapply(VELTS(p)[3], 0L, (SCM *)0);
  errno = 0;
  if (FALSEP(ans) || EOF_VAL==ans) return EOF;
  ASRTER(ICHRP(ans), ans, ARG1, "getc");
  return ICHR(ans);
}
static int sfclose(p)
     SCM p;
{
  SCM f = VELTS(p)[4];
  if (BOOL_F==f) return 0;
  f = apply(f, EOL, EOL);
  errno = 0;
  return BOOL_F==f ? EOF : 0;
}
static char s_mksfpt[] = "make-soft-port";
SCM mksfpt(pv, modes)
     SCM pv, modes;
{
  SCM z;
  long flags;
  static long arities[] = {1, 1, 0, 0, 0};
#ifndef RECKLESS
  int i;
  if (! (NIMP(pv) && VECTORP(pv) && 5==LENGTH(pv)))
    badarg: wta(pv, (char *)ARG1, s_mksfpt);
  for (i = 0; i < 5; i++) {
    ASRTGO(FALSEP(VELTS(pv)[i]) ||
	   scm_arity_check(VELTS(pv)[i], arities[i], (char *)0),
	   badarg);
  }
#endif
  ASRTER(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_mksfpt);
  flags = mode_bits(CHARS(modes), (char *)0);
  ASRTER(flags, modes, ARG2, s_mksfpt);
  DEFER_INTS;
  z = scm_port_entry((FILE *)pv, tc16_sfport, flags);
  ALLOW_INTS;
  return z;
}

static ptobfuns sfptob = {
  "soft",
  markcdr,
  noop0,
  0,
  0,
  sfputc,
  sfputs,
  sfwrite,
  sfflush,
  sfgetc,
  sfclose};

		/* Closed ports, just return an error code and let
		   the caller complain. */
static int clputc(c, p)
     int c; FILE *p;
{
  return EOF;
}
static sizet clwrite(str, siz, num, p)
     sizet siz, num;
     char *str; FILE *p;
{
  return 0;
}
static int clputs(s, p)
     char *s; FILE *p;
{
  return EOF;
}
static int clgetc(p)
     FILE *p;
{
  return EOF;
}
static ptobfuns clptob = {
  s_port_type,
  mark0,
  noop0,
  0,
  0,
  clputc,
  clputs,
  clwrite,
  clgetc,
  clgetc,
  0};

/* The following ptob is for printing system messages in an interrupt-safe
   way.  Writing to sys_errp while interrupts are disabled will never enable
   interrupts, do any actual i/o, or any allocation.  Messages will be
   written to cur_errp as soon as interrupts are enabled. There will only
   ever be one of these. */
int output_deferred = 0;
static int tc16_sysport;
#define SYS_ERRP_SIZE 480
static char errbuf[SYS_ERRP_SIZE];
static sizet errbuf_end = 0;

static sizet syswrite(str, siz, num, p)
     sizet siz, num;
     char *str; FILE *p;
{
  sizet src, dst = errbuf_end;
  sizet n = siz*num;
  if (ints_disabled) {
    deferred_proc = process_signals;
    output_deferred = !0;
    for (src = 0; src < n; src++, dst++)
      errbuf[dst % SYS_ERRP_SIZE] = str[src];
    errbuf_end = dst;
  }
  else {
    /* if (NIMP(cur_errp) && OPOUTPORTP(cur_errp)) lfflush(cur_errp); */
    if (errbuf_end > 0) {
      if (errbuf_end > SYS_ERRP_SIZE) {
	scm_warn("output buffer", " overflowed", UNDEFINED);
	scm_intprint((long)errbuf_end, 10, cur_errp);
	lputs(" chars needed\n", cur_errp);
	errbuf_end = errbuf_end % SYS_ERRP_SIZE;
	lfwrite(&errbuf[errbuf_end], 1,
		SYS_ERRP_SIZE - errbuf_end, cur_errp);
      }
      lfwrite(errbuf, sizeof(char), errbuf_end, cur_errp);
      errbuf_end = 0;
    }
    num = lfwrite(str, siz, num, cur_errp);
    /* if (NIMP(cur_errp) && OPOUTPORTP(cur_errp)) lfflush(cur_errp); */
  }
  errno = 0;
  return num;
}
static int sysputs(s, p)
     char *s; FILE *p;
{
  syswrite(s, 1, strlen(s), p);
  return 0;
}
static int sysputc(c, p)
     int c; FILE *p;
{
  char cc = c;
  syswrite(&cc, 1, 1, p);
  return c;
}
static int sysflush(p)
     FILE *p;
{
  syswrite("", 0, 0, p);
  return 0;
}
static ptobfuns sysptob = {
  0,
  mark0,
  noop0,
  0,
  0,
  sysputc,
  sysputs,
  syswrite,
  sysflush,
  noop0,
  noop0};

/* A `safeport' is used for writing objects as part of an error response.
   Since these objects may be very large or circular, the safeport will
   output only a fixed number of characters before exiting via longjmp.
   A setjmp must be done before each use of the safeport. */

static char s_msp[] = "mksafeport";
int tc16_safeport;
SCM mksafeport(maxlen, port)
     int maxlen;
     SCM port;
{
  SCM z;
  if (UNBNDP(port)) port = cur_errp;
  ASRTER(NIMP(port) && OPPORTP(port), port, ARG2, s_msp);
  z = must_malloc_cell(sizeof(safeport)+0L,
		       tc16_safeport | OPN | WRTNG,
		       s_msp);
  ((safeport *)STREAM(z))->ccnt = maxlen;
  ((safeport *)STREAM(z))->port = port;
  return z;
}
int reset_safeport(sfp, maxlen, port)
     int maxlen;
     SCM sfp, port;
{
  if (NIMP(sfp) && tc16_safeport==TYP16(sfp)) {
    ((safeport *)STREAM(sfp))->ccnt = maxlen;
    if (NIMP(port))
      ((safeport *)STREAM(sfp))->port = port;
    return !0;
  }
  return 0;
}
static sizet safewrite(str, siz, num, p)
     sizet siz, num;
     char *str; safeport *p;
{
  int count = p->ccnt;
  sizet n = siz*num;
  if (n < count) {
    p->ccnt = count - n;
    lfwrite(str, siz, num, p->port);
  }
  else if (count) {
    num = count / siz;
    p->ccnt = 0;
    lfwrite(str, siz, num, p->port);
    lputs(" ...", p->port);
    longjmp(p->jmpbuf, !0);	/* The usual C longjmp, not SCM's longjump */
  }
  return num;
}
static int safeputs(s, p)
     char *s; safeport *p;
{
  safewrite(s, 1, strlen(s), p);
  return 0;
}
static int safeputc(c, p)
     int c; safeport *p;
{
  char cc = c;
  safewrite(&cc, 1, 1, p);
  return c;
}
static int safeflush(p)
     safeport *p;
{
  if (p && NIMP(p->port) && OPOUTPORTP(p->port)) lfflush(p->port);
  return 0;
}
static SCM marksafep(ptr)
     SCM ptr;
{
  return ((safeport *)STREAM(ptr))->port;
}
static int freesafep(ptr)
     FILE *ptr;
{
  must_free((char *)ptr, sizeof(safeport));
  return 0;
}
static ptobfuns safeptob = {
  0,
  marksafep,
  freesafep,
  0,
  0,
  safeputc,
  safeputs,
  safewrite,
  safeflush,
  noop0,
  noop0};

static int freeprint(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  if (tc_broken_heart==CAR(exp)) {
    lputs("#<GC-FORWARD->", port);
    scm_iprin1(CDR(exp), port, writing);
  }
  else {
    if (NIMP(CDR(exp)) && tc7_smob==CAR(CDR(exp))) {
      lputs("#<FREE-CELL ", port);
    }
    else {
      lputs("#<NEW-CELL . ", port);
      scm_iprin1(CDR(exp), port, writing);
    }
    lputs(" @0x", port);
    scm_intprint((long)exp, -16, port);
  }
  lputc('>', port);
  return !0;
}
static smobfuns freecell = {
  mark0,
  free0,
  freeprint,
  0};
static smobfuns flob = {
  mark0,
  /*flofree*/0,
  floprint,
#ifdef FLOATS
  floequal
#else
  0
#endif
};
static smobfuns bigob = {
  mark0,
  /*bigfree*/0,
  bigprint,
#ifdef BIGDIG
  bigequal
#else
  0
#endif
};

scm_gra finals_gra;
static char s_final[] = "final";

/* statically allocated ports for diagnostic messages */
static cell tmp_errpbuf[3];
static SCM tmp_errp;
extern sizet num_protects;	/* sys_protects now in scl.c */
void init_types()
{
  sizet j = num_protects;
  while(j) sys_protects[--j] = UNDEFINED;

  /* We need to set up tmp_errp before any errors may be
     thrown, the port_table index will be zero, usable by
     all ports that don't care about their table entries. */
  tmp_errp = PTR2SCM(CELL_UP(&tmp_errpbuf[0]));
  CAR(tmp_errp) = tc16_fport | OPN | WRTNG;
  /*  CAR(tmp_errp) = scm_port_entry(tc16_fport, OPN|WRTNG); */
  SETSTREAM(tmp_errp, stderr);
  cur_errp = def_errp = sys_safep = tmp_errp;

  /* subrs_gra is trimmed to actual used by scm_init_extensions() */
  scm_init_gra(&subrs_gra, sizeof(subr_info), 420 , 0, "subrs");
  scm_init_gra(&ptobs_gra, sizeof(ptobfuns), 8, 255, "ptobs");
  /* These newptob calls must be done in this order */
  /* tc16_fport = */ newptob(&fptob);
  /* tc16_pipe = */ newptob(&pipob);
  /* tc16_strport = */ newptob(&stptob);
  /* tc16_sfport = */ newptob(&sfptob);
  tc16_clport = newptob(&clptob);
  tc16_sysport = newptob(&sysptob);
  tc16_safeport = newptob(&safeptob);
  scm_init_gra(&smobs_gra, sizeof(smobfuns), 16, 255, "smobs");
  /* These newsmob calls must be done in this order */
  newsmob(&freecell);
  newsmob(&flob);
  newsmob(&bigob);
  newsmob(&bigob);
  scm_init_gra(&finals_gra, sizeof(void (*)()), 4, 0, s_final);
}

#ifdef TEST_FINAL
void test_final()
{
  fputs("test_final ok\n", stderr);
}
#endif
void add_final(final)
     void (* final)();
{
  scm_grow_gra(&finals_gra, (char *)&final);
}

static SCM gc_finalizers = EOL, gc_finalizers_pending = EOL;
static char s_add_finalizer[] = "add-finalizer";
SCM scm_add_finalizer(value, finalizer)
     SCM value, finalizer;
{
  SCM z;
  ASRTER(NIMP(value), value, ARG1, s_add_finalizer);
#ifndef RECKLESS
  scm_arity_check(finalizer, 0L, s_add_finalizer);
#endif
  z = acons(value, finalizer, EOL);
  DEFER_INTS;
  CDR(z) = gc_finalizers;
  gc_finalizers = z;
  ALLOW_INTS;
  return UNSPECIFIED;
}

static char s_estk[] = "environment stack";
static cell ecache_v[ECACHE_SIZE];
SCM scm_egc_roots[ECACHE_SIZE/20];
CELLPTR scm_ecache;
VOLATILE long scm_ecache_index, scm_ecache_len, scm_egc_root_index;
SCM scm_estk = UNDEFINED, *scm_estk_ptr;
static SCM estk_pool = EOL;
long scm_estk_size;
static SCM make_stk_seg(size, contents)
     sizet size;
     SCM contents;
{
  SCM seg = BOOL_F, *src, *dst;
  sizet i;
  VERIFY_INTS("make_stk_seg", 0L);
  while NIMP(estk_pool) {
    if (size==LENGTH(estk_pool)) {
      seg = estk_pool;
      estk_pool = SCM_ESTK_PARENT(seg);
      break;
    }
    estk_pool = SCM_ESTK_PARENT(estk_pool);
  }
  if (IMP(seg)) seg = must_malloc_cell((long)size*sizeof(SCM),
				     MAKE_LENGTH(size, tc7_vector), s_estk);
  dst = VELTS(seg);
  if (NIMP(contents)) {
    src = VELTS(contents);
    for (i = size; i--;) dst[i] = src[i];
  }
  else {
    for (i = size; i--;) dst[i] = UNSPECIFIED;
    SCM_ESTK_PARENT(seg) = BOOL_F;
    SCM_ESTK_PARENT_INDEX(seg) = INUM0;
    dst[SCM_ESTK_BASE - 1] = UNDEFINED;	 /* underflow sentinel */
  }
  dst[size - 1] = UNDEFINED;	/* overflow sentinel */
  return seg;
}
/* size is a number of SCM elements, or zero for a default size.
   If nonzero, size must be SCM_ESTK_BASE + N * SCM_ESTK_FRLEN + 1
   for some reasonable number of stackframes N  */
void scm_estk_reset(size)
     sizet size;
{
  VERIFY_INTS("scm_estk_reset", 0L);
  if (!size) size = SCM_ESTK_BASE + 20*SCM_ESTK_FRLEN + 1;
  scm_estk = make_stk_seg(size, UNDEFINED);
  scm_estk_ptr = &(VELTS(scm_estk)[SCM_ESTK_BASE]);
  scm_estk_size = size + 0L;
}
void scm_estk_grow()
{
  /* 40 and 10 below are adjustable parameters:  the number of frames
     in a stack segment, and the number of frames to overlap between
     stack segments. */
  sizet size = 40 * SCM_ESTK_FRLEN + SCM_ESTK_BASE + 1;
  sizet overlap = 10*SCM_ESTK_FRLEN;
  SCM estk = make_stk_seg(size, UNDEFINED);
  SCM *newv, *oldv;
  sizet i, j;
  newv = VELTS(estk);
  oldv = VELTS(scm_estk);
  j = scm_estk_ptr - oldv + SCM_ESTK_FRLEN - overlap;
  SCM_ESTK_PARENT(estk) = scm_estk;
  SCM_ESTK_PARENT_WRITABLEP(estk) = BOOL_T;
  SCM_ESTK_PARENT_INDEX(estk) = MAKINUM(j - SCM_ESTK_FRLEN);
  for (i = SCM_ESTK_BASE; i < SCM_ESTK_BASE + overlap; i++, j++) {
    newv[i] = oldv[j];
    oldv[j] = BOOL_F;
  }
  scm_estk = estk;
  scm_estk_ptr = &(newv[SCM_ESTK_BASE + overlap]);
  scm_estk_size += size + 0L;
  /*  growth_mon(s_estk, scm_estk_size, "locations", !0); */
}
void scm_estk_shrink()
{
  SCM parent;
  sizet i;
  parent = SCM_ESTK_PARENT(scm_estk);
  i = INUM(SCM_ESTK_PARENT_INDEX(scm_estk));
  if (IMP(parent)) wta(UNDEFINED, "underflow", s_estk);
  if (BOOL_F==SCM_ESTK_PARENT_WRITABLEP(scm_estk)) {
    parent = make_stk_seg((sizet)LENGTH(parent), parent);
    SCM_ESTK_PARENT_WRITABLEP(parent) = BOOL_F;
  }
  SCM_ESTK_PARENT(scm_estk) = estk_pool;
  estk_pool = scm_estk;
  scm_estk_size -= LENGTH(scm_estk);
  scm_estk = parent;
  scm_estk_ptr = &(VELTS(parent)[i]);
  /*  growth_mon(s_estk, scm_estk_size, "locations", 0); */
}

void scm_env_cons(x, y)
     SCM x, y;
{
   register SCM z;
   register int i;
   DEFER_INTS_EGC;
   i = scm_ecache_index;
   if (1>i) {
     scm_egc();
     i = scm_ecache_index;
   }
   z = PTR2SCM(&(scm_ecache[--i]));
   CAR(z) = x;
   CDR(z) = y;
   scm_env_tmp = z;
   scm_ecache_index = i;
}

void scm_env_cons2(w, x, y)
     SCM w, x, y;
{
   SCM z1, z2;
   register int i;
   DEFER_INTS_EGC;
   i = scm_ecache_index;
   if (2>i) {
     scm_egc();
     i = scm_ecache_index;
   }
   z1 = PTR2SCM(&(scm_ecache[--i]));
   CAR(z1) = x;
   CDR(z1) = y;
   z2 = PTR2SCM(&(scm_ecache[--i]));
   CAR(z2) = w;
   CDR(z2) = z1;
   scm_env_tmp = z2;
   scm_ecache_index = i;
}

void scm_env_cons3(v, w, x, y)
     SCM v, w, x, y;
{
   SCM z1, z2;
   register int i;
   DEFER_INTS_EGC;
   i = scm_ecache_index;
   if (3>i) {
     scm_egc();
     i = scm_ecache_index;
   }
   z1 = PTR2SCM(&(scm_ecache[--i]));
   CAR(z1) = x;
   CDR(z1) = y;
   z2 = PTR2SCM(&(scm_ecache[--i]));
   CAR(z2) = w;
   CDR(z2) = z1;
   z1 = PTR2SCM(&(scm_ecache[--i]));
   CAR(z1) = v;
   CDR(z1) = z2;
   scm_env_tmp = z1;
   scm_ecache_index = i;
}

void scm_env_v2lst(argc, argv)
     long argc;
     SCM *argv;
{
   SCM z1, z2;
   register int i;
   DEFER_INTS_EGC;
   i = scm_ecache_index;
   if (argc>i) {
     scm_egc();
     i = scm_ecache_index;
   }
   z1 = z2 = scm_env_tmp;	/* set z1 just in case argc is zero */
   while (argc--) {
     z1 = PTR2SCM(&(scm_ecache[--i]));
     CAR(z1) = argv[argc];
     CDR(z1) = z2;
     z2 = z1;
   }
   scm_env_tmp = z1;
   scm_ecache_index = i;
}

/* scm_env = acons(names, scm_env_tmp, scm_env) */
void scm_extend_env()
{
   SCM z;
   register int i;
   DEFER_INTS_EGC;
   i = scm_ecache_index;
   if (1>i) {
     scm_egc();
     i = scm_ecache_index;
   }
   z = PTR2SCM(&(scm_ecache[--i]));
   CAR(z) = scm_env_tmp;
   CDR(z) = scm_env;
   scm_env = z;
   scm_ecache_index = i;
}
void old_scm_extend_env(names)
     SCM names;
{
   SCM z1, z2;
   register int i;
   DEFER_INTS_EGC;
   i = scm_ecache_index;
   if (2>i) {
     scm_egc();
     i = scm_ecache_index;
   }
   z1 = PTR2SCM(&(scm_ecache[--i]));
   CAR(z1) = names;
   CDR(z1) = scm_env_tmp;
   z2 = PTR2SCM(&(scm_ecache[--i]));
   CAR(z2) = z1;
   CDR(z2) = scm_env;
   scm_env = z2;
   scm_ecache_index = i;
}
char s_obunhash[] = "object-unhash", s_cache_gc[] = "cache_gc";
char s_recursive[] = "recursive";
#define s_gc (s_cache_gc+6)
static iproc subr0s[] = {
	{"tmpnam", ltmpnam},
	{"open-ports", scm_open_ports},
	{0, 0}};

static iproc subr1s[] = {
	{s_input_portp, input_portp},
	{s_output_portp, output_portp},
	{s_portp, scm_portp},
	{s_port_closedp, port_closedp},
	{s_close_port, close_port},
	{"eof-object?", eof_objectp},
	{"port-type", scm_port_type},
	{s_cwos, cwos},
	{"object-hash", obhash},
	{s_obunhash, obunhash},
	{s_del_fil, del_fil},
	{0, 0}};

static iproc subr2s[] = {
	{s_try_open_file, try_open_file},
	{s_cwis, cwis},
	{s_mksfpt, mksfpt},
	{s_add_finalizer, scm_add_finalizer},
	{0, 0}};

SCM dynwind P((SCM thunk1, SCM thunk2, SCM thunk3));
void init_io()
{
  make_subr("dynamic-wind", tc7_subr_3, dynwind);
  make_subr(s_gc, tc7_subr_1o, gc);
  init_iprocs(subr0s, tc7_subr_0);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);
  loc_open_file =
    &CDR(sysintern(s_open_file,
		   CDR(sysintern(s_try_open_file, UNDEFINED))));
  loc_try_create_file = &CDR(sysintern(s_try_create_file, UNDEFINED));
#ifndef CHEAP_CONTINUATIONS
  add_feature("full-continuation");
#endif
#ifdef TEST_FINAL
  add_final(test_final);
#endif
}

void grew_lim(nm)
     long nm;
{
  growth_mon(s_limit, nm, "bytes", !0);
}
int expmem = 0;
sizet hplim_ind = 0;
long heap_cells = 0;
CELLPTR *hplims, heap_org;
VOLATILE SCM freelist = EOL;
long mltrigger, mtrigger = INIT_MALLOC_LIMIT;
int gc_hook_pending = 0, gc_hook_active = 0;

/* Ints should be deferred when calling igc_for_alloc. */
static char *igc_for_alloc(where, olen, size, what)
     char *where;
     unsigned long olen;
     unsigned long size;
     const char *what;
{
  char *ptr;
  unsigned long nm;
			/* Check to see that heap is initialized */
  ASRTER(heap_cells > 0, MAKINUM(size), NALLOC, what);
/* printf("igc_for_alloc(%lx, %lu, %u, %s)\n", where, olen, size, what); fflush(stdout); */
  igc(what, rootcont);
  nm = mallocated + size - olen;
  if (nm > mltrigger) {
    if (nm > mtrigger) grew_lim(nm + nm/2);
    else grew_lim(mtrigger + mtrigger/2);
  }
  if (where) SYSCALL(ptr = (char *)realloc(where, size););
  else SYSCALL(ptr = (char *)malloc(size););
  ASRTER(ptr, MAKINUM(size), NALLOC, what);
  if (nm > mltrigger) {
    if (nm > mtrigger) mtrigger = nm + nm/2;
    else mtrigger += mtrigger/2;
    mltrigger = mtrigger - MIN_MALLOC_YIELD;
  }
  mallocated = nm;
  return ptr;
}
char *must_malloc(len, what)
     long len;
     const char *what;
{
  char *ptr;
  sizet size = len;
  unsigned long nm = mallocated + size;
  VERIFY_INTS("must_malloc", what);
#ifdef SHORT_SIZET
  ASRTER(len==size, MAKINUM(len), NALLOC, what);
#endif
  if (nm <= mtrigger) SYSCALL(ptr = (char *)malloc(size););
  else ptr = 0;
  if (!ptr) ptr = igc_for_alloc(0L, 0L, size+0L, what);
  else mallocated = nm;
/* printf("must_malloc(%lu, %s) => %lx\n", len, what, ptr); fflush(stdout); */
  return ptr;
}
SCM must_malloc_cell(len, c, what)
     long len;
     SCM c;
     const char *what;
{
  SCM z;
  char *ptr;
  sizet size = len;
  unsigned long nm = mallocated + size;
  VERIFY_INTS("must_malloc_cell", what);
#ifdef SHORT_SIZET
  ASRTER(len==size, MAKINUM(len), NALLOC, what);
#endif
  NEWCELL(z);
  if (nm <= mtrigger) SYSCALL(ptr = (char *)malloc(size););
  else ptr = 0;
  if (!ptr) ptr = igc_for_alloc(0L, 0L, size+0L, what);
  else mallocated = nm;
/* printf("must_malloc_cell(%lu, %lx, %s) => %lx\n", len, c, what, ptr); fflush(stdout); */
  SETCHARS(z, ptr);
  CAR(z) = c;
  return z;
}
char *must_realloc(where, olen, len, what)
     char *where;
     unsigned long olen, len;
     const char *what;
{
  char *ptr;
  sizet size = len;
  unsigned long nm = mallocated + size - olen;
  VERIFY_INTS("must_realloc", what);
#ifdef SHORT_SIZET
  ASRTER(len==size, MAKINUM(len), NALLOC, what);
#endif
  ASRTER(!errjmp_bad, MAKINUM(len), NALLOC, what);
/* printf("must_realloc(%lx, %lu, %lu, %s)\n", where, olen, len, what); fflush(stdout);
   printf("nm = %ld <= mtrigger = %ld: %d; size = %u\n", nm, mtrigger, (nm <= mtrigger), size); fflush(stdout); */
  if (nm <= mtrigger) SYSCALL(ptr = (char *)realloc(where, size););
  else ptr = 0;
  if (!ptr) ptr = igc_for_alloc(where, olen, size+0L, what);
  else mallocated = nm;
  return ptr;
}
void must_realloc_cell(z, olen, len, what)
     SCM z;
     unsigned long olen, len;
     const char *what;
{
  char *ptr, *where = CHARS(z);
  sizet size = len;
  unsigned long nm = mallocated + size - olen;
  VERIFY_INTS("must_realloc_cell", what);
#ifdef SHORT_SIZET
  ASRTER(len==size, MAKINUM(len), NALLOC, what);
#endif
  ASRTER(!errjmp_bad, MAKINUM(len), NALLOC, what);
/* printf("must_realloc_cell(%lx, %lu, %lu, %s)\n", z, olen, len, what); fflush(stdout); */
  if (nm <= mtrigger) SYSCALL(ptr = (char *)realloc(where, size););
  else ptr = 0;
  if (!ptr) ptr = igc_for_alloc(where, olen, size+0L, what);
  else mallocated = nm;
  SETCHARS(z, ptr);
}
void must_free(obj, len)
     char *obj;
     sizet len;
{
  if (obj) {
#ifdef CAREFUL_INTS
    while (len--) obj[len] = '#';
#endif
/* printf("free(%lx)\n", obj); fflush(stdout); */
    free(obj);
    mallocated = mallocated - len;
  }
  else wta(INUM0, "already free", "");
}

SCM symhash;			/* This used to be a sys_protect, but
				   Radey Shouman <shouman@zianet.com>
				   added GC for unused, UNDEFINED
				   symbols.*/
int no_symhash_gc =
#ifdef NO_SYM_GC
  !0				/* Hobbit-compiled code must not GC symhash. */
#else
  0
#endif
  ;
int symhash_dim = NUM_HASH_BUCKETS;
/* sym2vcell looks up the symbol in the symhash table. */
SCM sym2vcell(sym)
     SCM sym;
{
  SCM lsym, z;
  sizet hash = strhash(UCHARS(sym), (sizet)LENGTH(sym),
		       (unsigned long)symhash_dim);
  for (lsym = VELTS(symhash)[hash];NIMP(lsym);lsym = CDR(lsym)) {
    z = CAR(lsym);
    if (CAR(z)==sym) return z;
  }
  wta(sym, "uninterned symbol? ", "");
}
/* intern() and sysintern() return a pair;
   CAR is the symbol, CDR is the value. */
SCM intern(name, len)
     char *name;
     sizet len;
{
  SCM lsym, z;
  register sizet i = len;
  register unsigned char *tmp = (unsigned char *)name;
  sizet hash = strhash(tmp, i, (unsigned long)symhash_dim);
  /* printf("intern %s len=%d\n",name,len); fflush(stdout); */
  DEFER_INTS;
  for (lsym = VELTS(symhash)[hash];NIMP(lsym);lsym = CDR(lsym)) {
    z = CAR(lsym);
    z = CAR(z);
    tmp = UCHARS(z);
    if (LENGTH(z) != len) goto trynext;
    for (i = len;i--;) if (((unsigned char *)name)[i] != tmp[i]) goto trynext;
    ALLOW_INTS;
    return CAR(lsym);
  trynext: ;
  }
  /*  lsym = makfromstr(name, len); */
  lsym = must_malloc_cell(len+1L, MAKE_LENGTH(len, tc7_msymbol), s_string);
  i = len;
  CHARS(lsym)[len] = 0;
  while (i--) CHARS(lsym)[i] = name[i];
  z = acons(lsym, UNDEFINED, UNDEFINED);
  CDR(z) = VELTS(symhash)[hash];
  VELTS(symhash)[hash] = z;
  z = CAR(z);
  ALLOW_INTS;
  return z;
}
SCM sysintern(name, val)
     const char *name;
     SCM val;
{
  SCM lsym, z;
  sizet len = strlen(name);
  register sizet i = len;
  register unsigned char *tmp = (unsigned char *)name;
  sizet hash = strhash(tmp, i, (unsigned long)symhash_dim);
  for (lsym = VELTS(symhash)[hash];NIMP(lsym);lsym = CDR(lsym)) {
    z = CAR(lsym);
    z = CAR(z);
    tmp = UCHARS(z);
    if (LENGTH(z) != len) goto trynext;
    for (i = len;i--;) if (((unsigned char *)name)[i] != tmp[i]) goto trynext;
    lsym = CAR(lsym);
    if (!UNBNDP(val)) CDR(lsym) = val;
    else if (UNBNDP(CDR(lsym)) && tc7_msymbol==TYP7(CAR(lsym)))
      scm_gc_protect(lsym);
    return lsym;
  trynext: ;
  }
  NEWCELL(lsym);
  SETLENGTH(lsym, len, tc7_ssymbol);
  SETCHARS(lsym, name);
  lsym = cons(lsym, val);
  z = cons(lsym, UNDEFINED);
  CDR(z) = VELTS(symhash)[hash];
  VELTS(symhash)[hash] = z;
  return lsym;
}
SCM cons(x, y)
     SCM x, y;
{
	register SCM z;
	NEWCELL(z);
	CAR(z) = x;
	CDR(z) = y;
	return z;
}
SCM cons2(w, x, y)
     SCM w, x, y;
{
	register SCM z;
	NEWCELL(z);
	CAR(z) = x;
	CDR(z) = y;
	x = z;
	NEWCELL(z);
	CAR(z) = w;
	CDR(z) = x;
	return z;
}
SCM acons(w, x, y)
     SCM w, x, y;
{
	register SCM z;
	NEWCELL(z);
	CAR(z) = w;
	CDR(z) = x;
	x = z;
	NEWCELL(z);
	CAR(z) = x;
	CDR(z) = y;
	return z;
}

SCM makstr(len)
     long len;
{
	SCM s;
#ifndef SHORT_SIZET
	ASRTER(!(len & ~LENGTH_MAX), MAKINUM(len), NALLOC, s_string);
#endif
	DEFER_INTS;
	s = must_malloc_cell(len+1L, MAKE_LENGTH(len, tc7_string), s_string);
	CHARS(s)[len] = 0;
	ALLOW_INTS;
	return s;
}

char s_redefining[] = "redefining ";
scm_gra subrs_gra;
SCM scm_maksubr(name, type, fcn)
     const char *name;
     int type;
     SCM (*fcn)();
{
	subr_info info;
	int isubr;
	register SCM z;
	info.name = name;
	for (isubr = subrs_gra.len; 0 < isubr--;) {
	  if (0==strcmp(((char **)subrs_gra.elts)[isubr], name)) {
	    scm_warn(s_redefining, (char *)name, UNDEFINED);
	    goto foundit;
	  }
	}
	isubr = scm_grow_gra(&subrs_gra, (char *)&info);
 foundit:
	NEWCELL(z);
	if (!fcn && tc7_cxr==type) {
	  const char *p = name;
	  int code = 0;
	  while (*++p != 'r')
	    switch (*p) {
	    default: wta(UNDEFINED, "bad cxr name", (char *)name);
	    case 'a': code = (code<<2) + 1; continue;
	    case 'd': code = (code<<2) + 2; continue;
	    }
	  type += (code << 8);
	}
	CAR(z) = (isubr<<16) + type;
	SUBRF(z) = fcn;
	return z;
}
SCM make_subr(name, type, fcn)
     const char *name;
     int type;
     SCM (*fcn)();
{
	return CDR(sysintern(name, scm_maksubr(name, type, fcn)));
}

#ifdef CCLO
char s_comp_clo[] = "compiled-closure";
SCM makcclo(proc, len)
     SCM proc;
     long len;
{
  SCM s;
# ifndef SHORT_SIZET
  ASRTER(len < (((unsigned long)-1L)>>16), UNDEFINED, NALLOC, s_comp_clo);
# endif
  DEFER_INTS;
  s = must_malloc_cell(len*sizeof(SCM), MAKE_NUMDIGS(len, tc16_cclo),
		       s_comp_clo);
  while (--len) VELTS(s)[len] = UNSPECIFIED;
  CCLO_SUBR(s) = proc;
  ALLOW_INTS;
  return s;
}
#endif

void stack_check()
{
  STACKITEM *start = CONT(rootcont)->stkbse;
  STACKITEM stack;
#ifdef STACK_GROWS_UP
  if (&stack - start > STACK_LIMIT/sizeof(STACKITEM))
#else
  if (start - &stack > STACK_LIMIT/sizeof(STACKITEM))
#endif /* def STACK_GROWS_UP */
    {
      stack_report();
      wta(UNDEFINED, (char *)SEGV_SIGNAL, "stack");
    }
}

void stack_report()
{
  STACKITEM stack;
  lputs(";; stack: 0x", cur_errp);
  scm_intprint((long)CONT(rootcont)->stkbse, -16, cur_errp);
  lputs(" - 0x", cur_errp);
  scm_intprint((long)&stack, -16, cur_errp);
  lputs("; ", cur_errp);
  scm_intprint(stack_size(CONT(rootcont)->stkbse)*sizeof(STACKITEM), 10, cur_errp);
  lputs(" bytes\n", cur_errp);
}

SCM dynwind(thunk1, thunk2, thunk3)
     SCM thunk1, thunk2, thunk3;
{
  SCM ans;
  apply(thunk1, EOL, EOL);
  dynwinds = acons(thunk1, thunk3, dynwinds);
  ans = apply(thunk2, EOL, EOL);
  dynwinds = CDR(dynwinds);
  apply(thunk3, EOL, EOL);
  return ans;
}
void downd(to, delta)
     SCM to;
     long delta;
{
 tail:
  if (dynwinds==to);
  else if (0 > delta) {
    downd(CDR(to), 1+delta);
    apply(CAR(CAR(to)), EOL, EOL);
    dynwinds = to;
  }
  else {
    SCM from = CDR(CAR(dynwinds));
    dynwinds = CDR(dynwinds);
    apply(from, EOL, EOL);
    delta--; goto tail;		/* downd(to, delta-1); */
  }
}
void dowinds(to)
     SCM to;
{
  downd(to,  ilength(dynwinds) - ilength(to));
}

/* Remember that setjump needs to be called after scm_make_cont */

SCM scm_make_cont()
{
  SCM cont, estk, *from;
  CONTINUATION *ncont;
  sizet n;
  VERIFY_INTS("scm_make_cont", 0L);
  NEWCELL(cont);
  from = VELTS(scm_estk);
  n = scm_estk_ptr - from + SCM_ESTK_FRLEN;
#ifdef CHEAP_CONTINUATIONS
  estk = scm_estk;
#else
  from[1] = BOOL_F;		/* Can't write to parent stack */
  estk = must_malloc_cell((long)n*sizeof(SCM),
			  MAKE_LENGTH(n, tc7_vector), s_cont);
  {
    SCM *to = VELTS(estk);
    while(n--) to[n] = from[n];
  }
#endif
  ncont = make_continuation(CONT(rootcont));
  if (!ncont) wta(MAKINUM(-1), (char *)NALLOC, s_cont);
  ncont->other.parent = rootcont;
  SETCONT(cont, ncont);
  SETLENGTH(cont, ncont->length, tc7_contin);
  ncont->other.dynenv = dynwinds;
  ncont->other.stkframe[0] = scm_env;
  ncont->other.stkframe[1] = scm_env_tmp;
  ncont->other.estk = estk;
#ifdef CHEAP_CONTINUATIONS
  ncont->other.estk_ptr = scm_estk_ptr;
#else
  ncont->other.estk_ptr = (SCM *)0;
#endif
#ifndef RECKLESS
  ncont->other.stkframe[2] = scm_trace_env;
  ncont->other.stkframe[3] = scm_trace;
#endif
  return cont;
}
static char s_sstale[] = "strangely stale";
void scm_dynthrow(tocont, arg1, arg2, rest)
     SCM tocont;
     SCM arg1, arg2, rest;
{
  CONTINUATION *cont = CONT(tocont);
  if (cont->stkbse != CONT(rootcont)->stkbse)
    wta(tocont, &s_sstale[10], s_cont);
  dowinds(cont->other.dynenv);
  {
    DEFER_INTS;
#ifdef CHEAP_CONTINUATIONS
    scm_estk = cont->other.estk;
    scm_estk_ptr = cont->other.estk_ptr;
#else
    {
      SCM *to, *from =  VELTS(cont->other.estk);
      sizet n = LENGTH(cont->other.estk);
      if (LENGTH(scm_estk) < n) scm_estk_reset(n);
      to = VELTS(scm_estk);
      scm_estk_ptr = &(to[n - SCM_ESTK_FRLEN]);
      while(n--) to[n] = from[n];
    }
#endif
    scm_env = cont->other.stkframe[0];
    scm_env_tmp = cont->other.stkframe[1];
#ifndef RECKLESS
    scm_trace_env = cont->other.stkframe[2];
    scm_trace = cont->other.stkframe[3];
#endif
    if (!UNBNDP(arg2) && IM_VALUES_TOKEN == scm_env_tmp) {
      scm_env_cons(arg2, rest);
      arg2 = UNDEFINED;
    }
    ALLOW_INTS;
  }
  if (!UNBNDP(arg2)) return;	/* eval will signal wrong number of args */
  throw_to_continuation(cont, arg1, CONT(rootcont));
  wta(tocont, s_sstale, s_cont);
}

SCM obhash(obj)
     SCM obj;
{

#ifdef BIGDIG
  long n = SRS(obj, 1);
  if (!FIXABLE(n)) return long2big(n);
#endif
  return (obj<<1)+2L;
}

SCM obunhash(obj)
     SCM obj;
{
#ifdef BIGDIG
  if (NIMP(obj) && BIGP(obj)) {
    sizet i = NUMDIGS(obj);
    BIGDIG *ds = BDIGITS(obj);
    if (TYP16(obj)==tc16_bigpos) {
      obj = 0;
      while (i--) obj = BIGUP(obj) + ds[i];
    }
    else {
      obj = 0;
      while (i--) obj = BIGUP(obj) - ds[i];
    }
    obj <<= 1;
    goto comm;
  }
#endif
  ASRTER(INUMP(obj), obj, ARG1, s_obunhash);
  obj = SRS(obj, 1) & ~1L;
comm:
  if (IMP(obj)) return obj;
  if (NCELLP(obj)) return BOOL_F;
  {
    /* This code is adapted from mark_locations() in "sys.c" and
       scm_cell_p() in "rope.c", which means that changes to these
       routines must be coordinated. */
    register CELLPTR ptr = (CELLPTR)SCM2PTR(obj);
    register sizet i = 0, j = hplim_ind;
    do {
      if (PTR_GT(hplims[i++], ptr)) break;
      if (PTR_LE(hplims[--j], ptr)) break;
      if ((i != j)
	  && PTR_LE(hplims[i++], ptr)
	  && PTR_GT(hplims[--j], ptr)) continue;
      if (NFREEP(obj)) return obj;
      break;
    } while(i<j);
  }
  return BOOL_F;
}

unsigned long strhash(str, len, n)
     unsigned char *str;
     sizet len;
     unsigned long n;
{
  if (len>5)
    {
      sizet i = 5;
      unsigned long h = 264 % n;
      while (i--) h = ((h<<8) + ((unsigned)(downcase[str[h % len]]))) % n;
      return h;
    }
  else {
    sizet i = len;
    unsigned long h = 0;
    while (i) h = ((h<<8) + ((unsigned)(downcase[str[--i]]))) % n;
    return h;
  }
}

static void fixconfig(s1, s2, s)
     char *s1, *s2;
     int s;
{
  fputs(s1, stderr);
  fputs(s2, stderr);
  fputs("\nin ", stderr);
  fputs(s ? "setjump" : "scmfig", stderr);
  fputs(".h and recompile scm\n", stderr);
  quit(MAKINUM(1L));
}

void heap_report()
{
  sizet i = 0;
  if (hplim_ind) lputs("; heap segments:", sys_errp);
  while(i < hplim_ind) {
    {
      long seg_cells = CELL_DN(hplims[i+1]) - CELL_UP(hplims[i]);
      lputs("\n; 0x", sys_errp);
      scm_intprint((long)hplims[i++], -16, sys_errp);
      lputs(" - 0x", sys_errp);
      scm_intprint((long)hplims[i++], -16, sys_errp);
      lputs("; ", sys_errp);
      scm_intprint(seg_cells, 10, sys_errp);
      lputs(" cells; ", sys_errp);
      scm_intprint(seg_cells / (1024 / sizeof(CELLPTR)), 10, sys_errp);
      lputs(".kiB", sys_errp);
    }}
}
sizet init_heap_seg(seg_org, size)
     CELLPTR seg_org;
     sizet size;
{
  register CELLPTR ptr = seg_org;
#ifdef POINTERS_MUNGED
  register SCM scmptr;
#else
# define scmptr ptr
#endif
  CELLPTR seg_end = CELL_DN((char *)ptr + size);
  sizet i = hplim_ind, ni = 0;
  if (ptr==NULL) return 0;
  while((ni < hplim_ind) && PTR_LE(hplims[ni], seg_org)) ni++;
  while(i-- > ni) hplims[i+2] = hplims[i];
  hplim_ind += 2;
  hplims[ni++] = ptr;		/* same as seg_org here */
  hplims[ni++] = seg_end;
  ptr = CELL_UP(ptr);
  ni = seg_end - ptr;
/* printf("ni = %u; hplim_ind = %u\n", ni, hplim_ind); */
/* printf("ptr = %lx\n", ptr); */
  for (i = ni;i--;ptr++) {
#ifdef POINTERS_MUNGED
    scmptr = PTR2SCM(ptr);
#endif
    CAR(scmptr) = (SCM)tc_free_cell;
    CDR(scmptr) = PTR2SCM(ptr+1);
  }
/*  CDR(scmptr) = freelist; */
  CDR(PTR2SCM(--ptr)) = freelist;
  freelist = PTR2SCM(CELL_UP(seg_org));
  heap_cells += ni;
  return size;
#ifdef scmptr
# undef scmptr
#endif
}
static void alloc_some_heap()
{
  CELLPTR ptr, *tmplims;
  sizet len = (2+hplim_ind)*sizeof(CELLPTR);
  ASRTGO(len==(2+hplim_ind)*sizeof(CELLPTR), badhplims);
  if (errjmp_bad) wta(UNDEFINED, "need larger initial", s_heap);
  tmplims = (CELLPTR *)must_realloc((char *)hplims,
				    len-2L*sizeof(CELLPTR), (long)len,
				    s_heap);
  /*  SYSCALL(tmplims = (CELLPTR *)realloc((char *)hplims, len);); */
  if (!tmplims)
badhplims:
    wta(UNDEFINED, s_nogrow, s_hplims);
  else hplims = tmplims;
  /* hplim_ind gets incremented in init_heap_seg() */
  if (expmem) {
    len = (sizet)(EXPHEAP(heap_cells)*sizeof(cell));
    if ((sizet)(EXPHEAP(heap_cells)*sizeof(cell)) != len) len = 0;
  }
  else len = HEAP_SEG_SIZE;
  while (len >= MIN_HEAP_SEG_SIZE) {
    SYSCALL(ptr = (CELLPTR) malloc(len););
    if (ptr) {
      init_heap_seg(ptr, len);
      return;
    }
    len /= 2;
  }
  wta(UNDEFINED, s_nogrow, s_heap);
}

/* Initialize a Growable arRAy, of initial size LEN, growing to at most
   MAXLEN elements of size ELTSIZE */
void scm_init_gra(gra, eltsize, len, maxlen, what)
     scm_gra *gra;
     sizet eltsize, len, maxlen;
     const char *what;
{
  char *nelts;
  /* DEFER_INTS; */
  /* Can't call must_malloc, because heap may not be initialized yet. */
  /*  SYSCALL(nelts = malloc(len*eltsize););
      if (!nelts) wta(MAKINUM(len*eltsize), (char *)NALLOC, what);
      mallocated += len*eltsize;
  */
  nelts = must_malloc((long)len*eltsize, what);
  gra->eltsize = eltsize;
  gra->len = 0;
  gra->elts = nelts;
  gra->alloclen = len;
  gra->maxlen = maxlen;
  gra->what = what;
  /* ALLOW_INTS; */
}
/* Returns the index into the elt array */
int scm_grow_gra(gra, elt)
     scm_gra *gra;
     char *elt;
{
  int i;
  char *tmp;
  if (gra->alloclen <= gra->len) {
    sizet inc = gra->len / 5 + 1;
    sizet nlen = gra->len + inc;
    if (gra->maxlen && nlen > gra->maxlen)
      /* growerr: */ wta(MAKINUM(nlen), (char *)NALLOC, gra->what);
    /*
      SYSCALL(tmp = realloc(gra->elts, nlen*gra->eltsize););
      if (!tmp) goto growerr;
      mallocated += (nlen - gra->alloclen)*gra->eltsize;
    */
    tmp = must_realloc(gra->elts, (long)gra->alloclen*gra->eltsize,
		       (long)nlen*gra->eltsize, gra->what);
    gra->elts = tmp;
    gra->alloclen = nlen;
  }
  tmp = &gra->elts[gra->len*gra->eltsize];
  gra->len += 1;
  for (i = 0; i < gra->eltsize; i++)
    tmp[i] = elt[i];
  return gra->len - 1;
}
void scm_trim_gra(gra)
     scm_gra *gra;
{
  char *tmp;
  long curlen = gra->len;
  if (0L==curlen) curlen = 1L;
  if (curlen==(long)gra->alloclen) return;
  tmp = must_realloc(gra->elts,
		     (long)gra->alloclen * gra->eltsize,
		     curlen * gra->eltsize,
		     gra->what);
  gra->elts = tmp;
  gra->alloclen = curlen;
}
void scm_free_gra(gra)
     scm_gra *gra;
{
  free(gra->elts);
  gra->elts = 0;
  mallocated -= gra->maxlen*gra->eltsize;
}
void gra_report1(gra)
     scm_gra *gra;
{
  scm_intprint((long)gra->len, -10, cur_errp);
  lputs(" (of ", cur_errp);
  scm_intprint((long)gra->alloclen, -10, cur_errp);
  lputs(") ", cur_errp);
  lputs(gra->what, cur_errp);
  lputs("; ", cur_errp);
}
void gra_report()
{
  lputs(";; gra: ", cur_errp);
  gra_report1(&ptobs_gra);
  gra_report1(&smobs_gra);
  gra_report1(&finals_gra);
  gra_report1(&subrs_gra);
  lputs("\n", cur_errp);
}

scm_gra smobs_gra;
long newsmob(smob)
     smobfuns *smob;
{
  return tc7_smob + 256*scm_grow_gra(&smobs_gra, (char *)smob);
}
scm_gra ptobs_gra;
long newptob(ptob)
     ptobfuns *ptob;
{
  return tc7_port + 256*scm_grow_gra(&ptobs_gra, (char *)ptob);
}
port_info *scm_port_table = 0;
static sizet scm_port_table_len = 0;
static char s_port_table[] = "port table";
SCM scm_port_entry(stream, ptype, flags)
     FILE *stream;
     long ptype, flags;
{
  SCM z;
  sizet nlen;
  int i, j;
  VERIFY_INTS("scm_port_entry", 0L);
  flags = flags | (ptype & ~0xffffL);
  ASRTER(flags, INUM0, ARG1, "scm_port_entry");
  for (i = 1; i < scm_port_table_len; i++)
    if (0L==scm_port_table[i].flags) goto ret;
  if (scm_port_table_len <= SCM_PORTNUM_MAX) {
    nlen = scm_port_table_len + (scm_port_table_len / 2);
    if (nlen >= SCM_PORTNUM_MAX) nlen = (sizet)SCM_PORTNUM_MAX + 1;
    scm_port_table = (port_info *)
      must_realloc((char *)scm_port_table,
		   (long)scm_port_table_len * sizeof(port_info),
		   (long)nlen * sizeof(port_info),
		   s_port_table);
    scm_port_table_len = nlen;
    growth_mon(s_port_table, nlen + 0L, "entries", !0);
    for (j = i; j < scm_port_table_len; j++) {
      scm_port_table[j].flags = 0L;
      scm_port_table[j].data = UNDEFINED;
      scm_port_table[j].port = UNDEFINED;
    }
  }
  else {
    igc(s_port_table, rootcont);
    for (i = 0; i < scm_port_table_len; i++)
      if (0L==scm_port_table[i].flags) goto ret;
    wta(UNDEFINED, s_nogrow, s_port_table);
  }
 ret:
  NEWCELL(z);
  SETSTREAM(z, stream);
  CAR(z) = (((long)i)<<20) | (flags & 0x0f0000) | ptype;
  scm_port_table[i].unread = EOF;
  scm_port_table[i].flags = flags;
  scm_port_table[i].line = 1L;	/* should both be one-based? */
  scm_port_table[i].col = 1;
  scm_port_table[i].data = UNSPECIFIED;
  scm_port_table[i].port = z;
  return z;
}
SCM scm_open_ports()
{
  SCM p, res = EOL;
  int k;
  for (k = scm_port_table_len - 1; k > 0; k--) {
    p = scm_port_table[k].port;
    if (NIMP(p) && OPPORTP(p))
      res = cons(p, res);
  }
  return res;
}

SCM markcdr(ptr)
     SCM ptr;
{
  return CDR(ptr);
}
sizet free0(ptr)
     CELLPTR ptr;
{
  return 0;
}
SCM equal0(ptr1, ptr2)
     SCM ptr1, ptr2;
{
  return (CDR(ptr1)==CDR(ptr2)) ? BOOL_T : BOOL_F;
}

static char remsg[] = "remove\n#define ", addmsg[] = "add\n#define ",
  rdmsg[] = "reduce";
void init_storage(stack_start_ptr, init_heap_size)
     STACKITEM *stack_start_ptr;
     long init_heap_size;
{
	sizet j;
	/* Because not all protects may get initialized */
	freelist = EOL;
	expmem = 0;
	estk_pool = EOL;
	scm_estk = BOOL_F;
	scm_port_table = 0;
	scm_port_table_len = 0;
	no_symhash_gc =
#ifdef NO_SYM_GC
	  !0	       /* Hobbit-compiled code must not GC symhash. */
#else
	  0
#endif
	  ;

#ifdef SHORT_SIZET
	if (sizeof(sizet) >= sizeof(long))
	  fixconfig(remsg, "SHORT_SIZET", 0);
#else
	if (sizeof(sizet) < sizeof(long))
	  fixconfig(addmsg, "SHORT_SIZET", 0);
#endif
#ifdef SHORT_INT
	if (sizeof(int) >= sizeof(long))
	  fixconfig(remsg, "SHORT_INT", 0);
#else
	if (sizeof(int) < sizeof(long))
	  fixconfig(addmsg, "SHORT_INT", 0);
#endif
#ifdef CDR_DOUBLES
	if (sizeof(double) != sizeof(long))
	  fixconfig(remsg, "CDR_DOUBLES", 0);
#else
# ifdef SINGLES
	if (sizeof(float) != sizeof(long)) {
	  if (sizeof(double) == sizeof(long))
	    fixconfig(addmsg, "CDR_DOUBLES", 0);
	  else
	    fixconfig(remsg, "SINGLES", 0);
	}
# endif
#endif
#ifdef BIGDIG
	if (2*BITSPERDIG/CHAR_BIT > sizeof(long))
	  fixconfig(remsg, "BIGDIG", 0);
# ifndef DIGSTOOBIG
	if (DIGSPERLONG*sizeof(BIGDIG) > sizeof(long))
	  fixconfig(addmsg, "DIGSTOOBIG", 0);
# endif
	if (NUMDIGS_MAX > (((unsigned long)-1L)>>16))
	  fixconfig(rdmsg, "NUMDIGS_MAX", 0);
#endif
#ifdef STACK_GROWS_UP
	if (((STACKITEM *)&j - stack_start_ptr) < 0)
	  fixconfig(remsg, "STACK_GROWS_UP", 1);
#else
	if ((stack_start_ptr - (STACKITEM *)&j) < 0)
	  fixconfig(addmsg, "STACK_GROWS_UP", 1);
#endif
	j = HEAP_SEG_SIZE;
	if (HEAP_SEG_SIZE != j)
	  fixconfig(rdmsg, "size of HEAP_SEG_SIZE", 0);

	mtrigger = INIT_MALLOC_LIMIT;
	mltrigger = mtrigger - MIN_MALLOC_YIELD;
	hplims = (CELLPTR *) must_malloc(2L*sizeof(CELLPTR), s_hplims);
	if (0L==init_heap_size) init_heap_size = INIT_HEAP_SIZE;
	j = init_heap_size;
/*  	printf("j = %u; init_heap_size = %lu\n", j, init_heap_size); */
	if ((init_heap_size != j) || !init_heap_seg((CELLPTR) malloc(j), j)) {
	  j = HEAP_SEG_SIZE;
/*  	  printf("j = %u; HEAP_SEG_SIZE = %lu\n", j, HEAP_SEG_SIZE); */
	  if (!init_heap_seg((CELLPTR) malloc(j), j))
	    wta(MAKINUM(j), (char *)NALLOC, s_heap);
	}
	else expmem = 1;
	heap_org = CELL_UP(hplims[0]);
		/* hplims[0] can change. do not remove heap_org */

	scm_port_table_len = 16;
	scm_port_table = (port_info *)
	  must_malloc((long)scm_port_table_len * sizeof(port_info), s_port_table);
	for (j = 0; j < scm_port_table_len; j++) {
	  scm_port_table[j].flags = 0L;
	  scm_port_table[j].data = UNDEFINED;
	  scm_port_table[j].port = UNDEFINED;
	}

	nullstr = must_malloc_cell(1L, MAKE_LENGTH(0, tc7_string), s_string);
	CHARS(nullstr)[0] = 0;
	nullvect = must_malloc_cell(1L, MAKE_LENGTH(0, tc7_vector), s_vector);
	{
	  long i = symhash_dim;
	  SCM *velts;
	  symhash = must_malloc_cell(i * sizeof(SCM),
				     MAKE_LENGTH(i, tc7_vector),
				     s_vector);
	  velts = VELTS(symhash);
	  while(--i >= 0) (velts)[i] = EOL;
	}
	/* Now that symhash is setup, we can sysintern() */
	sysintern("most-positive-fixnum", (SCM)MAKINUM(MOST_POSITIVE_FIXNUM));
	sysintern("most-negative-fixnum", (SCM)MAKINUM(MOST_NEGATIVE_FIXNUM));
#ifdef BIGDIG
	sysintern("bignum-radix", MAKINUM(BIGRAD));
#endif
	def_inp = scm_port_entry(stdin, tc16_fport, OPN|RDNG);
	SCM_PORTDATA(def_inp) = CAR(sysintern("stdin", UNDEFINED));
	def_outp = scm_port_entry(stdout, tc16_fport, OPN|WRTNG|TRACKED);
	SCM_PORTDATA(def_outp) = CAR(sysintern("stdout", UNDEFINED));
	NEWCELL(def_errp);
	CAR(def_errp) = (tc16_fport|OPN|WRTNG);
	SETSTREAM(def_errp, stderr);
	cur_inp = def_inp;
	cur_outp = def_outp;
	cur_errp = def_errp;
	NEWCELL(sys_errp);
	CAR(sys_errp) = (tc16_sysport|OPN|WRTNG);
	SETSTREAM(sys_errp, 0);
	sys_safep = mksafeport(0, def_errp);
	dynwinds = EOL;
	NEWCELL(rootcont);
	SETCONT(rootcont, make_root_continuation(stack_start_ptr));
	CAR(rootcont) = tc7_contin;
	CONT(rootcont)->other.dynenv = EOL;
	CONT(rootcont)->other.parent = BOOL_F;
	listofnull = cons(EOL, EOL);
	undefineds = cons(UNDEFINED, EOL);
	CDR(undefineds) = undefineds;
	/* flo0 is now setup in scl.c */
	/* Set up environment cache */
	scm_ecache_len = sizeof(ecache_v)/sizeof(cell);
	scm_ecache = CELL_UP(ecache_v);
	scm_ecache_len = CELL_DN(ecache_v + scm_ecache_len - 1) - scm_ecache + 1;
	scm_ecache_index = scm_ecache_len;
	scm_egc_root_index = sizeof(scm_egc_roots)/sizeof(SCM);
	scm_estk = BOOL_F;
	scm_estk_reset(0);
}

/* The way of garbage collecting which allows use of the cstack is due to */
/* Scheme In One Defun, but in C this time.

 *			  COPYRIGHT (c) 1989 BY				    *
 *	  PARADIGM ASSOCIATES INCORPORATED, CAMBRIDGE, MASSACHUSETTS.	    *
 *			   ALL RIGHTS RESERVED				    *

Permission to use, copy, modify, distribute and sell this software
and its documentation for any purpose and without fee is hereby
granted, provided that the above copyright notice appear in all copies
and that both that copyright notice and this permission notice appear
in supporting documentation, and that the name of Paradigm Associates
Inc not be used in advertising or publicity pertaining to distribution
of the software without specific, written prior permission.

PARADIGM DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
PARADIGM BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
SOFTWARE.

gjc@paradigm.com

Paradigm Associates Inc		 Phone: 617-492-6079
29 Putnam Ave, Suite 6
Cambridge, MA 02138
*/
char s_cells[] = "cells";
SCM gc_for_newcell()
{
	SCM fl;
	int oints = ints_disabled; /* Temporary expedient */
	if (!oints) ints_disabled = 1;
	igc(s_cells, rootcont);
	if ((gc_cells_collected < MIN_GC_YIELD) || IMP(freelist)) {
	  alloc_some_heap();
	  growth_mon(s_numheaps, (long)(hplim_ind/2), s_segs, !0);
	  growth_mon(s_heap, heap_cells, s_cells, !0);
	}
	++cells_allocated;
	fl = freelist;
	freelist = CDR(fl);
	ints_disabled = oints;
	return fl;
}

void gc_for_open_files()
{
  igc("open files", rootcont);
}

void scm_fill_freelist()
{
  while IMP(freelist) {
    igc(s_cells, rootcont);
    if ((gc_cells_collected < MIN_GC_YIELD) || IMP(freelist)) {
      alloc_some_heap();
      growth_mon(s_numheaps, (long)(hplim_ind/2), s_segs, !0);
      growth_mon(s_heap, heap_cells, s_cells, !0);
    }
  }
}

static char	s_bad_type[] = "unknown type in ";
void mark_locations P((STACKITEM x[], sizet n));
static void mark_syms P((SCM v));
static void mark_sym_values P((SCM v));
static void mark_subrs P((void));
static void sweep_symhash P((SCM v));
static void mark_finalizers P((SCM *live, SCM *dead));
static void mark_port_table P((SCM port));
static void sweep_port_table P((void));
static void egc_mark P((void));
static void egc_sweep P((void));

SCM gc(arg)
     SCM arg;
{
  DEFER_INTS;
  if (UNBNDP(arg))
    igc("call", rootcont);
  else
    scm_egc();
  ALLOW_INTS;
  return UNSPECIFIED;
}

void scm_run_finalizers(exiting)
     int exiting;
{
  SCM f;
  if (exiting) {		/* run all finalizers, we're going home. */
    DEFER_INTS;
    while NIMP(gc_finalizers) {
      f = CAR(gc_finalizers);
      CAR(f) = CDR(f);
      CDR(f) = gc_finalizers_pending;
      gc_finalizers_pending = f;
      gc_finalizers = CDR(gc_finalizers);
    }
    ALLOW_INTS;
  }
  while (!0) {
    DEFER_INTS;
    if (NIMP(gc_finalizers_pending)) {
      f = CAR(gc_finalizers_pending);
      gc_finalizers_pending = CDR(gc_finalizers_pending);
    }
    else f = BOOL_F;
    ALLOW_INTS;
    if (IMP(f)) break;
    apply(f, EOL, EOL);
  }
}

static SCM *loc_gc_hook = 0;
void scm_gc_hook ()
{
  if (gc_hook_active) {
    scm_warn("gc-hook thrashing?\n", "", UNDEFINED);
    return;
  }
  gc_hook_active = !0;
  if (! loc_gc_hook) loc_gc_hook = &CDR(sysintern("gc-hook", UNDEFINED));
  if (NIMP(*loc_gc_hook)) apply(*loc_gc_hook, EOL, EOL);
  scm_run_finalizers(0);
  gc_hook_active = 0;
}

void igc(what, basecont)
     const char *what;
     SCM basecont;
{
  int j = num_protects;
  long oheap_cells = heap_cells;
  STACKITEM * stackbase = IMP(basecont) ? 0 : CONT(basecont)->stkbse;
#ifdef DEBUG_GMALLOC
  int err = check_frag_blocks();
  if (err) wta(MAKINUM(err), "malloc corrupted", what);
#endif
  gc_start(what);
  if (errjmp_bad) wta(UNDEFINED, s_recursive, s_gc);
  errjmp_bad = s_gc;
  if (no_symhash_gc)		/* Hobbit-compiled code needs this. */
    gc_mark(symhash);
  else {
    /* By marking symhash first, we provide the best immunity from
       accidental references.  In order to accidentally protect a
       symbol, a pointer will have to point directly at the symbol (as
       opposed to the vector or bucket lists).  */
    mark_syms(symhash);
    /* mark_sym_values() can be called anytime after mark_syms.  */
    mark_sym_values(symhash);
  }
  mark_subrs();
  egc_mark();
  if (stackbase) {
#ifdef __ia64__
    mark_regs_ia64(CONT(basecont));
#else
    jump_buf save_regs_gc_mark;
    FLUSH_REGISTER_WINDOWS;
    /* This assumes that all registers are saved into the jump_buf */
    setjump(save_regs_gc_mark);
    mark_locations((STACKITEM *) save_regs_gc_mark,
		   (sizet) (sizeof(STACKITEM) - 1 + sizeof save_regs_gc_mark) /
		   sizeof(STACKITEM));
    {
      /* stack_len is long rather than sizet in order to guarantee that
	 &stack_len is long aligned */
# ifdef STACK_GROWS_UP
#  ifdef nosve
      long stack_len = (STACKITEM *)(&stack_len) - stackbase;
#  else
      long stack_len = stack_size(stackbase);
#  endif
      mark_locations(stackbase, (sizet)stack_len);
# else
#  ifdef nosve
      long stack_len = stackbase - (STACKITEM *)(&stack_len);
#  else
      long stack_len = stack_size(stackbase);
#  endif
      mark_locations((stackbase - stack_len), (sizet)stack_len);
# endif
    }
#endif
  }
  while(j--)
    gc_mark(sys_protects[j]);
  mark_finalizers(&gc_finalizers, &gc_finalizers_pending);
  if (!no_symhash_gc)		/* if not Hobbit-compiled code. */
    sweep_symhash(symhash);
  gc_sweep(!stackbase);
  sweep_port_table();
  egc_sweep();
  estk_pool = EOL;
  errjmp_bad = (char *)0;
  gc_end();
  if (oheap_cells != heap_cells) {
    int grewp = heap_cells > oheap_cells;
    growth_mon(s_numheaps, (long)(hplim_ind/2), s_segs, grewp);
    growth_mon(s_heap, heap_cells, s_cells, grewp);
  }
  gc_hook_pending = !0;
  deferred_proc = process_signals;
}

static char s_not_free[] = "not freed";
void free_storage()
{
  DEFER_INTS;
  loc_gc_hook = (SCM *)0;
  gc_start("free");
  errjmp_bad = "free_storage";
  cur_inp = BOOL_F; cur_outp = BOOL_F;
  cur_errp = tmp_errp; sys_errp = tmp_errp;
  gc_mark(def_inp);		/* don't want to close stdin */
  gc_mark(def_outp);		/* don't want to close stdout */
  gc_mark(def_errp);		/* don't want to close stderr */
  gc_sweep(0);
  rootcont = BOOL_F;
  while (hplim_ind) {		/* free heap segments */
    hplim_ind -= 2;
    {
      CELLPTR ptr = CELL_UP(hplims[hplim_ind]);
      sizet seg_cells = CELL_DN(hplims[hplim_ind+1]) - ptr;
      heap_cells -= seg_cells;
      free((char *)hplims[hplim_ind]);
      hplims[hplim_ind] = 0;
      /* At this point, sys_errp is no longer valid */
      /* growth_mon(s_heap, heap_cells, s_cells, 0); fflush(stderr); */
    }}
  if (heap_cells) wta(MAKINUM(heap_cells), s_not_free, s_heap);
  if (hplim_ind) wta((SCM)MAKINUM(hplim_ind), s_not_free, s_hplims);
  /* Not all cells get freed (see gc_mark() calls above). */
  /* if (cells_allocated) wta(MAKINUM(cells_allocated), s_not_free, "cells"); */
  /* either there is a small memory leak or I am counting wrong. */
  must_free((char *)hplims, 0);
  /* if (mallocated) wta(MAKINUM(mallocated), s_not_free, "malloc"); */
  hplims = 0;
  scm_free_gra(&finals_gra);
  scm_free_gra(&smobs_gra);
  scm_free_gra(&subrs_gra);
  /* gc_end(); */
  /* ALLOW_INTS; */ /* A really bad idea, but printing does it anyway. */
  /* exit_report(); */
  /* lfflush(sys_errp); */	/* This causes segfault in fc9 */
  scm_free_gra(&ptobs_gra);
  lmallocated = mallocated = 0;
  /* Can't do gc_end() here because it uses ptobs which have been freed */
  fflush(stdout);		/* in lieu of close */
  fflush(stderr);		/* in lieu of close */
}

#define HUGE_LENGTH(x) (LENGTH_MAX==LENGTH(x) ? *((unsigned long *)VELTS(x)) : LENGTH(x))

/* This is used to force allocation of SCM temporaries on the stack,
   it should be called with any SCM variables used for malloc headers
   and entirely local to a C procedure.  */
void scm_protect_temp(ptr)
     SCM *ptr;
{
  return;
}

static char s_gc_sym[] = "mark_syms", s_wrong_length[] = "wrong length";
void gc_mark(p)
     SCM p;
{
  register long i;
  register SCM ptr = p;
  CHECK_STACK;
 gc_mark_loop:
  if (IMP(ptr)) return;
 gc_mark_nimp:
  if (NCELLP(ptr)
      /* #ifndef RECKLESS */
      /* || PTR_GT(hplims[0], (CELLPTR)ptr) */
      /* || PTR_GE((CELLPTR)ptr, hplims[hplim_ind-1]) */
      /* #endif */
      ) wta(ptr, "rogue pointer in ", s_heap);
  switch TYP7(ptr) {
  case tcs_cons_nimcar:
    if (GCMARKP(ptr)) break;
    SETGCMARK(ptr);
    if (IMP(CDR(ptr))	/* IMP works even with a GC mark */
	|| (CONSP(GCCDR(ptr)) && GCMARKP(GCCDR(ptr)))
	) {
      ptr = CAR(ptr);
      goto gc_mark_nimp;
    }
    gc_mark(CAR(ptr));
    ptr = GCCDR(ptr);
    goto gc_mark_nimp;
  case tcs_cons_imcar:
  case tcs_cons_gloc:
    if (GCMARKP(ptr)) break;
    SETGCMARK(ptr);
    ptr = GCCDR(ptr);
    goto gc_mark_loop;
  case tcs_closures:
    if (GCMARKP(ptr)) break;
    SETGCMARK(ptr);
    if (IMP(GCENV(ptr))) {
      ptr = CODE(ptr);
      goto gc_mark_nimp;
    }
    gc_mark(CODE(ptr));
    ptr = GCENV(ptr);
    goto gc_mark_nimp;
  case tc7_specfun:
    if (GC8MARKP(ptr)) break;
    SETGC8MARK(ptr);
#ifdef CCLO
    if (tc16_cclo==GCTYP16(ptr)) {
      i = CCLO_LENGTH(ptr);
      if (i==0) break;
      while(--i>0) if (NIMP(VELTS(ptr)[i])) gc_mark(VELTS(ptr)[i]);
      ptr = VELTS(ptr)[0];
    }
    else
#endif
      ptr = CDR(ptr);
    goto gc_mark_loop;
  case tc7_vector:
    if (GC8MARKP(ptr)) break;
    SETGC8MARK(ptr);
    i = LENGTH(ptr);
    if (i==0) break;
    while(--i>0) if (NIMP(VELTS(ptr)[i])) gc_mark(VELTS(ptr)[i]);
    ptr = VELTS(ptr)[0];
    goto gc_mark_loop;
  case tc7_contin:
    if (GC8MARKP(ptr)) break;
    SETGC8MARK(ptr);
    mark_locations((STACKITEM *)VELTS(ptr),
		   (sizet)(LENGTH(ptr) +
			   (sizeof(STACKITEM) - 1 + sizeof(CONTINUATION)) /
			   sizeof(STACKITEM)));
    break;
  case tc7_string:
  case tc7_msymbol:
    if (GC8MARKP(ptr)) break;
    ASRTER(!(CHARS(ptr)[HUGE_LENGTH(ptr)]), MAKINUM(HUGE_LENGTH(ptr)),
	   s_wrong_length, s_gc);
  case tc7_ssymbol:
  case tc7_VfixN8: case tc7_VfixZ8: case tc7_VfixZ16: case tc7_VfixN16:
  case tc7_VfixZ32: case tc7_VfixN32: case tc7_VfixZ64: case tc7_VfixN64:
  case tc7_VfloR32: case tc7_VfloC32: case tc7_VfloR64: case tc7_VfloC64:
  case tc7_Vbool:
    SETGC8MARK(ptr);
  case tcs_subrs:
    break;
  case tc7_port:
    if (GC8MARKP(ptr)) break;
    SETGC8MARK(ptr);
    i = PTOBNUM(ptr);
    if (!(i < numptob)) goto def;
    mark_port_table(ptr);
    if (!ptobs[i].mark) break;
    ptr = (ptobs[i].mark)(ptr);
    goto gc_mark_loop;
  case tc7_smob:
    if (GC8MARKP(ptr)) break;
    SETGC8MARK(ptr);
    switch TYP16(ptr) {		/* should be faster than going through smobs */
    case tc_free_cell:
      /* printf("found free_cell %X ", ptr); fflush(stdout); */
      ASRTER(tc_broken_heart!=CAR(ptr), ptr, "found ecache forward", s_gc);
      /*      CDR(ptr) = UNDEFINED */;
      break;
#ifdef BIGDIG
    case tcs_bignums:
      break;
#endif
#ifdef FLOATS
    case tc16_flo:
      break;
#endif
    default:
      i = SMOBNUM(ptr);
      if (!(i < numsmob)) goto def;
      SETGC8MARK(ptr);
      if (!smobs[i].mark)  break;
      ptr = (smobs[i].mark)(ptr);
      goto gc_mark_loop;
    }
    break;
  default: def: wta(ptr, s_bad_type, "gc_mark");
  }
}

/* mark_locations() marks a location pointed to by x[0:n] only if
   `x[m]' is cell-aligned and points into a valid heap segment.  This
   code is duplicated by obunhash() in "sys.c" and scm_cell_p() in
   "rope.c", which means that changes to these routines must be
   coordinated. */

void mark_locations(x, n)
     STACKITEM x[];
     sizet n;
{
	register long m = n;
	register int i, j;
	register CELLPTR ptr;
	while(0 <= --m) if (CELLP(*(SCM **)&x[m])) {
		ptr = (CELLPTR)SCM2PTR((SCM)(*(SCM **)&x[m]));
		i = 0;
		j = hplim_ind;
		do {
			if (PTR_GT(hplims[i++], ptr)) break;
			if (PTR_LE(hplims[--j], ptr)) break;
			if ((i != j)
			    && PTR_LE(hplims[i++], ptr)
			    && PTR_GT(hplims[--j], ptr)) continue;
			/* if (NFREEP(*(SCM **)&x[m])) */ gc_mark(*(SCM *)&x[m]);
			break;
		} while(i<j);
	}
}

static void gc_sweep(contin_bad)
     int contin_bad;
{
  register CELLPTR ptr;
#ifdef POINTERS_MUNGED
  register SCM scmptr;
#else
# define scmptr (SCM)ptr
#endif
  register SCM nfreelist = EOL;
  register long n = 0;
  register sizet j, minc;
  unsigned long pre_m = mallocated;
  sizet i = 0;
  sizet seg_cells;
  while (i < hplim_ind) {
    ptr = CELL_UP(hplims[i++]);
    seg_cells = CELL_DN(hplims[i++]) - ptr;
    for (j = seg_cells; j--; ++ptr) {
#ifdef POINTERS_MUNGED
      scmptr = PTR2SCM(ptr);
#endif
      switch TYP7(scmptr) {
      case tcs_cons_imcar:
      case tcs_cons_nimcar:
      case tcs_cons_gloc:
      case tcs_closures:
	if (GCMARKP(scmptr)) goto cmrkcontinue;
	break;
      case tc7_specfun:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
#ifdef CCLO
	if (tc16_cclo==GCTYP16(scmptr)) {
	  minc = (CCLO_LENGTH(scmptr)*sizeof(SCM));
	  goto freechars;
	}
#endif
	break;
      case tc7_vector:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = (LENGTH(scmptr)*sizeof(SCM));
      freechars:
	must_free(CHARS(scmptr), minc);
/*	SETCHARS(scmptr, 0);*/
	break;
      case tc7_Vbool:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = sizeof(long)*((HUGE_LENGTH(scmptr)+LONG_BIT-1)/LONG_BIT);
	goto freechars;
      case tc7_VfixZ32:
      case tc7_VfixN32:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)*sizeof(long);
	goto freechars;
      case tc7_VfixN8:
      case tc7_VfixZ8:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)*sizeof(char);
	goto freechars;
      case tc7_VfixZ16:
      case tc7_VfixN16:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)*sizeof(short);
	goto freechars;
      case tc7_VfloR32:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)*sizeof(float);
	goto freechars;
      case tc7_VfloC32:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)*2*sizeof(float);
	goto freechars;
      case tc7_VfloR64:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)*sizeof(double);
	goto freechars;
      case tc7_VfloC64:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)*2*sizeof(double);
	goto freechars;
      case tc7_string:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = HUGE_LENGTH(scmptr)+1;
	goto freechars;
      case tc7_msymbol:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	minc = LENGTH(scmptr)+1;
	goto freechars;
      case tc7_contin:
	if (GC8MARKP(scmptr)) {
	  if (contin_bad && CONT(scmptr)->length)
	    scm_warn("uncollected ", "", scmptr);
	  goto c8mrkcontinue;
	}
	minc = LENGTH(scmptr)*sizeof(STACKITEM) + sizeof(CONTINUATION);
	mallocated = mallocated - minc;
	free_continuation(CONT(scmptr)); break; /* goto freechars; */
      case tc7_ssymbol:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	/* Do not free storage because tc7_ssymbol means scmptr's
           storage was not created by a call to malloc(). */
	break;
      case tcs_subrs:
	continue;
      case tc7_port:
	if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	if (OPENP(scmptr)) {
	  int k = PTOBNUM(scmptr);
	  if (!(k < numptob)) goto sweeperr;
				/* Yes, I really do mean ptobs[k].free */
				/* rather than ptobs[k].close.  .close */
				/* is for explicit CLOSE-PORT by user */
	  (ptobs[k].free)(STREAM(scmptr));
	  gc_ports_collected++;
	  SETSTREAM(scmptr, 0);
	  CAR(scmptr) &= ~OPN;
	}
	break;
      case tc7_smob:
	switch GCTYP16(scmptr) {
	case tc_free_cell:
	  if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	  break;
#ifdef BIGDIG
	case tcs_bignums:
	  if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	  minc = (NUMDIGS(scmptr)*sizeof(BIGDIG));
	  goto freechars;
#endif /* def BIGDIG */
#ifdef FLOATS
	case tc16_flo:
	  if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	  switch ((int)(CAR(scmptr)>>16)) {
	  case (IMAG_PART | REAL_PART)>>16:
	    minc = 2*sizeof(double);
	    goto freechars;
	  case REAL_PART>>16:
	  case IMAG_PART>>16:
	    minc = sizeof(double);
	    goto freechars;
	  case 0:
	    break;
	  default:
	    goto sweeperr;
	  }
	  break;
#endif /* def FLOATS */
	default:
	  if (GC8MARKP(scmptr)) goto c8mrkcontinue;
	  {
	    int k = SMOBNUM(scmptr);
	    if (!(k < numsmob)) goto sweeperr;
	    minc = (smobs[k].free)((CELLPTR)scmptr);
	  }
	}
	break;
      default: sweeperr: wta(scmptr, s_bad_type, "gc_sweep");
      }
      ++n;
      CAR(scmptr) = (SCM)tc_free_cell;
      CDR(scmptr) = nfreelist;
      nfreelist = scmptr;
      continue;
    c8mrkcontinue:
      CLRGC8MARK(scmptr);
      continue;
    cmrkcontinue:
      CLRGCMARK(scmptr);
    }
#ifdef GC_FREE_SEGMENTS
    if (n==seg_cells) {
      heap_cells -= seg_cells;
      n = 0;
      free((char *)hplims[i-2]);
      /*      must_free((char *)hplims[i-2],
		sizeof(cell) * (hplims[i-1] - hplims[i-2])); */
      hplims[i-2] = 0;
      for (j = i;j < hplim_ind;j++) hplims[j-2] = hplims[j];
      hplim_ind -= 2;
      i -= 2;			/* need to scan segment just moved. */
      nfreelist = freelist;
    }
    else
#endif /* ifdef GC_FREE_SEGMENTS */
	freelist = nfreelist;
    gc_cells_collected += n;
    n = 0;
  }
  lcells_allocated += (heap_cells - gc_cells_collected - cells_allocated);
  cells_allocated = (heap_cells - gc_cells_collected);
  gc_malloc_collected = (pre_m - mallocated);
  lmallocated = lmallocated - gc_malloc_collected;
}

/* mark_syms marks those symbols of hash table V which have
   non-UNDEFINED values.  */
static void mark_syms(v)
     SCM v;
{
  SCM x, al;
  int k = LENGTH(v);
  while (k--)
    for (al = VELTS(v)[k]; NIMP(al); al = GCCDR(al)) {
      /* If this bucket has already been marked, then something is wrong.  */
      ASRTER(!GCMARKP(al), al, s_bad_type, s_gc_sym);
      x = CAR(al);
      SETGCMARK(al);		/* Do mark bucket list */
# ifdef CAREFUL_INTS
      ASRTER(NIMP(x) && NIMP(CAR(x)) && !GCMARKP(x), x, s_bad_type, s_gc_sym);
      ASRTER(!GC8MARKP(CAR(x)) && !(CHARS(CAR(x))[LENGTH(CAR(x))]),
	     CAR(x), s_wrong_length, s_gc_sym);
      ASRTER(strhash(UCHARS(CAR(x)), (sizet)LENGTH(CAR(x)),
		     (unsigned long)symhash_dim)==k,
	     CAR(x), "bad hash", s_gc_sym);
# endif
      if (UNDEFINED==CDR(x) && tc7_msymbol==TYP7(CAR(x)))
	goto used;		/* Don't mark symbol.  */
      SETGC8MARK(CAR(x));
    used:
      /* SETGCMARK(x) */;	/* Don't mark value cell.  */
      /* We used to mark the value cell, but value cells get returned
	 by calls to intern().  This caused a rare GC leak which only
	 showed up in large programs. */
    }
  SETGC8MARK(v);		/* Mark bucket vector.  */
}

/* mark_symhash marks the values of hash table V.  */
static void mark_sym_values(v)
     SCM v;
{
  SCM x, al;
  int k = LENGTH(v);
  /* SETGC8MARK(v); */		/* already set by mark_syms */
  while (k--)
    for (al = VELTS(v)[k]; NIMP(al); al = GCCDR(al)) {
      x = GCCDR(CAR(al));
      if (IMP(x)) continue;
      gc_mark(x);
    }
}

/* Splice any unused valueless symbols out of the hash buckets. */
static void sweep_symhash(v)
     SCM v;
{
  SCM al, x, *lloc;
  int k = LENGTH(v);
  while (k--) {
    lloc = &(VELTS(v)[k]);
    while NIMP(al = (*lloc & ~1L)) {
      x = CAR(al);
      if (GC8MARKP(CAR(x))) {
	lloc = &(CDR(al));
	SETGCMARK(x);
      }
      else {
	*lloc = CDR(al);
	CLRGCMARK(al);		/* bucket pair to be collected by gc_sweep */
	CLRGCMARK(x);		/* value cell to be collected by gc_sweep */
	gc_syms_collected++;
      }
    }
    VELTS(v)[k] &= ~1L;		/* We may have deleted the first cell */
  }
}

/* This function should be called after all other marking is done. */
static void mark_finalizers(finalizers, pending)
     SCM *finalizers, *pending;
{
  SCM lst, elt, v;
  SCM live = EOL, undead = *finalizers;
  int more_to_do = !0;
  gc_mark(*pending);
  while NIMP(*pending) pending = &CDR(*pending);
  while (more_to_do) {
    more_to_do = 0;
    lst = undead;
    undead = EOL;
    while (NIMP(lst)) {
      elt = CAR(lst);
      v = CAR(elt);
      switch (TYP3(v)) {
      default:
	if (GCMARKP(v)) goto marked;
	goto unmarked;
      case tc3_tc7_types:
	if (GC8MARKP(v)) {
	marked:
	  gc_mark(CDR(elt));
	  more_to_do = !0;
	  v = lst;
	  lst = CDR(lst);
	  CDR(v) = live;
	  live = v;
	}
	else {
	unmarked:
	  v = lst;
	  lst = CDR(lst);
	  CDR(v) = undead;
	  undead = v;
	}
	break;
      }
    }
  }
  gc_mark(live);
  for (lst = undead; NIMP(lst); lst = CDR(lst))
    CAR(lst) = CDR(CAR(lst));
  gc_mark(undead);
  *finalizers = live;
  *pending = undead;
}

static void mark_subrs()
{
  /* subr_info *table = subrs; */
  /* int k = subrs_gra.len; */
  /* while (k--) { } */
}
static void mark_port_table(port)
     SCM port;
{
  int i = SCM_PORTNUM(port);
  ASRTER(i>=0 && i<scm_port_table_len, MAKINUM(i), "bad port", s_gc);
  if (i) {
    scm_port_table[i].flags |= 1;
    if (NIMP(scm_port_table[i].data))
      gc_mark(scm_port_table[i].data);
  }
}
static void sweep_port_table()
{
  int k;
	/* tmp_errp gets entry 0, so we never clear its flags. */
  for (k = scm_port_table_len - 1; k > 0; k--) {
    if (scm_port_table[k].flags & 1)
      scm_port_table[k].flags &= (~1L);
    else {
      scm_port_table[k].flags = 0L;
      scm_port_table[k].data = UNDEFINED;
      scm_port_table[k].port = UNDEFINED;
    }
  }
}

/* Environment cache GC routines */
/* This is called during a non-cache gc. We only mark those stack frames
   that are in use. */
static void egc_mark()
{
  SCM *v;
  int i;
  gc_mark(scm_env);
  gc_mark(scm_env_tmp);
  if (IMP(scm_estk)) return;	/* Can happen when moving estk. */
  if (GC8MARKP(scm_estk)) return;
  v = VELTS(scm_estk);
  SETGC8MARK(scm_estk);
  i = scm_estk_ptr - v + SCM_ESTK_FRLEN;
  while(--i >= 0)
    if (NIMP(v[i]))
      gc_mark(v[i]);
}
static void egc_sweep()
{
  SCM z;
  int i;
  for (i = scm_ecache_index; i < scm_ecache_len; i++) {
    z = PTR2SCM(&(scm_ecache[i]));
    if (CONSP(z)) {
      CLRGCMARK(z);
    }
    else {
      CLRGC8MARK(z);
    }
  }
  /* Under some circumstances I don't fully understand, continuations may
     point to dead ecache cells. This prevents gc marked cells from causing
     errors during ecache gc. */
  for (i = scm_ecache_index; i--;) {
    scm_ecache[i].car = UNSPECIFIED;
    scm_ecache[i].cdr = UNSPECIFIED;
  }
}

#define ECACHEP(x) (PTR_LE((CELLPTR)(ecache_v), (CELLPTR)SCM2PTR(x)) && \
		    PTR_GT((CELLPTR)(ecache_v) + ECACHE_SIZE, (CELLPTR)SCM2PTR(x)))
static void egc_copy(px)
     SCM *px;
{
  SCM z, x = *px;
  do {
    if (tc_broken_heart==CAR(x)) {
      *px = CDR(x);
      return;
    }
    if (IMP(freelist)) wta(freelist, "empty freelist", "ecache gc");
    z = freelist;
    freelist = CDR(freelist);
    ++cells_allocated;
    CAR(z) = CAR(x);
    CDR(z) = CDR(x);
    CAR(x) = (SCM)tc_broken_heart;
    CDR(x) = z;
    *px = z;
    x = CAR(z);
    if (NIMP(x) && ECACHEP(x))
      egc_copy(&(CAR(z)));
    px = &(CDR(z));
    x = *px;
  } while (NIMP(x) && ECACHEP(x));
}

static void egc_copy_locations(ve, len)
     SCM *ve;
     sizet len;
{
  SCM x;
  while (len--) {
    x = ve[len];
    if (NIMP(x) && ECACHEP(x)) {
      if (tc_broken_heart==CAR(x))
	ve[len] = CDR(x);
      else
	egc_copy(&(ve[len]));
    }
  }
}
static void egc_copy_stack(stk, len)
     SCM stk;
     sizet len;
{
  while (!0) {
    egc_copy_locations(VELTS(stk), len);
    len = INUM(SCM_ESTK_PARENT_INDEX(stk)) + SCM_ESTK_FRLEN;
    stk =SCM_ESTK_PARENT(stk);
    if (IMP(stk)) return;
    /*    len = LENGTH(stk); */
  }
}
extern long tc16_env, tc16_promise;
static void egc_copy_roots()
{
  SCM *roots = &(scm_egc_roots[scm_egc_root_index]);
  SCM e, x;
  int len = sizeof(scm_egc_roots)/sizeof(SCM) - scm_egc_root_index ;
  if (!(len>=0 && len <= sizeof(scm_egc_roots)/sizeof(SCM)))
    wta(MAKINUM(scm_egc_root_index), "egc-root-index", "corrupted");
  while (len--) {
    x = roots[len];
    if (IMP(x)) continue;
    switch TYP3(x) {
    clo:
    case tc3_closure:
      e = ENV(x);
      if (NIMP(e) && ECACHEP(e)) {
	egc_copy(&e);
	CDR(x) = (6L & CDR(x)) | e;
      }
      break;
    case tc3_cons_imcar:
    case tc3_cons_nimcar:	/* These are environment frames that have
				   been destructively altered by DEFINE or
				   LETREC.  This is only a problem if a
				   non-cache cell was made to point into the
				   cache. */
      if (ECACHEP(x)) break;
      e = CAR(x);
      if (NIMP(e) && ECACHEP(e))
	egc_copy(&(CAR(x)));
      break;
    default:
      if (tc7_contin==TYP7(x)) {
	egc_copy_locations(CONT(x)->other.stkframe, 2);
#ifndef CHEAP_CONTINUATIONS
	x = CONT(x)->other.estk;
	egc_copy_stack(x, LENGTH(x));
#endif
	break;
      }
      if (tc16_env==CAR(x)) {
	e = CDR(x);
	if (NIMP(e) && ECACHEP(e))
	  egc_copy(&(CDR(x)));
	break;
      }
      if (tc16_promise==CAR(x)) {
	x = CDR(x);
	goto clo;
      }
    }
  }
  scm_egc_root_index = sizeof(scm_egc_roots)/sizeof(SCM);
}
extern long scm_stk_moved, scm_clo_moved, scm_env_work;
static int egc_need_gc()
{
  SCM fl = freelist;
  int n;
  if (heap_cells - cells_allocated <= scm_ecache_len)
    return 1;
 /* Interrupting a NEWCELL could leave cells_allocated inconsistent with
    freelist, see handle_it() in repl.c */
  for (n = 4; n; n--) {
    if (IMP(fl)) return 1;
    fl = CDR(fl);
  }
  return 0;
}
void scm_egc()
{
  VERIFY_INTS("scm_egc", 0L);
/* We need to make sure there are enough cells available to migrate
   the entire environment cache, gc does not work properly during ecache gc */
  while (egc_need_gc()) {
    igc("ecache", rootcont);
    if ((gc_cells_collected < MIN_GC_YIELD) ||
	(heap_cells - cells_allocated <= scm_ecache_len) || IMP(freelist)) {
      alloc_some_heap();
      growth_mon("number of heaps", (long)(hplim_ind/2), "segments", !0);
      growth_mon(s_heap, heap_cells, s_cells, !0);
    }
  }
  if (errjmp_bad)
    wta(UNDEFINED, s_recursive, s_cache_gc);
  {
    SCM stkframe[2];
    long lcells = cells_allocated;
    sizet nstk = (scm_estk_ptr - VELTS(scm_estk) + SCM_ESTK_FRLEN);
    ASRTER(nstk<=LENGTH(scm_estk), UNDEFINED, "estk corrupted", s_cache_gc);
    scm_egc_start();
    stkframe[0] = scm_env;
    stkframe[1] = scm_env_tmp;
    egc_copy_roots();
    scm_clo_moved += cells_allocated - lcells;
    lcells = cells_allocated;
    egc_copy_locations(stkframe, sizeof(stkframe)/sizeof(SCM));
    egc_copy_stack(scm_estk, nstk);
    scm_env = stkframe[0];
    scm_env_tmp = stkframe[1];
    scm_stk_moved += cells_allocated - lcells;
    scm_ecache_index = scm_ecache_len;
    scm_env_work += scm_ecache_len;
    scm_egc_end();
  }
  errjmp_bad = (char *)0;
}
