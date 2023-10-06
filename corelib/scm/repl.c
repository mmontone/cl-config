/* "repl.c" error, read-eval-print loop, read, write and load code.
 * Copyright (C) 1990-2006 Free Software Foundation, Inc.
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
#include "setjump.h"
void	igc P((const char *what, SCM basecont));
void	unexec P((char *new_name, char *a_name, unsigned data_start,
		  unsigned bss_start, unsigned entry_address));
void	scm_fill_freelist P((void));

#ifdef __CYGWIN__
# include <sys/types.h>
#endif

#ifdef __NetBSD__
# include <ctype.h>
# include <unistd.h>
#endif

#ifdef __OpenBSD__
# include <ctype.h>
# include <unistd.h>
#endif

#ifdef PLAN9
# include <ctype.h>
#endif

#ifdef linux
# include <ctype.h>
#endif

#ifdef ARM_ULIB
# include <termio.h>
int set_erase()
{
   struct termio tin;

   ioctl(0, TCGETA, &tin);
   tin.c_cc[VERASE] = '\010';

   ioctl(0, TCSETA, &tin);
   return(0);
}
#endif

unsigned char upcase[CHAR_CODE_LIMIT];
unsigned char downcase[CHAR_CODE_LIMIT];
unsigned char lowers[] = "abcdefghijklmnopqrstuvwxyz";
unsigned char uppers[] = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
void init_tables()
{
  int i;
  for (i = 0;i<CHAR_CODE_LIMIT;i++) upcase[i] = downcase[i] = i;
  for (i = 0;i<sizeof lowers/sizeof(char);i++) {
    upcase[lowers[i]] = uppers[i];
    downcase[uppers[i]] = lowers[i];
  }
}

#ifdef EBCDIC
char *charnames[] = {
  "nul","soh","stx","etx", "pf", "ht", "lc","del",
   0   , 0   ,"smm", "vt", "ff", "cr", "so", "si",
  "dle","dc1","dc2","dc3","res", "nl", "bs", "il",
  "can", "em", "cc", 0   ,"ifs","igs","irs","ius",
   "ds","sos", "fs", 0   ,"byp", "lf","eob","pre",
   0   , 0   , "sm", 0   , 0   ,"enq","ack","bel",
   0   , 0   ,"syn", 0   , "pn", "rs", "uc","eot",
   0   , 0   , 0   , 0   ,"dc4","nak", 0   ,"sub",
  "space", s_newline, "tab", "backspace", "return", "page", "null"};
char charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
\040\041\042\043\044\045\046\047\
\050\051\052\053\054\055\056\057\
\060\061\062\063\064\065\066\067\
\070\071\072\073\074\075\076\077\
 \n\t\b\r\f\0";
#endif /* def EBCDIC */
#ifdef ASCII
char *charnames[] = {
  "nul","soh","stx","etx","eot","enq","ack","bel",
   "bs", "ht", "nl", "vt", "np", "cr", "so", "si",
  "dle","dc1","dc2","dc3","dc4","nak","syn","etb",
  "can", "em","sub","esc", "fs", "gs", "rs", "us",
  "space", s_newline, "tab", "backspace", "return", "page", "null", "del"};
char charnums[] =
"\000\001\002\003\004\005\006\007\
\010\011\012\013\014\015\016\017\
\020\021\022\023\024\025\026\027\
\030\031\032\033\034\035\036\037\
 \n\t\b\r\f\0\177";
#endif /* def ASCII */
char *isymnames[] = {
				/* Special Forms */
				/*  NUM_ISPCSYMS ISPCSYMS here */
  "#@and", "#@begin", "#@case", "#@cond", "#@do", "#@if", "#@lambda",
  "#@let", "#@let*", "#@letrec", "#@or", "#@quote", "#@set!",
  "#@funcall", "#@apply", "#@farloc-car", "#@farloc-cdr", "#@delay",
  "#@quasiquote", "#@eval-for-apply", "#@let-syntax", "#@acro-call",
  "#<line>",  "#@define", "#@eval-values",
  "#@unquote", "#@unquote-splicing", "#@else", "#@=>", "#@values-token",
  "#@keyword",
				/* user visible ISYMS */
				/* other keywords */
				/* Flags */
  "#f", "#t", "#<undefined>", "#<eof>", "()", "#<unspecified>"
  };

static char	s_read_char[] = "read-char", s_peek_char[] = "peek-char";
char	        s_write[] = "write", s_newline[] = "newline";
static char	s_display[] = "display", s_write_char[] = "write-char";
static char	s_freshline[] = "freshline";

static char	s_eofin[] = "end of file in ";
static char	s_unknown_sharp[] = "unknown # object";

static SCM scm_lread1 P((SCM port, int flgs, const char *what));
static SCM scm_lreadr P((SCM tok_buf, SCM port, int flgs));
static SCM scm_lreadpr P((SCM tok_buf, SCM port, int flgs));
static SCM scm_lreadparen P((SCM tok_buf, SCM port, int flgs, char *name));
static SCM scm_lread_rec P((SCM tok_buf, SCM port));
static sizet scm_read_token P((int ic, SCM tok_buf, SCM port, int flgs));
static void err_head P((char *str));
extern int case_sensitize_symbols; /* 0 or 8 */

void scm_intprint(n, radix, port)
     long n;
     int radix;
     SCM port;
{
  char num_buf[INTBUFLEN];
  lfwrite(num_buf, (sizet)sizeof(char), ilong2str(n, radix, num_buf), port);
}

void scm_ipruk(hdr, ptr, port)
     char *hdr;
     SCM ptr;
     SCM port;
{
  lputs("#<unknown-", port);
  lputs(hdr, port);
  if (scm_cell_p(ptr)) {
    lputs(" (0x", port);
    scm_intprint(CAR(ptr), -16, port);
    lputs(" . 0x", port);
    scm_intprint(CDR(ptr), -16, port);
    lputs(") @", port);
  }
  lputs(" 0x", port);
  scm_intprint(ptr, -16, port);
  lputc('>', port);
}

void scm_iprlist(hdr, exp, tlr, port, writing)
     char *hdr, tlr;
     SCM exp;
     SCM port;
     int writing;
{
  lputs(hdr, port);
  /* CHECK_INTS; */
  scm_iprin1(CAR(exp), port, writing);
  exp = GCCDR(exp); /* CDR(exp); */
  for (;NIMP(exp);exp = GCCDR(exp) /* CDR(exp)*/) {
    if (!scm_cell_p(~1L & exp)) break;
    if (NECONSP(exp)) break;
    lputc(' ', port);
    /* CHECK_INTS; */
    scm_iprin1(CAR(exp), port, writing);
  }
  if (NNULLP(exp)) {
    lputs(" . ", port);
    scm_iprin1(exp, port, writing);
  }
  lputc(tlr, port);
}
void scm_iprin1(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  register long i;
taloop:
  switch (7 & PTR2INT(exp)) {
  case 2:
  case 6:
    scm_intprint(INUM(exp), 10, port);
    break;
  case 4:
    if (ICHRP(exp)) {
      i = ICHR(exp);
      if (writing) lputs("#\\", port);
      if (!writing) lputc((int)i, port);
      else if ((i <= ' ') && charnames[i]) lputs(charnames[i], port);
#ifndef EBCDIC
      else if (i=='\177')
	lputs(charnames[(sizeof charnames/sizeof(char *))-1], port);
#endif /* ndef EBCDIC */
      else if (i > '\177')
	scm_intprint(i, -8, port);
      else lputc((int)i, port);
    }
    else if (SCM_LINUMP(exp)) {
      lputs("#<line ", port);
      scm_intprint(SCM_LINUM(exp), -10, port);
      lputc('>', port);
    }
    else if (IFLAGP(exp) && (ISYMNUM(exp)<(sizeof isymnames/sizeof(char *))))
      lputs(ISYMCHARS(exp), port);
    else if (ILOCP(exp)) {
      lputs("#@", port);
      scm_intprint((long)IFRAME(exp), -10, port);
      lputc(ICDRP(exp)?'-':'+', port);
      scm_intprint((long)IDIST(exp), -10, port);
    }
    else goto idef;
    break;
  case 1:			/* gloc */
    if (!scm_cell_p(exp-1)) {
      scm_ipruk("gloc", exp, port);
      break;
    }
    lputs("#@", port);
#ifdef _M_ARM
    /* MS CLARM compiler workaround */
    exp = CAR(MS_CLARM_dumy = exp - 1);
#else
    exp = CAR(exp-1);
#endif
    goto taloop;
  default:
  idef:
    scm_ipruk("immediate", exp, port);
    break;
  case 0:
    if (!scm_cell_p(exp)) {
      scm_ipruk("heap", exp, port);
      break;
    }
    switch TYP7(exp) {
    case (127 & IM_LET):
      if (CAR(exp) != IM_LET) {
	lputs("(#@call ", port);
	exp = CDR(exp);
	scm_iprin1(CAR(exp), port, writing);
	scm_iprlist(" ", CAR(CDR(exp)), ')', port, writing);
	break;
      }
      /* else fall through */
    case (127 & IM_AND): case (127 & IM_BEGIN): case (127 & IM_CASE):
    case (127 & IM_COND): case (127 & IM_DO): case (127 & IM_IF):
    case (127 & IM_LAMBDA): case (127 & IM_LETSTAR):
    case (127 & IM_LETREC): case (127 & IM_OR): case (127 & IM_QUOTE):
    case (127 & IM_SET): case (127 & IM_FUNCALL):
    case tcs_cons_inum:
    case tcs_cons_iloc:
    case tcs_cons_chflag:
    case tcs_cons_gloc:
    case tcs_cons_nimcar:
      scm_iprlist("(", exp, ')', port, writing);
      break;
    case tcs_closures:
      scm_princlosure(exp, port, writing);
      break;
    case tc7_string:
      if (writing) {
	lputc('\"', port);
	for (i = 0;i<LENGTH(exp);++i) switch (CHARS(exp)[i]) {
	case '\"':
	case '\\':
	  lputc('\\', port);
	default:
	  lputc(CHARS(exp)[i], port);
	}
	lputc('\"', port);
	break;
      }
    case tcs_symbols:
      if (writing) {		/* slashified symbol */
	for (i = 0;i<LENGTH(exp);++i) switch (CHARS(exp)[i]) {
	case 'A': case 'B': case 'C': case 'D': case 'E':
	case 'F': case 'G': case 'H': case 'I': case 'J':
	case 'K': case 'L': case 'M': case 'N': case 'O':
	case 'P': case 'Q': case 'R': case 'S': case 'T':
	case 'U': case 'V': case 'W': case 'X': case 'Y': case 'Z':
	  if (case_sensitize_symbols) goto skipit;
	case '\\': case '\"': case '\'': case '(': case ')': case '#':
	  lputc('\\', port);
	skipit:
	default:
	  lputc(CHARS(exp)[i], port);
	}
	break;
      }
      else
	lfwrite(CHARS(exp), (sizet)sizeof(char), (sizet)LENGTH(exp), port);
      break;
    case tc7_vector:
      lputs("#(", port);
      for (i = 0;i+1<LENGTH(exp);++i) {
	/* CHECK_INTS; */
	scm_iprin1(VELTS(exp)[i], port, writing);
	lputc(' ', port);
      }
      if (i<LENGTH(exp)) {
	/* CHECK_INTS; */
	scm_iprin1(VELTS(exp)[i], port, writing);
      }
      lputc(')', port);
      break;
    case tc7_VfixN8: case tc7_VfixZ8: case tc7_VfixN16: case tc7_VfixZ16:
    case tc7_VfixN32: case tc7_VfixZ32: case tc7_VfixN64: case tc7_VfixZ64:
    case tc7_VfloR32: case tc7_VfloC32: case tc7_VfloR64: case tc7_VfloC64:
    case tc7_Vbool: 
      raprin1(exp, port, writing);
      break;
    case tcs_subrs:
      lputs("#<primitive-procedure ", port);
      lputs(SNAME(exp), port);
      lputc('>', port);
      break;
    case tc7_specfun:
#ifdef CCLO
      if (tc16_cclo==TYP16(exp)) {
	lputs("#<compiled-closure ", port);
	scm_iprin1(CCLO_SUBR(exp), port, writing);
	lputc(' ', port);
	scm_iprin1(VELTS(exp)[1], port, writing);
	lputc('>', port);
	break;
      }
#endif
      lputs("#<primitive-procedure ", port);
      lputs(CHARS(CDR(exp)), port);
      lputc('>', port);
      break;
    case tc7_contin:
      lputs("#<continuation ", port);
      scm_intprint(LENGTH(exp), -10, port);
      lputs(" @ ", port);
      scm_intprint((long)CHARS(exp), -16, port);
      lputc('>', port);
      break;
    case tc7_port:
      i = PTOBNUM(exp);
      if (i<numptob) {
	if (ptobs[i].print && (ptobs[i].print)(exp, port, writing))
	  ;
	else
	  prinport(exp, port, ptobs[i].name ? ptobs[i].name : "unknown");
	break;
      }
      goto punk;
    case tc7_smob:
      i = SMOBNUM(exp);
      if (i<numsmob && smobs[i].print && (smobs[i].print)(exp, port, writing))
	break;
      goto punk;
    default: punk: scm_ipruk("type", exp, port);
    }
  }
}

static char s_char_readyp[]="char-ready?";

#ifdef __IBMC__
# define MSDOS
#endif
#ifdef MSDOS
# include <io.h>
# include <conio.h>
static int input_waiting(f)
     FILE *f;
{
  if (feof(f)) return 1;
  if (fileno(f)==fileno(stdin) && (isatty(fileno(stdin)))) return kbhit();
  return -1;
}
#else
# ifdef _DCC
#  include <ioctl.h>
# else
#  ifndef AMIGA
#   ifndef vms
#    ifdef MWC
#     include <sys/io.h>
#    else
#     ifndef macintosh
#      ifndef ARM_ULIB
#       ifndef PLAN9
#        include <sys/ioctl.h>
#       endif
#      endif
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef HAVE_SYS_TIME_H
#  include <sys/time.h>
# endif

static int input_waiting(f)
     FILE *f;
{
# ifdef HAVE_SELECT
  fd_set ifds;
  struct timeval tv;
  int ret;

  FD_ZERO(&ifds);
  FD_SET(fileno(f), &ifds);
  tv.tv_sec = 0;
  tv.tv_usec = 0;
  SYSCALL(ret = select((fileno(f) + 1), &ifds, (fd_set *) NULL,
		 (fd_set *) NULL, &tv););
  ASRTER(ret>=0, MAKINUM(ret), "select error", s_char_readyp);
  return FD_ISSET(fileno(f), &ifds);
# else
#  ifdef FIONREAD
  long remir;
  if (feof(f)) return 1;
  ioctl(fileno(f), FIONREAD, &remir);
  return remir;
#  else
  return -1;
#  endif
# endif
}
#endif
/* perhaps should undefine MSDOS from __IBMC__ here */
SCM char_readyp(port)
     SCM port;
{
  if (UNBNDP(port)) port = cur_inp;
  ASRTER(NIMP(port) && OPINPORTP(port), port, ARG1, s_char_readyp);
  if (CRDYP(port) || !(BUF0 & SCM_PORTFLAGS(port))) return BOOL_T;
  return input_waiting(STREAM(port)) ? BOOL_T : BOOL_F;
}

#ifdef GO32
# include <pc.h>
#endif
#ifndef HAVE_SELECT
# ifdef PLAN9
#  define kbhit() 0
# else
#  include <time.h>
# endif
#endif
#ifdef __STDC__
# define timet time_t
#else
# define timet long
#endif
static char s_wfi[] = "wait-for-input";
SCM wait_for_input(args)
     SCM args;
{
  SCM how_long, port1, port, ports, ans = EOL;
  int timeout, pos = ARG2;
  ASRTER(!NULLP(args), INUM0, WNA, s_wfi);
  how_long = CAR(args);
  args = CDR(args);
  if (NULLP(args)) port1 = cur_inp;
  else {
    port1 = CAR(args);
    args = CDR(args);
  }
  timeout = num2long(how_long, (char *)ARG1, s_wfi);
  ASRTER(timeout >= 0, how_long, ARG1, s_wfi);
  port = port1;
  ports = args;
  while (1) {
/*     ASRTER(NIMP(port) && OPINPORTP(port) && (BUF0 & SCM_PORTFLAGS(port)), */
/* 	   port, pos, s_wfi); */
    if (CRDYP(port) || feof(STREAM(port))) timeout = 0;
    if (NULLP(ports)) break;
    if (ARG5 <= pos) pos = ARGn;
    else if (ARG1 < pos) pos = 1 + pos;
    port = CAR(ports);
    ports = CDR(ports);
  }
  {
#ifdef HAVE_SELECT
    fd_set ifds;
    struct timeval tv;
    int ret, fd_max = 0;

    tv.tv_sec = timeout;
    tv.tv_usec = 0;

    FD_ZERO(&ifds);
    port = port1;
    ports = args;
    while (1) {
      int fd = fileno(STREAM(port));
      FD_SET(fd, &ifds);
      if (fd_max < fd) fd_max = fd;

      if (NULLP(ports)) break;
      port = CAR(ports);
      ports = CDR(ports);
    }
    SYSCALL(ret = select(fd_max + 1, &ifds, (fd_set *)0L, (fd_set *)0L, &tv););
    ASRTER(ret>=0, MAKINUM(ret), "select error", s_wfi);

    port = port1;
    ports = args;
    while (1) {
      if (FD_ISSET(fileno(STREAM(port)), &ifds)
	  || CRDYP(port) || feof(STREAM(port)))
	ans = cons(port, ans);
      if (NULLP(ports)) break;
      port = CAR(ports);
      ports = CDR(ports);
    }
#else
    timet start = 0;
    time(&start);
    start = start + timeout;
    port = port1;
    ports = args;
    do {
      FILE *f = STREAM(port);
      if (feof(f)) ans = cons(port, ans);
      else {
# ifdef _WIN32
        if (fileno(f)==fileno(stdin) && (isatty(fileno(stdin))) && kbhit())
          ans = cons(port, ans);
# else
#  ifdef FIONREAD
        long remir;
        ioctl(fileno(f), FIONREAD, &remir);
        if (remir) ans = cons(port, ans);
#  else
        /* If we get here this is not going to work */
#  endif
# endif
          if (NULLP(ports)) break;
        port = CAR(ports);
        ports = CDR(ports);
      }
    } while (time((timet*)0L) < start);
#endif
    return NULLP(ans) ? BOOL_F : ans;
  }
}

SCM eof_objectp(x)
     SCM x;
{
	return (EOF_VAL==x) ? BOOL_T : BOOL_F;
}

static SCM *loc_broken_pipe = 0;
/* returning non-zero means try again. */
int scm_io_error(port, what)
     SCM port;
     const char *what;
{
#ifdef HAVE_PIPE
# ifdef EPIPE
  if (EPIPE==errno) {
    if (scm_verbose > 2) {
      err_head("WARNING");
      lputs(";;", cur_errp);
      lputs(what, cur_errp);
      lputs(": closing pipe ", cur_errp);
      scm_iprin1(port, cur_errp, 1);
      scm_newline(cur_errp);
    }
    close_port(port);
    if (*loc_broken_pipe && NIMP(*loc_broken_pipe))
      apply(*loc_broken_pipe, port, listofnull);
    return 0;
  }
# endif
#endif
  if (SCM_INTERRUPTED(errno)) {
    errno = 0;
    return !0;
  }
  wta(port, what, "Input/Output");
  return 0;			/* squelch warning */
}

void lfflush(port)		/* internal SCM call */
     SCM port;
{
  sizet i = PTOBNUM(port);
  while ((ptobs[i].fflush)(STREAM(port)) &&
	 scm_io_error(port, "lfflush"))
    ;
}
static char	s_force_output[] = "force-output";
SCM scm_force_output(port)
     SCM port;
{
  if (UNBNDP(port)) port = cur_outp;
  else ASRTER(NIMP(port) && OPOUTPORTP(port), port, ARG1, s_force_output);
  lfflush(port);
  return UNSPECIFIED;
}

SCM scm_write(obj, port)
     SCM obj, port;
{
	if (UNBNDP(port)) port = cur_outp;
	else ASRTER(NIMP(port) && OPOUTPORTP(port), port, ARG2, s_write);
	scm_iprin1(obj, port, 1);
	return UNSPECIFIED;
}
SCM scm_display(obj, port)
     SCM obj, port;
{
	if (UNBNDP(port)) port = cur_outp;
	else ASRTER(NIMP(port) && OPOUTPORTP(port), port, ARG2, s_display);
	scm_iprin1(obj, port, 0);
	return UNSPECIFIED;
}
SCM scm_newline(port)
     SCM port;
{
	if (UNBNDP(port)) port = cur_outp;
	else ASRTER(NIMP(port) && OPOUTPORTP(port), port, ARG1, s_newline);
	lputc('\n', port);
	if (port==cur_outp) lfflush(port);
	return UNSPECIFIED;
}
SCM scm_write_char(chr, port)
     SCM chr, port;
{
	if (UNBNDP(port)) port = cur_outp;
	else ASRTER(NIMP(port) && OPOUTPORTP(port), port, ARG2, s_write_char);
	ASRTER(ICHRP(chr), chr, ARG1, s_write_char);
	lputc((int)ICHR(chr), port);
	return UNSPECIFIED;
}
SCM scm_freshline(port)
     SCM port;
{
	if (UNBNDP(port)) port = cur_outp;
	else ASRTER(NIMP(port) && OPOUTPORTP(port), port, ARG1, s_freshline);
	if (INUM0==scm_port_col(port)) return UNSPECIFIED;
	lputc('\n', port);
	if (port==cur_outp) lfflush(port);
	return UNSPECIFIED;
}

void lputc(c, port)
     int c;
     SCM port;
{
  sizet i = PTOBNUM(port);
  while (EOF==(ptobs[i].fputc)(c, STREAM(port)) &&
	 scm_io_error(port, "fputc"))
    ;
  if (CRDY & CAR(port)) {
    i = SCM_PORTNUM(port);
    switch (c) {
    case LINE_INCREMENTORS:
      scm_port_table[i].line++;
      scm_port_table[i].col = 1;
      break;
    default:
      scm_port_table[i].col++;
    }
  }
}
void lputs(s, port)
     const char *s;
     SCM port;
{
  sizet i = PTOBNUM(port);
  ASRTER(s, INUM0, ARG1, "lputs");
  while (EOF==(ptobs[i].fputs)(s, STREAM(port)) &&
	 scm_io_error(port, "fputs"))
    ;
  if (CRDY & CAR(port)) {
    sizet j;
    i = SCM_PORTNUM(port);
    for (j = 0; s[j]; j++) {
      switch (s[j]) {
      case LINE_INCREMENTORS:
	scm_port_table[i].line++;
	scm_port_table[i].col = 1;
	break;
      default:
	scm_port_table[i].col++;
      }
    }
  }
}
sizet lfwrite(ptr, size, nitems, port)
     char *ptr;
     sizet size;
     sizet nitems;
     SCM port;
{
  sizet ret, i = PTOBNUM(port);
  do {
    ret = (ptobs[i].fwrite)(ptr, size, nitems, STREAM(port));
  } while(nitems != ret && scm_io_error(port, "fwrite"));
  if (CRDY & CAR(port)) {
    sizet j;
    i = SCM_PORTNUM(port);
    for (j = 0; j < ret*size; j++) {
      switch (ptr[j]) {
      case LINE_INCREMENTORS:
	scm_port_table[i].line++;
	scm_port_table[i].col = 1;
	break;
      default:
	scm_port_table[i].col++;
      }
    }
  }
  return ret;
}

int lgetc(port)
  SCM port;
{
  FILE *f;
  int c;
  int i, j = -1;
  if (CRDY & CAR(port)) {
    j = SCM_PORTNUM(port);
    c = scm_port_table[j].unread;
    if (c != EOF) {
      scm_port_table[j].unread = EOF;
      CAR(port) &= (scm_port_table[j].flags | (~0xf0000)); /* CLRDY(port) */
      return c;
    }
  }
  f = STREAM(port);
  i = PTOBNUM(port);
#ifdef linux
  c = (ptobs[i].fgetc)(f);
#else
  SYSCALL(c = (ptobs[i].fgetc)(f););
#endif
  if (j > -1)  {
    /* This means that CRDY is set, note that CRDY is overloaded */
    switch (c) {
    case LINE_INCREMENTORS:
      scm_port_table[j].line++;
      scm_port_table[j].colprev = scm_port_table[j].col;
      scm_port_table[j].col = 1;
      break;
    default:
      scm_port_table[j].col++;
    }
  }
  return c;
}
void lungetc(c, port)
  int c;
  SCM port;
{
  int i = PTOBNUM(port);
/*	ASRTER(!CRDYP(port), port, ARG2, "too many lungetc");*/
  if (ptobs[i].ungetc)
    (ptobs[i].ungetc)(c, port);
  else {
    scm_port_table[SCM_PORTNUM(port)].unread = c;
    CAR(port) |= CRDY;
  }
}

SCM scm_read_char(port)
     SCM port;
{
  int c;
  if (UNBNDP(port)) port = cur_inp;
  ASRTER(NIMP(port) && OPINPORTP(port), port, ARG1, s_read_char);
  c = lgetc(port);
  if (EOF==c) return EOF_VAL;
  return MAKICHR(c);
}
SCM scm_peek_char(port)
  SCM port;
{
	int c;
	if (UNBNDP(port)) port = cur_inp;
	else ASRTER(NIMP(port) && OPINPORTP(port), port, ARG1, s_peek_char);
	c = lgetc(port);
	if (EOF==c) return EOF_VAL;
	lungetc(c, port);
	return MAKICHR(c);
}

char *grow_tok_buf(tok_buf)
     SCM tok_buf;
{
  sizet len = LENGTH(tok_buf);
  len += len / 2;
  resizuve(tok_buf, (SCM)MAKINUM(len));
  return CHARS(tok_buf);
}

static int flush_ws(port)
     SCM port;
{
  register int c;
  while(1) switch (c = lgetc(port)) {
    case ';': lp: switch (c = lgetc(port)) {
      default: goto lp;
      case EOF: return c;
      case LINE_INCREMENTORS: break;
    }
    case LINE_INCREMENTORS:
    case WHITE_SPACES: break;
    case EOF:
    default:
      return c;
  }
}

/* Top-level readers */
static SCM p_read_for_load, p_read;
static char s_read[] = "read";
static char s_read_for_load[] = "read-for-load";
#ifndef MEMOIZE_LOCALS
static SCM p_read_numbered;
static char s_read_numbered[] = "read-numbered";
#endif
SCM scm_read(port)
     SCM port;
{
  return scm_lread1(port, case_sensitize_symbols, s_read);
}

SCM scm_read_for_load(port)
     SCM port;
{
  return scm_lread1(port, 4 | case_sensitize_symbols, s_read_for_load);
}

#ifndef MEMOIZE_LOCALS
SCM scm_read_numbered(port)
     SCM port;
{
  return scm_lread1(port, 6 | case_sensitize_symbols, s_read_numbered);
}
#endif

static SCM scm_lread1(port, flgs, what)
     SCM port;
     int flgs;
     const char *what;
{
  int c;
  SCM tok_buf;
  if (UNBNDP(port)) port = cur_inp;
  ASRTER(NIMP(port) && OPINPORTP(port), port, ARG1, what);
  do {
    c = flush_ws(port);
    if (EOF==c) return EOF_VAL;
    lungetc(c, port);
    tok_buf = makstr(30L);
  } while (EOF_VAL==(tok_buf = scm_lreadr(tok_buf, port, flgs)));
  return tok_buf;
}
static SCM *loc_loadsharp = 0, *loc_readsharp = 0, *loc_charsharp = 0;
static SCM scm_lreadpr(tok_buf, port, flgs)
     SCM tok_buf;
     SCM port;
     int flgs;
{
  int c;
  sizet j;
  SCM p;
  if (2==(3 & flgs)) return scm_lread_rec(tok_buf, port);
 tryagain:
  c = flush_ws(port);
  switch (c) {
  case EOF: return EOF_VAL;
#ifdef BRACKETS_AS_PARENS
  case '[':
#endif
  case '(': return scm_lreadparen(tok_buf, port, flgs, s_list);
#ifdef BRACKETS_AS_PARENS
  case ']':
#endif
  case ')': return UNDEFINED; /* goto tryagain; */
  case '\'': return cons2(i_quote,
			  scm_lreadr(tok_buf, port, flgs), EOL);
  case '`': return cons2(i_quasiquote,
			 scm_lreadr(tok_buf, port, flgs), EOL);
  case ',':
    c = lgetc(port);
    if ('@'==c) p = i_uq_splicing;
    else {
      lungetc(c, port);
      p = i_unquote;
    }
    return cons2(p, scm_lreadr(tok_buf, port, flgs), EOL);
  case '#':
    c = lgetc(port);
    switch (c) {
#ifdef BRACKETS_AS_PARENS
    case '[':
#endif
    case '(':
      p = scm_lreadparen(tok_buf, port, flgs, s_vector);
      return NULLP(p) ? nullvect : vector(p);
    case 't': case 'T': return BOOL_T;
    case 'f': case 'F': return BOOL_F;
    case 'b': case 'B': case 'o': case 'O':
    case 'd': case 'D': case 'x': case 'X':
    case 'i': case 'I': case 'e': case 'E':
      lungetc(c, port);
      c = '#';
      goto num;
    case '*':
      j = scm_read_token(c, tok_buf, port, flgs);
      p = istr2bve(CHARS(tok_buf)+1, (long)(j-1));
      if (NFALSEP(p)) return p;
      else goto unkshrp;
    case '\\':
      c = lgetc(port);
      if ('\\'==c) {
	CHARS(tok_buf)[0] = c;
	j = 1;
      } else j = scm_read_token(c, tok_buf, port, flgs);
      if (j==1) return MAKICHR(c);
      for (c = 0;c<sizeof charnames/sizeof(char *);c++)
	if (charnames[c]
	    && (0==strcasecmp(charnames[c], CHARS(tok_buf))))
	  return MAKICHR(charnums[c]);
      if (loc_charsharp && NIMP(*loc_charsharp)) {
	resizuve(tok_buf, MAKINUM(j));
	p = apply(*loc_charsharp, tok_buf, listofnull);
	if (ICHRP(p)) return p;
      }
      wta(UNDEFINED, "unknown # object: #\\", CHARS(tok_buf));
    case '|':
      j = 1;	/* here j is the comment nesting depth */
    lp: c = lgetc(port);
    lpc:
      switch (c) {
      case EOF: wta(UNDEFINED, s_eofin, "balanced comment");
      case LINE_INCREMENTORS:
      default:
	goto lp;
      case '|':
	if ('#' != (c = lgetc(port))) goto lpc;
	if (--j) goto lp;
	break;
      case '#':
	if ('|' != (c = lgetc(port))) goto lpc;
	++j; goto lp;
      }
      goto tryagain;
    default: callshrp:
      {
	SCM reader =
#ifndef MEMOIZE_LOCALS
	  (3 & flgs) ? p_read_numbered :
#endif
	  ((4 & flgs) ? p_read_for_load : p_read);
	SCM args = cons2(MAKICHR(c), port, cons(reader, EOL));
	if ((4 & flgs) && loc_loadsharp && NIMP(*loc_loadsharp)) {
	  p = apply(*loc_loadsharp, args, EOL);
	  if (UNSPECIFIED==p) goto tryagain;
	  return p;
	} else if (loc_readsharp && NIMP(*loc_readsharp)) {
	  p = apply(*loc_readsharp, args, EOL);
	  if (UNSPECIFIED==p) goto tryagain;
	  return p;
	}
      }
    unkshrp: wta((SCM)MAKICHR(c), s_unknown_sharp, "");
    }
  case '\"':
    j = 0;
    while ('\"' != (c = lgetc(port))) {
      ASRTER(EOF != c, UNDEFINED, s_eofin, s_string);
      if (j+1 >= LENGTH(tok_buf)) grow_tok_buf(tok_buf);
      switch (c) {
      case LINE_INCREMENTORS: break;
      case '\\':
	switch (c = lgetc(port)) {
	case LINE_INCREMENTORS: continue;
	case '0': c = '\0'; break;
	case 'f': c = '\f'; break;
	case 'n': c = '\n'; break;
	case 'r': c = '\r'; break;
	case 't': c = '\t'; break;
	case 'a': c = '\007'; break;
	case 'v': c = '\v'; break;
	}
      }
      CHARS(tok_buf)[j] = c;
      ++j;
    }
    if (j==0) return nullstr;
    CHARS(tok_buf)[j] = 0;
    return makfromstr(CHARS(tok_buf), j);
  case DIGITS:
  case '.': case '-': case '+':
  num:
    j = scm_read_token(c, tok_buf, port, flgs);
    p = istring2number(CHARS(tok_buf), (long)j, 10L);
    if (NFALSEP(p)) return p;
    if (c=='#') {
      if ((j==2) && (lgetc(port)=='(')) {
	lungetc('(', port);
	c = CHARS(tok_buf)[1];
	goto callshrp;
      }
      wta(UNDEFINED, s_unknown_sharp, CHARS(tok_buf));
    }
    goto tok;
  default:
    j = scm_read_token(c, tok_buf, port, flgs);
  tok:
    p = intern(CHARS(tok_buf), j);
    return CAR(p);
  }
}
static SCM scm_lreadr(tok_buf, port, flgs)
     SCM tok_buf;
     SCM port;
     int flgs;
{
  SCM ans = scm_lreadpr(tok_buf, port, flgs);
  switch (ans) {
  case UNDEFINED:
    scm_warn("unexpected \")\"", "", port);
    return scm_lreadpr(tok_buf, port, flgs);
  }
  return ans;
}
static SCM scm_lread_rec(tok_buf, port)
     SCM tok_buf;
     SCM port;
{
  SCM line, form;
  int c = flush_ws(port);
  switch (c) {
  default:
    lungetc(c, port);
    line = scm_port_line(port);
    form = scm_lreadpr(tok_buf, port, 5);
    if (NFALSEP(line) && NIMP(form) &&
	(CONSP(form) || VECTORP(form))) {
      return cons(SCM_MAKE_LINUM(INUM(line)), form);
    }
    return form;
#ifdef BRACKETS_AS_PARENS
  case ']':
#endif
  case ')': return UNDEFINED;
  case EOF: return EOF_VAL;
  }
}

#ifdef _UNICOS
_Pragma("noopt");		/* # pragma _CRI noopt */
#endif
static sizet scm_read_token(ic, tok_buf, port, flgs)
     int ic;
     SCM tok_buf;
     SCM port;
     int flgs;
{
  register sizet j = 1;
  register int c = ic;
  register char *p = CHARS(tok_buf);
  p[0] = '\\'==c ? lgetc(port) : 8 & flgs ? c : downcase[c];
  while(1) {
    if (j+1 >= LENGTH(tok_buf)) p = grow_tok_buf(tok_buf);
    switch (c = lgetc(port)) {
#ifdef BRACKETS_AS_PARENS
    case '[': case ']':
#endif
    case '(': case ')': case '\"': case ';':
    case ',': case '`':
      /* case '#': */
    case WHITE_SPACES:
    case LINE_INCREMENTORS:
      lungetc(c, port);
    case EOF:
      p[j] = 0;
      return j;
    case '\\':	/* slashified symbol */
      p[j++] = lgetc(port);
      break;
    default:
      p[j++] = 8 & flgs ? c : downcase[c];
    }
  }
}
#ifdef _UNICOS
_Pragma("opt");			/* # pragma _CRI opt */
#endif

/* flgs was originally an argument to determine whether a read was */
/* top-level or recursve.  It has been overloaded to determine also */
/* what to do in the case of a recursive read. */
/* It distinguishes four states: */
/* 0 - not adding line-numbers - never changes.  Uses READ:SHARP */
/* 4 - not adding line-numbers - never changes.  Uses LOAD:SHARP */
/* 5 - top level read when adding line-numbers.  Uses LOAD:SHARP */
/* 6 - recursive read when adding line-numbers.  Uses LOAD:SHARP */

static SCM scm_lreadparen(tok_buf, port, flgs, name)
     SCM tok_buf;
     SCM port;
     int flgs;
     char *name;
{
  SCM lst, fst,
    tmp = scm_lreadpr(tok_buf, port, (0xC & flgs) | ((3 & flgs) ? 2 : 0));
  if (UNDEFINED==tmp) return EOL;
  if (i_dot==tmp) {
    fst = scm_lreadr(tok_buf, port, (0xC & flgs) | ((3 & flgs) ? 1 : 0));
  closeit:
    tmp = scm_lreadpr(tok_buf, port, 0);
    if (UNDEFINED != tmp) wta(UNDEFINED, "missing close paren", name);
    return fst;
  }
  fst = lst = cons(tmp, EOL);
  while (UNDEFINED !=
	 (tmp = scm_lreadpr(tok_buf, port, (0xC & flgs) | ((3 & flgs) ? 2 : 0)))) {
    if (EOF_VAL==tmp) wta(lst, s_eofin, s_list);
    if (i_dot==tmp) {
      CDR(lst) = scm_lreadr(tok_buf, port, (0xC & flgs) | ((3 & flgs) ? 1 : 0));
      goto closeit;
    }
    lst = (CDR(lst) = cons(tmp, EOL));
  }
  return fst;
}

/* These procedures implement synchronization primitives.  Processors
   with an atomic test-and-set instruction can use it here (and not
   DEFER_INTS). */
char s_swapcar[] = "swap-car!";
SCM swapcar(pair, value)
     SCM pair, value;
{
  SCM ret;
  ASRTER(NIMP(pair) && CONSP(pair), pair, ARG1, s_swapcar);
  DEFER_INTS;
  ret = CAR(pair);
  CAR(pair) = value;
  ALLOW_INTS;
  return ret;
}
char s_tryarb[] = "try-arbiter";
char s_relarb[] = "release-arbiter";
long tc16_arbiter;
SCM tryarb(arb)
     SCM arb;
{
  ASRTER((TYP16(arb)==tc16_arbiter), arb, ARG1, s_tryarb);
  DEFER_INTS;
  if (CAR(arb) & (1L<<16))
    arb = BOOL_F;
  else {
    CAR(arb) = tc16_arbiter | (1L<<16);
    arb = BOOL_T;
  }
  ALLOW_INTS;
  return arb;
}
SCM relarb(arb)
     SCM arb;
{
  ASRTER((TYP16(arb)==tc16_arbiter), arb, ARG1, s_relarb);
  if (!(CAR(arb) & (1L<<16))) return BOOL_F;
  CAR(arb) = tc16_arbiter;
  return BOOL_T;
}
SCM makarb(name)
     SCM name;
{
  register SCM z;
  NEWCELL(z);
  CDR(z) = name;
  CAR(z) = tc16_arbiter;
  return z;
}
static int prinarb(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  lputs("#<arbiter ", port);
  if (CAR(exp) & (1L<<16)) lputs("locked ", port);
  scm_iprin1(CDR(exp), port, writing);
  lputc('>', port);
  return !0;
}

static char s_tryload[] = "try-load";
#define s_load (&s_tryload[4])

struct errdesc {char *msg;char *s_response;short parent_err;};
struct errdesc errmsgs[] = {
  {"Wrong number of args", 0, 0},
  {"numerical overflow", 0, FPE_SIGNAL},
  {"Argument out of range", 0, FPE_SIGNAL},
  {"Could not allocate", "out-of-storage", 0},
  {"Thrashing", "thrashing", 0},
  {"EXIT", "end-of-program", -1},
  {"hang up", "hang-up", EXIT},
  {"user interrupt", "user-interrupt", 0},
  {"arithmetic error", "arithmetic-error", 0},
  {"bus error", 0, 0},
  {"segment violation", 0, 0},
  {"alarm", "alarm-interrupt", 0},
  {"virtual alarm", "virtual-alarm-interrupt", 0},
  {"profile interrupt", "profile-alarm-interrupt", 0},
};

void (* deferred_proc) P((void)) = 0;
char *errjmp_bad = "init";
VOLATILE int ints_disabled = 1;
unsigned long SIG_deferred = 0;
int scm_verbose = 1;		/* Low so that monitor info won't be */
				/* printed while in init_storage. (BOOM) */
static int errjmp_recursive = 0;
static int errobj_codep;
static SCM err_exp, err_env;
static char *err_pos, *err_s_subr;
static cell tmp_errobj = {(SCM)UNDEFINED, (SCM)EOL};
static cell tmp_loadpath = {(SCM)BOOL_F, (SCM)EOL};
SCM *loc_errobj = (SCM *)&tmp_errobj;
SCM *loc_loadpath = (SCM *)&tmp_loadpath;
unsigned long  mallocated = 0, lmallocated = 0;
long cells_allocated = 0, lcells_allocated = 0,
  rt = 0, gc_rt, gc_time_taken;
long gc_cells_collected, gc_malloc_collected, gc_ports_collected;
long gc_syms_collected;
long scm_env_work = 0,  scm_gcs = 0, scm_egcs = 0,
  scm_stk_moved = 0, scm_clo_moved = 0, scm_egc_rt;
static void def_err_response P((void));

int handle_it(i)
     int i;
{
  SCM proc;
  char *name = errmsgs[i-WNA].s_response;
  if (errjmp_bad || errjmp_recursive)
    wta(UNDEFINED, (char *)i, ""); /* sends it to def_err_response */
  /* NEWCELL does not defer interrupts; so be careful to maintain the
     freelist integrity. */
  if (name) {
    int j;
    SCM n[2];	   /* GC-protect discarded cells (possibly being used
		      by interrupted code). */
    DEFER_INTS;
    /* Two cells are discarded because NEWCELL may have been
       interrupted between computing the right-hand-side of
          freelist = CDR(freelist)
       and assigning it to freelist. */
    for (j=0; j<2; j++) NEWCELL(n[j]); /* discard 2 possibly-used cells */
    CDR(n[1]) = EOL;		/* lose pointer to freelist */
    ALLOW_INTS;
    /* discarding was necessary here because intern() may do NEWCELL */
    proc = CDR(intern(name, (sizet)strlen(name)));
    if (NIMP(proc)) {	   /* Save environment stack, in case it moves
			      when applying proc.  Do an ecache gc to
			      protect contents of stack. */
      SCM estk, *estk_ptr, env, env_tmp;
      DEFER_INTS;
#ifndef NO_ENV_CACHE
      scm_egc();
#endif
      estk = scm_estk;
      estk_ptr = scm_estk_ptr;
      env = scm_env;
      env_tmp = scm_env_tmp;
      scm_estk = BOOL_F;
      scm_estk_reset(0);
      SCM_ESTK_PARENT(scm_estk) = estk;
      SCM_ESTK_PARENT_INDEX(scm_estk) = MAKINUM(estk_ptr - VELTS(estk));
      ALLOW_INTS;
      apply(proc, EOL, EOL);
      DEFER_INTS;
      scm_estk = estk;
      scm_estk_ptr = estk_ptr;
      scm_env = env;
      scm_env_tmp = env_tmp;
      scm_fill_freelist();
      ALLOW_INTS;
      return i;
    }
  }
  /* Ensure that freelist is not empty when returning from interrupt */
  DEFER_INTS;
  scm_fill_freelist();
  ALLOW_INTS;
  return errmsgs[i-WNA].parent_err;
}

SCM exitval = MAKINUM(EXIT_FAILURE); /* INUM return value */
extern char s_unexec[];
SCM scm_top_level(initpath, toplvl_fun)
     char *initpath;
     SCM (*toplvl_fun)();
{
  SCM ret;
#ifdef _UNICOS
  int i;
#else
  long i;
#endif
  if (!toplvl_fun) toplvl_fun = repl;
  CONT(rootcont)->stkbse = (STACKITEM *)&i;
  i = setjump(CONT(rootcont)->jmpbuf);
#ifndef SHORT_INT
  if (i) i = UNCOOK(i);
#endif
 drloop:
  switch (PTR2INT(i)) {
  default:
    {
      char *name = errmsgs[i-WNA].s_response;
      if (name) {
	SCM proc = CDR(intern(name, (sizet)strlen(name)));
	if (NIMP(proc)) apply(proc, EOL, EOL);
      }}
    i = errmsgs[i-WNA].parent_err;
    if (i) goto drloop;
  case 1:			/* from everr() */
    def_err_response();
    goto reset_toplvl;
  case 0:
    exitval = MAKINUM(EXIT_SUCCESS);
    errjmp_bad = (char *)0;
    errjmp_recursive = 0;
    if (NIMP(sys_errp) && OPOUTPORTP(sys_errp)) lfflush(sys_errp);
    errno = 0;
    SIG_deferred = 0;
    deferred_proc = 0;
    ints_disabled = 0;
    scm_init_INITS();
    if (dumped) {
      lcells_allocated = cells_allocated;
      lmallocated = mallocated;
      rt = INUM(my_time());
      gc_time_taken = 0;
    }
    else if (initpath &&
	     (isspace(initpath[0]) || ';'==initpath[0] || '('==initpath[0]))
      scm_ldstr(initpath);
    else if (scm_ldfile(initpath ? initpath : "")) /* load Scheme init files */
      wta(*loc_errobj, "Could not open file", s_load);
    {
      SCM boot_tail = scm_evstr("boot-tail");
      /* initialization tail-call */
      if (NIMP(boot_tail))
        apply(boot_tail, (dumped ? makfrom0str(initpath) : BOOL_F), listofnull);
    }
  case -2:			/* abrt */
  reset_toplvl:
    ints_disabled = 1;
    errjmp_bad = (char *)0;
    errjmp_recursive = 0;
    if (NIMP(sys_errp) && OPOUTPORTP(sys_errp)) lfflush(sys_errp);
    SIG_deferred = 0;
    deferred_proc = 0;
    gc_hook_active = 0;
    scm_estk_reset(0);

    /* Closing the loading file turned out to be a bad idea. */
    /* But I will leave the code here in case someone wants it. */
#ifdef CLOSE_LOADING_PORTS_ON_ABORT
    if (NIMP(loadports) && OPINPORTP(CAR(loadports))) {
      if (scm_verbose > 1) {
	lputs("; Aborting load (closing): ", cur_errp);
	scm_display(*loc_loadpath, cur_errp);
	scm_newline(cur_errp);
      }
      close_port(CAR(loadports)); /* close loading file. */
    }
#endif

    *loc_loadpath = BOOL_F;
    loadports = EOL;
    ints_disabled = 0;
    dowinds(EOL);
    ret = toplvl_fun();		/* typically repl() */
    if (INUMP(ret)) exitval = ret;
    err_pos = (char *)EXIT;
    i = EXIT;
    goto drloop;		/* encountered EOF on stdin */
  case -1:			/* quit */
    dowinds(EOL);
    if (MAKINUM(EXIT_SUCCESS) != exitval) {
      lputs("; program args: ", cur_errp);
      scm_write(progargs, cur_errp);
      scm_newline(cur_errp);
    }
    return exitval;
  case -3:			/* restart. */
    dowinds(EOL);
    return 0;
#ifdef CAN_DUMP
  case -4:			/* dump */
    DEFER_INTS;
    scm_estk_reset(0);
    scm_egc();
    igc(s_unexec, BOOL_F);
    ALLOW_INTS;
    dumped = 1;
# ifdef linux
    sbrk(getpagesize());	/* The last few words of the .data segment
				   were not being mapped in for dumped
				   executables. */
# endif
    unexec(CHARS(*loc_errobj), execpath, 0, 0, 0);
    goto reset_toplvl;
#endif
  }
}

SCM line_num()
{
  if (IMP(loadports))
    return INUM0;
  return scm_port_line(CAR(loadports));
}
static char s_port_line[] = "port-line";
SCM scm_port_line(port)
     SCM port;
{
  sizet lnum;
  ASRTER(NIMP(port) && PORTP(port), port, ARG1, s_port_line);
  if (! (TRACKED & SCM_PORTFLAGS(port))) return BOOL_F;
  lnum = scm_port_table[SCM_PORTNUM(port)].line;
  switch (CGETUN(port)) {
  default:
  case EOF:			/* no ungetted char */
    break;
  case LINE_INCREMENTORS:
    lnum--;
    break;
  }
  return MAKINUM(lnum);
}
static char s_port_col[] = "port-column";
SCM scm_port_col(port)
     SCM port;
{
  long col;
  ASRTER(NIMP(port) && PORTP(port), port, ARG1, s_port_col);
  if (! (TRACKED & SCM_PORTFLAGS(port))) return BOOL_F;
  col = scm_port_table[SCM_PORTNUM(port)].col;
  switch (CGETUN(port)) {
  default:
    col--;
    break;
  case EOF:			/* no ungetted char */
    break;
  case LINE_INCREMENTORS:
    col = scm_port_table[SCM_PORTNUM(port)].colprev;
    break;
  }
  return MAKINUM(col);
}

static char	s_file_position[] = "file-position";
SCM scm_file_position(port, pos)
     SCM port, pos;
{
  ASRTER(NIMP(port) && OPFPORTP(port), port, ARG1, s_file_position);
  if (UNBNDP(pos) || FALSEP(pos)) {
    long ans;
    SYSCALL(ans = ftell(STREAM(port)););
    if (ans < 0) return BOOL_F;
    if (CRDYP(port)) ans--;
    return MAKINUM(ans);
  }
  ASRTER((INUMP(pos) && (INUM(pos) >= 0))
	 || (NIMP(pos) && (TYP16(pos)==tc16_bigpos)),
	 port, ARG2, s_file_position);
#ifndef RECKLESS
  if (TRACKED & SCM_PORTFLAGS(port)) {
    if (INUM0==pos) {
      int i = SCM_PORTNUM(port);
      scm_port_table[i].line = 1L;
      scm_port_table[i].col = 1;
    }
    else {
      if (2 <= scm_verbose)
	scm_warn("Setting file position for tracked port: ", "", port);
      SCM_PORTFLAGS(port) &= (~TRACKED);
    }
  }
#endif
  {
    int ans;
    CLRDY(port);		/* Clear ungetted char */
    SYSCALL(ans = fseek(STREAM(port), INUM(pos), 0););
#ifdef HAVE_PIPE
# ifdef ESPIPE
    if (!OPIOPORTP(port))
      ASRTER(ESPIPE != errno, port, ARG1, s_file_position);
# endif
#endif
    return ans ? BOOL_F : BOOL_T;
  }
}

static char s_port_filename[] = "port-filename";
SCM scm_port_filename(port)
     SCM port;
{
  SCM x;
  ASRTER(NIMP(port) && PORTP(port), port, ARG1, s_port_filename);
  x = SCM_PORTDATA(port);
  if (NIMP(x) && STRINGP(x))
    return SCM_PORTDATA(port);
  return BOOL_F;
}

SCM prog_args()
{
  return progargs;
}

extern char s_heap[];
void growth_mon(obj, size, units, grewp)
     char *obj;
     long size;
     char *units;
     int grewp;
{
  if (scm_verbose > 2)
    {
      lputs((grewp ? "; grew " : "; shrank "), sys_errp);
      lputs(obj, sys_errp);
      lputs(" to ", sys_errp);
      scm_intprint(size, -10, sys_errp);
      lputc(' ', sys_errp);
      lputs(units, sys_errp);
      if ((scm_verbose > 4) && (obj==s_heap)) heap_report();
      lputs("\n; ", sys_errp);
    }
}

void gc_start(what)
     const char *what;
{
  if (scm_verbose > 4) {
    lputs(";GC(", sys_errp);
    lputs(what, sys_errp);
    lputs(") ", sys_errp);
  }
  scm_gcs++;
  gc_rt = INUM(my_time());
  gc_cells_collected = 0;
  gc_malloc_collected = 0;
  gc_ports_collected = 0;
  gc_syms_collected = 0;
}
void gc_end()
{
  gc_rt = INUM(my_time()) - gc_rt;
  gc_time_taken = gc_time_taken + gc_rt;
  if (scm_verbose > 4) {
    scm_intprint(time_in_msec(gc_rt), -10, sys_errp);
    lputs(".ms cpu, ", sys_errp);
    scm_intprint(gc_cells_collected, -10, sys_errp);
    lputs(" cells, ", sys_errp);
    scm_intprint(gc_malloc_collected, -10, sys_errp);
    lputs(" malloc, ", sys_errp);
    scm_intprint(gc_syms_collected, -10, sys_errp);
    lputs(" syms, ", sys_errp);
    scm_intprint(gc_ports_collected, -10, sys_errp);
    lputs(" ports collected\n", sys_errp);
  }
}
void scm_egc_start()
{
  scm_egc_rt = INUM(my_time());
  scm_egcs++;
}
void scm_egc_end()
{
  scm_egc_rt = INUM(my_time()) - scm_egc_rt;
  gc_time_taken = gc_time_taken + scm_egc_rt;
}
void repl_report()
{
  if (scm_verbose > 2) {
    lfflush(cur_outp);
    lputs(";Evaluation took ", cur_errp);
    scm_intprint(time_in_msec(INUM(my_time())-rt), -10, cur_errp);
    lputs(".ms (", cur_errp);
    scm_intprint(time_in_msec(gc_time_taken), -10, cur_errp);
    lputs(".ms in gc) ", cur_errp);
    scm_intprint(cells_allocated - lcells_allocated, -10, cur_errp);
    lputs(" cells work, ", cur_errp);
    scm_env_work += scm_ecache_len - scm_ecache_index;
    scm_intprint(scm_env_work, -10, cur_errp);
    lputs(" env, ", cur_errp);
    scm_intprint(mallocated - lmallocated, -10, cur_errp);
    lputs(".B other\n", cur_errp);
    if (scm_verbose > 3) {
      lputc(';', cur_errp);
      scm_intprint(scm_gcs, -10, cur_errp);
      lputs( " gc, ", cur_errp);
      scm_intprint(scm_egcs, -10, cur_errp);
      lputs( " ecache gc, ", cur_errp);
      scm_intprint(scm_clo_moved, -10, cur_errp);
      lputs(" env migrated from closures, ", cur_errp);
      scm_intprint(scm_stk_moved, -10, cur_errp);
      lputs(" from stack\n", cur_errp);
    }
    lfflush(cur_errp);
  }
}
#ifndef LACK_SBRK
unsigned long scm_init_brk = 0, scm_dumped_brk = 0;
void init_sbrk()
{
  if (dumped)
    scm_dumped_brk = (unsigned long)sbrk(0);
  else
    scm_init_brk = (unsigned long)sbrk(0);
}
void scm_brk_report()
{
  unsigned long scm_curbrk = (unsigned long)sbrk(0),
    dif1 = ((dumped ? scm_dumped_brk : scm_curbrk) - scm_init_brk)/1024,
    dif2 = (scm_curbrk - scm_dumped_brk)/1024;

  lputs("initial brk = 0x", cur_errp);
  scm_intprint(scm_init_brk, -16, cur_errp);
  if (dumped) {
    lputs(", dumped = 0x", cur_errp);
    scm_intprint(scm_dumped_brk, -16, cur_errp);
  }
  lputs(", current = 0x", cur_errp);
  scm_intprint(scm_curbrk, -16, cur_errp);
  lputs("; ", cur_errp);
  scm_intprint(dif1, 10, cur_errp);
  if (dumped) {
    lputs(dif2 < 0 ? " - " : " + ", cur_errp);
    scm_intprint(dif2 < 0 ? -dif2 : dif2, 10, cur_errp);
  }
  lputs(".kiB\n", cur_errp);
}
#endif
SCM lroom(opt)
     SCM opt;
{
  scm_intprint(cells_allocated, -10, cur_errp);
  lputs(" out of ", cur_errp);
  scm_intprint(heap_cells, -10, cur_errp);
  lputs(" cells in use, ", cur_errp);
  scm_intprint(mallocated, -10, cur_errp);
  lputs(".B allocated (of ", cur_errp);
  scm_intprint(mtrigger, 10, cur_errp);
  lputs(")\n", cur_errp);
  if (!UNBNDP(opt)) {
#ifndef LACK_SBRK
    if (scm_init_brk) scm_brk_report();
#endif
    scm_ecache_report();
    heap_report(); lputc('\n', cur_errp);
    gra_report();
    stack_report();
  }
  return UNSPECIFIED;
}
void scm_ecache_report()
{
  scm_intprint(scm_estk_size, 10 , cur_errp);
  lputs(" env stack items, ", cur_errp);
  scm_intprint(scm_ecache_len - scm_ecache_index, 10, cur_errp);
  lputs(" out of ", cur_errp);
  scm_intprint(scm_ecache_len, 10, cur_errp);
  lputs(" env cells in use.\n", cur_errp);
}
void exit_report()
{
  if (scm_verbose > 2) {
    lputs(";Totals: ", cur_errp);
    scm_intprint(time_in_msec(INUM(my_time())), -10, cur_errp);
    lputs(".ms my time, ", cur_errp);
    scm_intprint(time_in_msec(INUM(your_time())), -10, cur_errp);
    lputs(".ms your time\n", cur_errp);
  }
}

SCM prolixity(arg)
     SCM arg;
{
  int old = scm_verbose;
  if (!UNBNDP(arg)) {
    if (FALSEP(arg)) scm_verbose = 1;
    else scm_verbose = INUM(arg);
  }
  return MAKINUM(old);
}

static SCM i_repl;
SCM repl()
{
  SCM x;
  SCM env = EOL; /* scm_env_addprop(SCM_ENV_FILENAME, i_repl, EOL); */
  int c;
  if (OPINPORTP(cur_inp) && OPOUTPORTP(cur_outp)) {
    repl_report();
    while(1) {
      if (OPOUTPORTP(cur_inp)) { /* This case for curses window */
	lfflush(cur_outp);
	if (scm_verbose) lputs(PROMPT, cur_inp);
	lfflush(cur_inp);
      }
      else {
	if (scm_verbose) lputs(PROMPT, cur_outp);
	lfflush(cur_outp);
      }
      lcells_allocated = cells_allocated;
      scm_env_work = scm_ecache_index - scm_ecache_len;
      scm_egcs = scm_clo_moved = scm_stk_moved = 0;
      lmallocated = mallocated;
      x = scm_read_for_load(cur_inp);
      rt = INUM(my_time());
      scm_gcs = 0;
      gc_time_taken = 0;
      if (EOF_VAL==x) return MAKINUM(EXIT_SUCCESS);
      if (!CRDYP(cur_inp)) {	/* assure newline read (and transcripted) */
	if (EOF==(c = lgetc(cur_inp))) break;
	lungetc(c, cur_inp);
      }
#ifdef __HIGHC__
# define __MSDOS__
#endif
#ifdef __MSDOS__
      if ('\n' != CGETUN(cur_inp))
	if (OPOUTPORTP(cur_inp)) /* This case for curses window */
	  {lfflush(cur_outp); scm_newline(cur_inp);}
	else scm_newline(cur_outp);
#endif
      if (NIMP(x)) {
	x = CONSP(x) ?
	  scm_eval_values(x, env, (SCM)EOL) :
	  cons(EVAL(x, env, (SCM)EOL), EOL);
      }
      else
	x = cons(x, EOL);
      repl_report();
      if (IMP(x))
	{if (scm_verbose > 2) lputs(";;no values\n", cur_outp);}
      else if (IMP(CDR(x))) {
	scm_iprin1(CAR(x), cur_outp, 1);
	lputc('\n', cur_outp);
      }
      else
	while (NIMP(x)) {
	  lputc(' ', cur_outp);
	  scm_iprin1(CAR(x), cur_outp, 1);
	  lputc('\n', cur_outp);
	  x = CDR(x);
	}
    }
  }
  return UNSPECIFIED;
}
SCM quit(n)
     SCM n;
{
  if (UNBNDP(n) || BOOL_T==n) n = MAKINUM(EXIT_SUCCESS);
  if (INUMP(n)) exitval = n;
  else exitval = MAKINUM(EXIT_FAILURE);
  if (errjmp_bad) exit(INUM(exitval));
  longjump(CONT(rootcont)->jmpbuf, COOKIE(-1));
}
SCM abrt()
{
  if (errjmp_bad) exit(EXIT_FAILURE);
  longjump(CONT(rootcont)->jmpbuf, COOKIE(-2));
}
char s_restart[] = "restart";
SCM restart()
{
  /* ASRTER(!dumped, UNDEFINED, "dumped can't", s_restart); */
  longjump(CONT(rootcont)->jmpbuf, COOKIE(-3));
}

#ifdef CAN_DUMP
char s_unexec[] = "unexec";
SCM scm_unexec(newpath)
     SCM newpath;
{
  ASRTER(NIMP(newpath) && STRINGP(newpath), newpath, ARG1, s_unexec);
  ASRTER(execpath, UNSPECIFIED, s_no_execpath, s_unexec);
  *loc_errobj = newpath;
  longjump(CONT(rootcont)->jmpbuf, COOKIE(-4));
}
#endif

#ifdef CAREFUL_INTS
ints_infot *ints_info = 0;
static void ints_viol_iprin(num)
     int num;
{
  char num_buf[INTBUFLEN];
  sizet i = ilong2str(num+0L, 10, num_buf);
  num_buf[i] = 0;
  fputs(num_buf, stderr);
}
void ints_viol(info, sense)
     ints_infot *info;
     int sense;
{
  fputs(info->fname, stderr);
  fputc(':', stderr);
  ints_viol_iprin(info->linum);
  fputs(": ints already ", stderr);
  fputs(sense ? "dis" : "en", stderr);
  fputs("abled (", stderr);
  ints_viol_iprin(ints_disabled);
  fputs(")\n", stderr);
  if (ints_info) {
    fputs(ints_info->fname, stderr);
    fputc(':', stderr);
    ints_viol_iprin(ints_info->linum);
    fputs(": last change\n", stderr);
  }
  ints_info = info;
}
void ints_warn(str1, str2, fname, linum)
     char *str1, *str2, *fname;
     int linum;
{
  fputs(fname, stderr);
  fputc(':', stderr);
  ints_viol_iprin(linum);
  fputs(": unprotected call to ", stderr);
  fputs(str1, stderr);
  if (str2) {
    fputs(" (", stderr);
    fputs(str2, stderr);
    fputc(')', stderr);
  }
  fputc('\n', stderr);
}
#endif

SCM tryload(filename, reader)
     SCM filename, reader;
{
  ASRTER(NIMP(filename) && STRINGP(filename), filename, ARG1, s_load);
  if (FALSEP(reader)) reader = UNDEFINED;
#ifndef RECKLESS
  if (!UNBNDP(reader)) scm_arity_check(reader, 1L, s_load);
#endif
  {
    SCM oloadpath = *loc_loadpath;
    SCM oloadports = loadports;
    SCM form, port;
    SCM env = EOL;
    port = open_file(filename, makfromstr("r?", (sizet)2*sizeof(char)));
    if (FALSEP(port)) return port;
    *loc_loadpath = filename;
    loadports = cons(port, loadports);
#ifdef SCM_ENV_FILENAME
    env = scm_env_addprop(SCM_ENV_FILENAME, filename, env);
#endif
    while(1) {
      if (UNBNDP(reader))
	form = scm_read_for_load(port);
      else
	form = scm_cvapply(reader, 1L, &port);
      if (EOF_VAL==form) break;
      SIDEVAL(form, env, EOL);
    }
    close_port(port);
    loadports = oloadports;
    *loc_loadpath = oloadpath;
  }
  return BOOL_T;
}
static char s_eval_string[] = "eval-string";
static char s_load_string[] = "load-string";
static SCM i_eval_string = 0;
SCM scm_eval_string(str)
     SCM str;
{
  SCM env = EOL;
#ifdef SCM_ENV_FILENAME
  if (i_eval_string)
    env = scm_env_addprop(SCM_ENV_FILENAME, i_eval_string, env);
#endif
  str = mkstrport(INUM0, str, OPN | RDNG, s_eval_string);
  str = scm_read(str);
  return EVAL(str, env, EOL);
}
static SCM i_load_string = 0;
SCM scm_load_string(str)
     SCM str;
{
  SCM env = EOL;
#ifdef SCM_ENV_FILENAME
  if (i_load_string)
    env = scm_env_addprop(SCM_ENV_FILENAME, i_load_string, env);
#endif
  ASRTER(NIMP(str) && (STRINGP(str) || SYMBOLP(str)), str, ARG1,
	 s_load_string);
  str = mkstrport(INUM0, str, OPN | RDNG, s_load_string);
  while(1) {
    SCM form = scm_read_for_load(str);
    if (EOF_VAL==form) break;
    SIDEVAL(form, env, EOL);
  }
  return BOOL_T;
}

void scm_line_msg(file, linum, port)
    SCM file, linum, port;
{
  scm_iprin1(file, port, 1);
  if (SCM_LINUMP(linum)) {
    lputs(", line ", port);
    scm_intprint(SCM_LINUM(linum), -10, port);
  }
  lputs(": ", port);
}
void scm_err_line(what, file, linum, port)
     const char *what;
     SCM file, linum, port;
{
  lputs(what, port);
  if (NIMP(file) && STRINGP(file))
    scm_line_msg(file, linum, port);
#ifdef CAUTIOUS
  else {
    SCM env = scm_env_getprop(SCM_ENV_FILENAME, scm_trace_env);
    if (NIMP(env)) {
      file = CAR(env);
      scm_check_linum(scm_trace, &linum);
      scm_line_msg(file, linum, port);
    }
  }
#endif
}

static void err_head(str)
     char *str;
{
  SCM lps;
  int oerrno = errno;
  exitval = MAKINUM(EXIT_FAILURE);
  if (NIMP(cur_outp) && OPOUTPORTP(cur_outp)) lfflush(cur_outp);
  lps = IMP(loadports) ? loadports : CDR(loadports);
  if (NIMP(lps)) {
    lputs("\n;In file loaded from ", cur_errp);
    for (; NIMP(lps); lps = CDR(lps)) {
      scm_iprin1(scm_port_filename(CAR(lps)), cur_errp, 0);
      lputs(":", cur_errp);
      scm_iprin1(scm_port_line(CAR(lps)), cur_errp, 1);
      lputs(IMP(CDR(lps)) ? ":" : ",\n;       loaded from ", cur_errp);
    }
  }
  lputs("\n;", cur_errp);
  lfflush(cur_errp);
  errno = oerrno;
  /* if (NIMP(cur_errp) && stderr==STREAM(cur_errp)) { ... } */
  if (errno>0) perror(str);
  fflush(stderr);
}
void scm_warn(str1, str2, obj)
     char *str1, *str2;
     SCM obj;
{
  err_head("WARNING");
  scm_err_line("WARNING: ", UNDEFINED, UNDEFINED, cur_errp);
  lputs(str1, cur_errp);
  if (str2 && *str2) {
    lputs(str2, cur_errp);
    lputc('\n', cur_errp);
  }
  if (!UNBNDP(obj)) {
    scm_iprin1(obj, cur_errp, 1);
    lputc('\n', cur_errp);
  }
  lfflush(cur_errp);
}

SCM lerrno(arg)
     SCM arg;
{
  int old = errno;
  if (!UNBNDP(arg)) {
    if (FALSEP(arg)) errno = 0;
    else errno = INUM(arg);
  }
  return MAKINUM(old);
}
static char s_perror[] = "perror";
SCM lperror(arg)
     SCM arg;
{
  ASRTER(NIMP(arg) && STRINGP(arg), arg, ARG1, s_perror);
  err_head(CHARS(arg));
  return UNSPECIFIED;
}
static void def_err_response()
{
  SCM file, env = err_env, obj = *loc_errobj;
  SCM linum = UNDEFINED;
  int badport = IMP(cur_errp) || !OPOUTPORTP(cur_errp);
  int writing = 2; /* Value of 2 used only for printing error messages */
  int codep = errobj_codep;
  DEFER_INTS;
  if (badport || (errjmp_recursive++)) {
    if (IMP(def_errp) || !OPOUTPORTP(def_errp)) exit(EXIT_FAILURE);
    lputs("RECURSIVE ERROR: ", def_errp);
    if (badport || TYP16(cur_errp)==tc16_sfport) {
      lputs("reverting from ", def_errp);
      scm_iprin1(cur_errp, def_errp, 2);
      lputs("to default error port\n", def_errp);
      cur_errp = def_errp;
      errjmp_recursive = 0;
    }
    else exit(EXIT_FAILURE);
  }
#ifdef SCM_ENV_FILENAME
  file = scm_env_getprop(SCM_ENV_FILENAME, env);
  if (NIMP(file)) file = CAR(file);
  else file = UNDEFINED;
#else
  file = BOOL_F;
#endif
  if (codep) obj = scm_check_linum(obj, &linum);
  err_exp = scm_check_linum(err_exp, UNBNDP(linum) ? &linum : 0L);
  err_head("ERROR");
  scm_err_line("ERROR: ", file, linum, cur_errp);
  if (err_s_subr && *err_s_subr) {
    lputs(err_s_subr, cur_errp);
    lputs(": ", cur_errp);
  }
  if (!err_pos) return;		/* Already been printed */
  if (err_pos==(char *)ARG1 && UNBNDP(obj)) err_pos = (char *)WNA;
#ifdef nosve
  if ((~0x1fL) & (short)err_pos) lputs(err_pos, cur_errp);
  else if (WNA > (short)err_pos) {
    lputs("Wrong type in arg", cur_errp);
    lputc((short)err_pos <= ARGn ? ' ' : '1' + (short)err_pos - ARG1, cur_errp);
  }
#else
  if ((~0x1fL) & (long)err_pos) lputs(err_pos, cur_errp);
  else if (WNA > (long)err_pos) {
    lputs("Wrong type in arg", cur_errp);
    lputc((long)err_pos <= ARGn ? ' ' : '1' + (int)err_pos - ARG1, cur_errp);
  }
#endif
  else lputs(errmsgs[((int)err_pos)-WNA].msg, cur_errp);
  lputs(((long)err_pos==WNA)?" given ":" ", cur_errp);
  err_pos = 0;
  if (!UNBNDP(obj))
    if (reset_safeport(sys_safep, 55, cur_errp))
      if (0==setjmp(SAFEP_JMPBUF(sys_safep))) {
	if (codep) scm_princode(obj, EOL, sys_safep, writing);
	else scm_iprin1(obj, sys_safep, writing);
      }
  if (UNBNDP(err_exp)) goto getout;
  if (NIMP(err_exp)) {
    if (reset_safeport(sys_safep, 55, cur_errp))
      if (0==setjmp(SAFEP_JMPBUF(sys_safep))) {
	lputs("\n; in expression: ", cur_errp);
	if (NCONSP(err_exp)) scm_princode(err_exp, env, sys_safep, writing);
	else if (UNDEFINED==CDR(err_exp))
	  scm_iprin1(CAR(err_exp), sys_safep, writing);
	else {
	  if (UNBNDP(env)) scm_iprlist("(... ", err_exp, ')', sys_safep, writing);
	  else scm_princode(err_exp, env, sys_safep, writing);
	}
      }
  }
  scm_scope_trace(env);
 getout:
#ifdef CAUTIOUS
  scm_stack_trace(UNDEFINED);
#endif
  lputc('\n', cur_errp);
  lfflush(cur_errp);
  err_exp = err_env = UNDEFINED;
  if (errjmp_bad) {
    lputs("\nFATAL ERROR DURING CRITICAL CODE SECTION: ", cur_errp);
    lputs(errjmp_bad, cur_errp);
    lputc('\n', cur_errp);
    lroom(BOOL_T);
#ifdef vms
    exit(EXIT_FAILURE);
#else
    exit(errno? (long)errno : EXIT_FAILURE);
#endif
  }
  errno = 0;
  ALLOW_INTS;
}
void everr(exp, env, arg, pos, s_subr, codep)
     SCM exp, env, arg;
     const char *pos, *s_subr;
     int codep;
{
  err_exp = exp;
  err_env = env;
  *loc_errobj = arg;
  err_pos = pos;
  err_s_subr = s_subr;
  errobj_codep = codep;
  if (errjmp_bad || errjmp_recursive) def_err_response();
  longjump(CONT(rootcont)->jmpbuf,
	   (~0x1fL) & (long)pos || (WNA > (long)pos) ?
	   COOKIE(1) : COOKIE((int)pos));
  /* will do error processing at stack base */
}
void wta(arg, pos, s_subr)
     SCM arg;
     const char *pos, *s_subr;
{
#ifndef RECKLESS
  everr(scm_trace, scm_trace_env, arg, pos, s_subr, 0);
#else
  everr(UNDEFINED, EOL, arg, pos, s_subr, 0);
#endif
}
void scm_experr(arg, pos, s_subr)
     SCM arg;
     const char *pos, *s_subr;
{
#ifndef RECKLESS
  everr(scm_trace, scm_trace_env, arg, pos, s_subr, !0);
#else
  everr(UNDEFINED, EOL, arg, pos, s_subr, !0);
#endif
}
SCM cur_input_port()
{
  return cur_inp;
}
SCM cur_output_port()
{
  return cur_outp;
}
SCM cur_error_port()
{
  return cur_errp;
}
char s_cur_inp[] = "set-current-input-port";
char s_cur_outp[] = "set-current-output-port";
char s_cur_errp[] = "set-current-error-port";
SCM set_inp(port)
     SCM port;
{
  SCM oinp;
  ASRTER(NIMP(port) && INPORTP(port), port, ARG1, s_cur_inp);
  DEFER_INTS;
  oinp = cur_inp;
  cur_inp = port;
  ALLOW_INTS;
  return oinp;
}
SCM set_outp(port)
     SCM port;
{
  SCM ooutp;
  ASRTER(NIMP(port) && OUTPORTP(port), port, ARG1, s_cur_outp);
  DEFER_INTS;
  ooutp = cur_outp;
  cur_outp = port;
  ALLOW_INTS;
  return ooutp;
}
SCM set_errp(port)
     SCM port;
{
  SCM oerrp;
  ASRTER(NIMP(port) && OUTPORTP(port), port, ARG1, s_cur_errp);
  DEFER_INTS;
  oerrp = cur_errp;
  cur_errp = port;
  ALLOW_INTS;
  return oerrp;
}
static char s_isatty[] = "isatty?";
SCM l_isatty(port)
     SCM port;
{
  int fn;
  ASRTER(NIMP(port) && OPPORTP(port), port, ARG1, s_isatty);
  if (tc16_fport != TYP16(port)) return BOOL_F;
  fn = fileno(STREAM(port));
  return (fn >= 0 && isatty(fn)) ? BOOL_T : BOOL_F;
}

static iproc subr0s[] = {
	{&s_cur_inp[4], cur_input_port},
	{&s_cur_outp[4], cur_output_port},
	{&s_cur_errp[4], cur_error_port},
	{"program-arguments", prog_args},
	{"line-number", line_num},
	{"abort", abrt},
	{s_restart, restart},
	{0, 0}};

static iproc subr1s[] = {
	{s_cur_inp, set_inp},
	{s_cur_outp, set_outp},
	{s_cur_errp, set_errp},
	{s_load_string, scm_load_string},
	{s_eval_string, scm_eval_string},
	{s_perror, lperror},
	{"make-arbiter", makarb},
	{s_tryarb, tryarb},
	{s_relarb, relarb},
	{s_isatty, l_isatty},
	{s_port_line, scm_port_line},
	{s_port_col, scm_port_col},
	{s_port_filename, scm_port_filename},
	{0, 0}};

static iproc subr1os[] = {
	{s_read_char, scm_read_char},
	{s_peek_char, scm_peek_char},
	{s_newline, scm_newline},
	{s_freshline, scm_freshline},
	{s_force_output, scm_force_output},
	{s_char_readyp, char_readyp},
	{"quit", quit},
	{"verbose", prolixity},
	{"errno", lerrno},
	{"room", lroom},
	{0, 0}};

static iproc subr2os[] = {
	{s_write, scm_write},
	{s_display, scm_display},
	{s_write_char, scm_write_char},
	{s_tryload, tryload},
	{s_file_position, scm_file_position},
#ifdef CAN_DUMP
	{s_unexec, scm_unexec},
#endif
	{0, 0}};

static smobfuns arbsmob = {markcdr, free0, prinarb};
char s_ccl[] = "char-code-limit";

void init_repl( iverbose )
     int iverbose;
{
	sysintern(s_ccl, MAKINUM(CHAR_CODE_LIMIT));
	i_repl = CAR(sysintern("repl", UNDEFINED));
	loc_errobj = &CDR(sysintern("errobj", UNDEFINED));
	loc_loadpath = &CDR(sysintern("*load-pathname*", BOOL_F));
	loc_loadsharp = &CDR(sysintern("load:sharp", UNDEFINED));
	loc_readsharp = &CDR(sysintern("read:sharp", UNDEFINED));
	loc_charsharp = &CDR(sysintern("char:sharp", UNDEFINED));
	loc_broken_pipe = &CDR(sysintern("broken-pipe", UNDEFINED));
	scm_verbose = iverbose;
	init_iprocs(subr0s, tc7_subr_0);
	init_iprocs(subr1os, tc7_subr_1o);
	init_iprocs(subr1s, tc7_subr_1);
	init_iprocs(subr2os, tc7_subr_2o);
	add_feature(s_char_readyp);
	make_subr(s_swapcar, tc7_subr_2, swapcar);
	make_subr(s_wfi, tc7_lsubr, wait_for_input);
#ifndef MEMOIZE_LOCALS
	p_read_numbered =
	  make_subr(s_read_numbered, tc7_subr_1, scm_read_numbered);
#endif
	p_read_for_load =
	  make_subr(s_read_for_load, tc7_subr_1, scm_read_for_load);
	p_read =
	  make_subr(s_read, tc7_subr_1o, scm_read);
	i_eval_string = CAR(sysintern(s_eval_string, UNDEFINED));
	i_load_string = CAR(sysintern(s_load_string, UNDEFINED));
	scm_ldstr("\n\
(define file-set-position file-position)\n\
");
#ifdef CAN_DUMP
	add_feature("dump");
	scm_ldstr("\
(define (dump file . thunk)\n\
  (cond ((null? thunk) (set! *interactive* #f) (set! *argv* #f))\n\
	((not (car thunk)) (set! *argv* #f))\n\
	((boolean? (car thunk)))\n\
	(else (set! boot-tail (lambda (t) ((car thunk))))))\n\
  (set! restart exec-self)\n\
  (require #f)\n\
  (unexec file))\n\
");
#endif
#ifdef ARM_ULIB
	set_erase();
#endif
	tc16_arbiter = newsmob(&arbsmob);
}
void final_repl()
{
  i_eval_string = i_load_string = 0;
  loc_errobj = (SCM *)&tmp_errobj;
  loc_loadpath = (SCM *)&tmp_loadpath;
  loadports = EOL;
}
