/* "scm.c" Initialization and interrupt code.
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

#ifdef PLAN9
# define signal(a,b) 0/* no signals in Plan 9 */
#else
# include <signal.h>
#endif
#include "scm.h"
#include "patchlvl.h"

#ifdef _WIN32
# include <io.h>
#endif

#ifdef __IBMC__
# include <io.h>
#endif

/* See scm.h for definition of P */

#ifndef STDC_HEADERS
	int alarm P((unsigned int));
	int pause P((void));
	unsigned int sleep P((unsigned int seconds));
	char *getenv P((const char *name));
	int system P((const char *));
#else /* added by Denys Duchier */
# ifdef SVR4
#  include <unistd.h>
# endif
# ifdef __NetBSD__
#  include <unistd.h>
# endif
# ifdef __OpenBSD__
#  include <unistd.h>
# endif
# ifdef __amigaos__
#  include <unistd.h>
# endif
#endif

void	init_sbrk P((void));

void	init_dynl P((void));
void	init_edline P((void));
void	init_eval P((void));
void	init_features P((void));
void	init_gsubr P((void));
void	init_io P((void));
void	init_ioext P((void));
void	init_posix P((void));
void	init_ramap P((void));
void	init_record P((void));
void	init_rgx P((void));
void	init_rope P((void));
void	init_repl P((int iverbose));
void	init_sc2 P((void));
void	init_scl P((void));
void	init_signals P((void));
void	init_socket P((void));
void	init_subrs P((void));
void	init_tables P((void));
void	init_time P((void));
void	init_types P((void));
void	init_unif P((void));
void	init_debug P((void));
void	reset_time P((void));
void	final_repl P((void));

void init_banner()
{
  fputs("SCM version "SCMVERSION", Copyright (C) 1990-2006 \
Free Software Foundation.\n\
SCM comes with ABSOLUTELY NO WARRANTY; for details type `(terms)'.\n\
This is free software, and you are welcome to redistribute it\n\
under certain conditions; type `(terms)' for details.\n", stderr);
}

void scm_init_INITS()
{
  if (!dumped) {
#ifdef INITS
    INITS;			/* call initialization of extension files */
#endif
  }
}

void (*init_user_scm) P((void));
SCM scm_init_extensions()
{
#ifdef COMPILED_INITS
  COMPILED_INITS;		/* initialize statically linked add-ons */
#endif
  init_user_scm();
#ifndef HAVE_DYNL
  /* No more init_*s, so trim gra[]s */
  scm_trim_gra(&subrs_gra);
  scm_trim_gra(&ptobs_gra);
  scm_trim_gra(&smobs_gra);
  scm_trim_gra(&finals_gra);
#endif
  return UNSPECIFIED;
}

#if (__TURBOC__==1)
# define signal ssignal		/* Needed for TURBOC V1.0 */
#endif

/* SIGRETTYPE is the type that signal handlers return.  See <signal.h>*/

#ifdef RETSIGTYPE
# define SIGRETTYPE RETSIGTYPE
#else
# ifdef STDC_HEADERS
#  if (__TURBOC__==1)
#   define SIGRETTYPE int
#  else
#   define SIGRETTYPE void
#  endif
# else
#  ifdef linux
#   define SIGRETTYPE void
#  else
#   define SIGRETTYPE int
#  endif
# endif
#endif

#ifdef vms
# ifdef __GNUC__
#  define SIGRETTYPE int
# endif
#endif

#define SIGNAL_BASE HUP_SIGNAL
/* PROF_SIGNAL appears below because it is the last signal
   defined in scm.h and in errmsgs in repl.c  */
static struct {
  int signo; SIGRETTYPE (*osig)(); SIGRETTYPE (*nsig)();
} sigdesc[PROF_SIGNAL - SIGNAL_BASE + 1];

#define NUM_SIGNALS (sizeof(sigdesc)/sizeof(sigdesc[0]))

void process_signals()
{
  int i, n;
  unsigned long mask = 1L;
  /* printf("process_signals; output_deferred=%d\n", output_deferred); fflush(stdout); */
  if (output_deferred) {
    output_deferred = 0;
    if (NIMP(sys_errp) && OPOUTPORTP(sys_errp)) lfflush(sys_errp);
  }
  for (n = 0; SIG_deferred && n < NUM_SIGNALS; n++) {
    if (SIG_deferred & mask) {
      i = n + SIGNAL_BASE;
      SIG_deferred &= ~mask;
      if (i != handle_it(i))
	wta(UNDEFINED, (char *)i, "");
    }
    mask <<= 1;
  }
  if (gc_hook_pending) {
    gc_hook_pending = 0;
    scm_gc_hook();
  }
  deferred_proc = 0;
}


#ifdef WINSIGNALS
SCM_EXPORT HANDLE scm_hMainThread;
HANDLE scm_hMainThread;
static SIGRETTYPE scmable_signal(int sig);
# ifdef __MINGW32__
static void sigintstub();
__asm(".globl _sigintstub");
__asm("_sigintstub:");
__asm("        pushl $2");
__asm("        call _scmable_signal");
__asm("        addl $4, %esp");
__asm("        popal");
__asm("        popfl");
__asm("        ret");
# else   /* works for Microsoft VC++ */
static __declspec(naked) void sigintstub()
{
	scmable_signal(SIGINT);
    __asm popad;
    __asm popfd;
    __asm ret;
}
# endif /* def __MINGW32__ */

/* control-c signal handler */
SIGRETTYPE  win32_sigint(int sig)
{
	CONTEXT ctx;
	DWORD *Stack;

	if (-1 == SuspendThread(scm_hMainThread))
		return;

	ctx.ContextFlags = CONTEXT_FULL;
	if (0 == GetThreadContext(scm_hMainThread, &ctx))
	{
		ResumeThread(scm_hMainThread);
		return;
	}

	Stack = (DWORD *)ctx.Esp;

	*--Stack = ctx.Eip;
	*--Stack = ctx.EFlags;
	*--Stack = ctx.Eax;
	*--Stack = ctx.Ecx;
	*--Stack = ctx.Edx;
	*--Stack = ctx.Ebx;
	*--Stack = ctx.Esp;
	*--Stack = ctx.Ebp;
	*--Stack = ctx.Esi;
	*--Stack = ctx.Edi;
	ctx.Esp = (DWORD)Stack;

	ctx.Eip = (DWORD)sigintstub;

	SetThreadContext(scm_hMainThread, &ctx);
	ResumeThread(scm_hMainThread);
}

#endif /*def WINSIGNALS*/


static char s_unksig[] = "unknown signal";
static SIGRETTYPE err_signal(sig)
     int sig;
{
  int i = NUM_SIGNALS;
  signal(sig, err_signal);
  while (i--)
    if (sig == sigdesc[i].signo) break;
  wta(MAKINUM(sig), (i < 0 ? s_unksig : (char *)(i + SIGNAL_BASE)), "");
}

static SIGRETTYPE scmable_signal(sig)
     int sig;
{
  int oerr = errno;
  int i = NUM_SIGNALS;
  while (i--)
    if (sig == sigdesc[i].signo) break;
  ASRTER(i >= 0, MAKINUM(sig), s_unksig, "");
#ifdef WINSIGNALS
  if (SIGINT == sig) signal(sig, win32_sigint);
  else
#endif
    signal(sig, scmable_signal);
  if (ints_disabled) {
    deferred_proc = process_signals;
    SIG_deferred |= (1L << i);
  }
  else {
#ifdef SIG_UNBLOCK
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, sig);
    sigprocmask(SIG_UNBLOCK, &set, 0L);
#endif
    SIG_deferred &= ~(1L << i);
    i += SIGNAL_BASE;
    if (i != handle_it(i)) {
      errno = oerr;
      wta(UNDEFINED, (char *)i, "");
    }
  }
  errno = oerr;
}



/* If doesn't have SIGFPE, disable FLOATS for the rest of this file. */

#ifndef SIGFPE
# undef FLOATS
#endif

#ifdef macintosh
# undef SIGALRM
#endif
#ifdef atarist
# undef SIGALRM			/* only available via MiNT libs */
#endif
#ifdef __HIGHC__
# undef SIGALRM
#endif
#ifdef LACK_SETITIMER
# undef SIGPROF
#endif
#ifdef SIGALRM
static char s_alarm[] = "alarm";
SCM lalarm(i)
     SCM i;
{
  unsigned int j;
  ASRTER(INUMP(i) && (INUM(i) >= 0), i, ARG1, s_alarm);
  SYSCALL(j = alarm(INUM(i)););
  return MAKINUM(j);
}
# ifdef SIGPROF
#  include <sys/time.h>
static char s_setitimer[] = "setitimer";
static struct {SCM sym; int which;} setitimer_tab[3] = {
  {UNDEFINED, 0}, {UNDEFINED, 0}, {UNDEFINED, 0}};
/* VALUE and INTERVAL are milliseconds */
SCM scm_setitimer(which, value, interval)
     SCM which, value, interval;
{
  struct itimerval tval, oval;
  int w;
  int i = sizeof(setitimer_tab)/sizeof(setitimer_tab[0]);
  while (i--) {
    if (which==setitimer_tab[i].sym) {
      w = setitimer_tab[i].which;
      if (BOOL_T==value)
	SYSCALL(w = getitimer(w, &oval););
      else {
	if (BOOL_F==value) value = INUM0;
	ASRTER(INUMP(value), value, ARG2, s_setitimer);
	if (BOOL_F==interval) interval = INUM0;
	ASRTER(INUMP(interval), interval, ARG3, s_setitimer);
	tval.it_value.tv_sec = INUM(value) / 1000;
	tval.it_value.tv_usec = (INUM(value) % 1000)*1000;
	tval.it_interval.tv_sec = INUM(interval) / 1000;
	tval.it_interval.tv_usec = (INUM(interval) % 1000)*1000;
	SYSCALL(w = setitimer(w, &tval, &oval););
      }
      if (w) return BOOL_F;
      return cons2(MAKINUM(oval.it_value.tv_usec/1000 +
			   oval.it_value.tv_sec*1000),
		   MAKINUM(oval.it_interval.tv_usec/1000 +
			   oval.it_interval.tv_sec*1000),
		   EOL);
    }
  }
  return BOOL_F;
}
# endif
# ifndef AMIGA
SCM l_pause()
{
  pause();
  return UNSPECIFIED;
}
# endif
#endif /* SIGALRM */

#ifdef _WIN32
# include <windows.h>
#endif
#ifdef __IBMC__
# include <os2.h>
#endif
#ifndef AMIGA
# ifndef _Windows
static char s_sleep[] = "sleep";
SCM l_sleep(i)
     SCM i;
{
  unsigned int j = 0;
  ASRTER(INUMP(i) && (INUM(i) >= 0), i, ARG1, s_sleep);
#  ifdef __HIGHC__
  SYSCALL(sleep(INUM(i)););
#  else
#   ifdef _WIN32
  Sleep(INUM(i) * 1000);
#   else
#    ifdef __IBMC__
  DosSleep(INUM(i) * 1000);
#    else
  SYSCALL(j = sleep(INUM(i)););
#    endif
#   endif
#  endif
  return MAKINUM(j);
}
# endif
#endif

#ifdef PLAN9
int raise(sig)
     int sig;
{
  char *str;
  int len;
  char ibuf[12];
  char pidbuf[32];
  int fd;
  int res;
  sprint(ibuf, "%ld", sig);
  len = strlen(ibuf);
  sprint(pidbuf, "/proc/%d/note", getpid());
  fd = open(pidbuf, OWRITE);
  res = write(fd, ibuf, len);
  close (fd);
  return res==len;
}
#endif

#ifndef _WIN32
# ifndef sun
#  ifndef THINK_C
#   ifndef __TURBOC__
#    ifdef STDC_HEADERS
#     ifndef __MWERKS__
#      ifndef __IBMC__
#       ifndef PLAN9
#        define LACK_RAISE
#       endif
#      endif
#     endif
#    endif
#   endif
#  endif
# endif
#endif
/* int raise P((int sig)); */
static char s_raise[] = "raise";
SCM l_raise(sig)
     SCM sig;
{
  ASRTER(INUMP(sig), sig, ARG1, s_raise);
#ifdef LACK_RAISE
# ifdef vms
  return MAKINUM(gsignal((int)INUM(sig)));
# else
  return kill(getpid (), (int)INUM(sig)) ? BOOL_F : BOOL_T;
# endif
#else
  return raise((int)INUM(sig)) ? BOOL_F : BOOL_T;
#endif
}

#ifdef TICKS
unsigned int tick_count = 0, ticken = 0;
SCM *loc_tick_signal;
void tick_signal()
{
  if (ticken && NIMP(*loc_tick_signal)) {
    ticken = 0;
    apply(*loc_tick_signal, EOL, EOL);
  }
}
static char s_ticks[] = "ticks";
SCM lticks(i)
     SCM i;
{
  SCM j = ticken ? tick_count : 0;
  if (!UNBNDP(i)) ticken = tick_count = INUM(i);
  return MAKINUM(j);
}
#endif

#ifdef SIGPIPE
static SIGRETTYPE (*oldpipe) ();
#endif

int case_sensitize_symbols = 0;	/* set to 8 to read case-sensitive symbols */
int dumped = 0;			/* Is this an invocation of unexec exe? */

#ifdef SHORT_ALIGN
typedef short STACKITEM;
#else
typedef long STACKITEM;
#endif
/* See scm.h for definition of P */
void  init_storage P((STACKITEM *stack_start_ptr, long init_heap_size));

void init_scm(iverbose, buf0stdin, init_heap_size)
     int iverbose;
     int buf0stdin;
     long init_heap_size;
{
  STACKITEM i;
  if (2 <= iverbose) init_banner();
  if (!dumped) {
    init_types();
    init_tables();
    init_storage(&i, init_heap_size); /* CONT(rootcont)->stkbse gets set here */
  } else {
    /* The streams when the program was dumped need to be reset. */
    SETSTREAM(def_inp, stdin);
    SETSTREAM(def_outp, stdout);
    SETSTREAM(def_errp, stderr);
  }
  if (buf0stdin) SCM_PORTFLAGS(def_inp) |= BUF0;
  else SCM_PORTFLAGS(def_inp) &= ~BUF0;
  if (!dumped) {
    init_features();
    init_subrs();
    init_scl();
    init_unif();
    init_time();
    init_io();
    init_eval();		/* call to scm_evstr switches INTS discipline */
    init_debug();
    init_rope();
    init_repl(iverbose);
  }
  else reset_time();
#ifdef HAVE_DYNL
  /* init_dynl() must check dumped to avoid redefining subrs */
  init_dynl();
#endif
}

static void init_sig1(scm_err, signo, handler)
     int scm_err;
     int signo;
     SIGRETTYPE (*handler)();
{
  int i = scm_err - SIGNAL_BASE;
  ASRTER(i < NUM_SIGNALS, MAKINUM(i), OUTOFRANGE, "init_sig1");
  sigdesc[i].signo = signo;
  sigdesc[i].nsig = handler;
  sigdesc[i].osig = signal(signo, handler);
}
void init_signals()
{
#ifdef WINSIGNALS
    /* Added to allow gcc -O2 to work. */
  volatile unsigned long dont_optimize_me = (unsigned long)scmable_signal;
  init_sig1(INT_SIGNAL, SIGINT, win32_sigint);
#else
# ifdef SIGINT
  init_sig1(INT_SIGNAL, SIGINT, scmable_signal);
# endif
#endif
#ifdef SIGHUP
  init_sig1(HUP_SIGNAL, SIGHUP, scmable_signal);
#endif
#ifdef FLOATS
  init_sig1(FPE_SIGNAL, SIGFPE, err_signal);
#endif
#ifdef SIGBUS
  init_sig1(BUS_SIGNAL, SIGBUS, err_signal);
#endif
#ifdef SIGSEGV			/* AMIGA lacks! */
  init_sig1(SEGV_SIGNAL, SIGSEGV, err_signal);
#endif
#ifdef SIGALRM
  alarm(0);			/* kill any pending ALRM interrupts */
  init_sig1(ALRM_SIGNAL, SIGALRM, scmable_signal);
# ifdef SIGPROF
  init_sig1(PROF_SIGNAL, SIGPROF, scmable_signal);
# endif
# ifdef SIGVTALRM
  init_sig1(VTALRM_SIGNAL, SIGVTALRM, scmable_signal);
# endif
#endif
#ifdef SIGPIPE
  oldpipe = signal(SIGPIPE, SIG_IGN);
#endif
#ifdef ultrix
  siginterrupt(SIGINT, 1);
  siginterrupt(SIGALRM, 1);
  siginterrupt(SIGHUP, 1);
  siginterrupt(SIGPIPE, 1);
#endif /* ultrix */
}
/* This is used in preparation for a possible fork().  Ignore all
   signals before the fork so that child will catch only if it
   establishes a handler */
void ignore_signals()
{
  int i = NUM_SIGNALS;
#ifdef ultrix
  siginterrupt(SIGINT, 0);
  siginterrupt(SIGALRM, 0);
  siginterrupt(SIGHUP, 0);
  siginterrupt(SIGPIPE, 0);
#endif /* ultrix */
  while (i--)
    if (sigdesc[i].signo)
      signal(sigdesc[i].signo, SIG_DFL);
  /* Some documentation claims that ALRMs are cleared accross forks.
     If this is not always true then the value returned by alarm(0)
     will have to be saved and unignore_signals() will have to
     reinstate it. */
  /* This code should be necessary only if the forked process calls
     alarm() without establishing a handler: */
#ifdef SIGALRM
  /* oldalrm = signal(SIGALRM, SIG_DFL); */
#endif
  /* These flushes are per warning in man page on fork(). */
  fflush(stdout);
  fflush(stderr);
#ifdef SIG_UNBLOCK
  {
    sigset_t set;
    sigfillset(&set);
    sigprocmask(SIG_UNBLOCK, &set, 0L);
  }
#endif
}

void unignore_signals()
{
  int i = NUM_SIGNALS;
  while (i--)
    if (sigdesc[i].signo)
      signal(sigdesc[i].signo, sigdesc[i].nsig);
#ifdef ultrix
  siginterrupt(SIGINT, 1);
  siginterrupt(SIGALRM, 1);
  siginterrupt(SIGHUP, 1);
  siginterrupt(SIGPIPE, 1);
#endif /* ultrix */
}

void restore_signals()
{
  int i;
#ifdef ultrix
  siginterrupt(SIGINT, 0);
  siginterrupt(SIGALRM, 0);
  siginterrupt(SIGHUP, 0);
  siginterrupt(SIGPIPE, 0);
#endif /* ultrix */
#ifdef SIGALRM
# ifndef SIGPROF
  alarm(0);			/* kill any pending ALRM interrupts */
# else
  i = sizeof(setitimer_tab)/sizeof(setitimer_tab[0]);
  while (i--)
    if (NIMP(setitimer_tab[i].sym))
      scm_setitimer(setitimer_tab[i].sym, BOOL_F, BOOL_F);
# endif
#endif
  i = NUM_SIGNALS;
  while (i--)
    if (sigdesc[i].signo)
      signal(sigdesc[i].signo, sigdesc[i].osig);
#ifdef SIGPIPE
  oldpipe = signal(SIGPIPE, SIG_IGN);
#endif
}

void scm_init_from_argv(argc, argv, script_arg, iverbose, buf0stdin)
     int argc;
     const char * const *argv;
     char *script_arg;
     int iverbose;
     int buf0stdin;
{
  long i = 0L;
  int j = 0;
  if ((2 <= argc) && argv[1] && (0==strncmp("-a", argv[1], 2)))
    i = atol((0==argv[1][2] && 3 <= argc && argv[2]) ? argv[2] : &argv[1][2]);
  init_scm(iverbose, buf0stdin, (0 >= i) ? 0L : 1024L * i); /* size in kB */
  for (j = 0; argv[j]; j++) {
    if (0==strcmp(argv[j], "--no-symbol-case-fold")) {
      case_sensitize_symbols = 8;
      break;
    }
  }
  progargs = EOL;
  progargs = makfromstrs(argc, argv);
  sysintern("*script*", script_arg ? makfrom0str(script_arg) : BOOL_F);
}
void final_scm(freeall)
     int freeall;
{
#ifdef TICKS
  ticken = 0;
#endif
  scm_run_finalizers(!0);
#ifdef FINALS
  FINALS;			/* call shutdown of extensions files */
#endif /* for compatability with older modules */
  /* call finalization of user extensions */
  {
    int k = num_finals;
    while (k--) (finals[k])();
  }
  final_repl();
  if (freeall) free_storage();	/* free all allocated memory */
}

#ifdef PLAN9
# define SYSTNAME "plan9"
# define DIRSEP "/"
#endif
#ifdef __MACH__
# define SYSTNAME "unix"
# define DIRSEP "/"
#endif
#ifdef __CYGWIN__
# define SYSTNAME "unix"
# define DIRSEP "/"
#endif
#ifdef vms
# define SYSTNAME "vms"
#endif
#ifdef HAVE_UNIX
# define DIRSEP "/"
# ifndef MSDOS			/* DJGPP defines both */
#  define SYSTNAME "unix"
# endif
#endif
#ifdef MWC
# define SYSTNAME "coherent"
# define DIRSEP "/"
#endif
#ifdef _Windows
# define SYSTNAME "windows"
# define DIRSEP "\\"
#else
# ifdef MSDOS
#  define SYSTNAME "ms-dos"
#  ifndef HAVE_UNIX
#   define DIRSEP "\\"
#  endif
# endif
#endif
#ifdef __EMX__
# define SYSTNAME "os/2"
# define DIRSEP "\\"
#endif
#ifdef __IBMC__
# define SYSTNAME "os/2"
# define DIRSEP "\\"
#endif
#ifdef THINK_C
# define SYSTNAME "thinkc"
# define DIRSEP ":"
#endif
#ifdef __MWERKS__
# define SYSTNAME "macos"
# define DIRSEP ":"
#endif
#ifdef AMIGA
# define SYSTNAME "amiga"
# define DIRSEP "/"
#endif
#ifdef atarist
# define SYSTNAME "atarist"
# define DIRSEP "\\"
#endif
#ifdef ARM_ULIB
# define SYSTNAME "acorn"
#endif
#ifdef nosve
# define DIRSEP "."
#endif
#ifdef __amigaos__
# define SYSTNAME "amiga"
# define DIRSEP "/"
#endif
#ifdef __NetBSD__
# define SYSTNAME "unix"
# define DIRSEP "/"
#endif
const char dirsep[] = DIRSEP;

SCM softtype()
{
#ifdef nosve
  return CAR(sysintern("nosve", UNDEFINED));
#else
  return CAR(sysintern(SYSTNAME, UNDEFINED));
#endif
}

#ifdef PLAN9
/* This code is adapted from /sys/src/ape/lib/ap/plan9/isatty.c. */
int isatty (int fd)
{
  Dir d1, d2;
  char buf[40];
  int t;
  if (dirfstat(fd, &d1) < 0) return 0;
  if (strncmp(d1.name, "ptty", 4) == 0) return 1;
  if (dirstat("/dev/cons", &d2) < 0) return 0;
  /* If we came in through con, /dev/cons is probably #d/0, which won't
   * match stdin.  Opening #d/0 and fstating it gives the values of the
   * underlying channel */
  if (d2.type == 'd') {
    strcpy(buf, "#d/");
    strcpy(buf+3, d2.name);
    if ((t = open(buf, 0)) < 0) return 0;
    if (dirfstat(t, &d2) < 0) {
      close(t);
      return 0;
    }
    close(t);
  }
  return (d1.type == d2.type) && (d1.dev == d2.dev);
}

/* A temporary hack: give SCM our own errno. */
int errno;
#endif

int init_buf0(inport)
     FILE *inport;
{
  if (isatty(fileno(inport))) {
#ifndef NOSETBUF
# ifndef _DCC
#  ifndef ultrix
#   ifndef __WATCOMC__
#    ifndef macintosh
#     if (__TURBOC__ != 1)
#      ifndef _Windows
    setbuf(inport, 0L);		/* Often setbuf isn't actually required */
#      endif
#     endif
#    endif
#   endif
#  endif
# endif
#endif
    return !0;			/* stdin gets marked BUF0 in init_scm() */
  }
  return 0;
}

char *execpath = 0;
char s_no_execpath[] = "no execpath";
#define s_execpath (s_no_execpath+3)
SCM scm_execpath(newpath)
     SCM newpath;
{
  SCM retval = execpath ? makfrom0str(execpath) : BOOL_F;
  if (UNBNDP(newpath))
    return retval;
  if (FALSEP(newpath)) {
    if (execpath) free(execpath);
    execpath = 0;
    return retval;
  }
  ASRTER(NIMP(newpath) && STRINGP(newpath), newpath, ARG1, s_execpath);
  if (execpath) free(execpath);
  execpath = (char *)malloc((sizet)(LENGTH(newpath) + 1));
  ASRTER(execpath, newpath, NALLOC, s_execpath);
  strncpy(execpath, CHARS(newpath), LENGTH(newpath) + 1);
  return retval;
}
/* Return 0 if getcwd() returns 0. */
char *scm_find_execpath(argc, argv, script_arg)
     int argc;
     const char * const *argv;
     const char *script_arg;
{
  char *exepath = 0;
#ifndef macintosh
# ifdef HAVE_UNIX
#  ifndef MSDOS
  if (script_arg) exepath = script_find_executable(script_arg);
#  endif
# endif
  if (!exepath && argv[0]) exepath = dld_find_executable(argv[0]);
/*fprintf(stderr, "scm_find_execpath: argv[0] = %s; script_arg = %s; exepath = %s\n", argv[0], script_arg, exepath); fflush(stderr); */
#endif
  return exepath;
}

#ifdef PLAN9
int system(command)
     const char *command;
{
  int sts;
  int pid = fork();
  if (pid) {
    Waitmsg wm;
    sts = -1;
    while (wait(&wm) != -1) {
      if (pid==atoi(wm.pid)) {
	sts = 0;
	break;
      }
    }
  } else sts = execl("/bin/rc", "/bin/rc", "-c", command, nil);
  return sts;
}
#endif

#ifndef _Windows
char s_system[] = "system";
SCM lsystem(cmd)
     SCM cmd;
{
  ASRTER(NIMP(cmd) && STRINGP(cmd), cmd, ARG1, s_system);
  ignore_signals();
# ifdef AZTEC_C
  cmd = MAKINUM(Execute(CHARS(cmd), 0, 0));
# else
  cmd = MAKINUM(0L+system(CHARS(cmd)));
# endif
  unignore_signals();
  return cmd;
}
#endif

extern char **environ;		/* The Linux man page says this
				   declaration is necessary. */
char s_getenv[] = "getenv";
char *getenv();
SCM scm_getenv(nam)
     SCM nam;
{
  char *val;
  if (UNBNDP(nam)) {
    char **nvrnmnt = environ;
    SCM lst = EOL;
    do {
      char *eql = strchr(*nvrnmnt, '=');
      ASRTER(eql, makfrom0str(*nvrnmnt), "Bad environ", s_getenv);
      lst = cons(cons(makfromstr(*nvrnmnt, eql - *nvrnmnt),
		      makfrom0str(eql + 1)),
		 lst);
    } while (*++nvrnmnt);
    return lst;
  }
  ASRTER(NIMP(nam) && STRINGP(nam), nam, ARG1, s_getenv);
  val = getenv(CHARS(nam));
  if (!val) return BOOL_F;
  return makfrom0str(val);
}

#ifdef vms
# include <descrip.h>
# include <ssdef.h>
char s_ed[] = "ed";
SCM ed(fname)
     SCM fname;
{
  struct dsc$descriptor_s d;
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_ed);
  d.dsc$b_dtype = DSC$K_DTYPE_T;
  d.dsc$b_class = DSC$K_CLASS_S;
  d.dsc$w_length = LENGTH(fname);
  d.dsc$a_pointer = CHARS(fname);
  /* I don't know what VMS does with signal handlers across the
     edt$edit call. */
  ignore_signals();
  edt$edit(&d);
  unignore_signals();
  return fname;
}
SCM vms_debug()
{
  lib$signal(SS$_DEBUG);
  return UNSPECIFIED;
}
#endif

static iproc subr0s[] = {
	{"software-type", softtype},
	{"scm_init_extensions", scm_init_extensions},
#ifdef vms
	{"vms-debug", vms_debug},
#endif
#ifdef SIGALRM
# ifndef AMIGA
	{"pause", l_pause},
# endif
#endif
	{0, 0}};
static iproc subr1s[] = {
#ifndef _Windows
	{s_system, lsystem},
#endif
#ifdef vms
	{s_ed, ed},
#endif
#ifdef SIGALRM
	{s_alarm, lalarm},
#endif
#ifndef AMIGA
# ifndef _Windows
	{s_sleep, l_sleep},
# endif
#endif
#ifndef sun
# ifndef _WIN32
	{s_raise, l_raise},
# endif
#endif
	{0, 0}};

SCM *loc_features;
void add_feature(str)
     char* str;
{
  *loc_features = cons(CAR(sysintern(str, UNDEFINED)), *loc_features);
}
void init_features()
{
  loc_features = &CDR(sysintern("slib:features", EOL));
  init_iprocs(subr0s, tc7_subr_0);
  init_iprocs(subr1s, tc7_subr_1);
  make_subr(s_execpath, tc7_subr_1o, scm_execpath);
  make_subr(s_getenv, tc7_subr_1o, scm_getenv);
#ifdef SIGALRM
# ifdef SIGPROF
  make_subr(s_setitimer, tc7_subr_3, scm_setitimer);
#  ifdef ITIMER_REAL
  setitimer_tab[0].sym = CAR(sysintern("real", UNDEFINED));
  setitimer_tab[0].which = ITIMER_REAL;
#  endif
#  ifdef ITIMER_VIRTUAL
  setitimer_tab[1].sym = CAR(sysintern("virtual", UNDEFINED));
  setitimer_tab[1].which = ITIMER_VIRTUAL;
#  endif
#  ifdef ITIMER_PROF
  setitimer_tab[2].sym = CAR(sysintern("profile", UNDEFINED));
  setitimer_tab[2].which = ITIMER_PROF;
#  endif
# endif
#endif
#ifdef TICKS
  loc_tick_signal = &CDR(sysintern("ticks-interrupt", UNDEFINED));
  make_subr(s_ticks, tc7_subr_1o, lticks);
#endif
#ifdef RECKLESS
  add_feature("reckless");
#endif
#ifndef _Windows
  add_feature(s_system);
#endif
#ifdef vms
  add_feature(s_ed);
#endif
  sysintern("*scm-version*", CAR(sysintern(SCMVERSION, UNDEFINED)));
}
