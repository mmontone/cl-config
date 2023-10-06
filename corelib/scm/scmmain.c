/* "scmmain.c" main() for SCM.
 * Copyright (C) 1990-1999 Free Software Foundation, Inc.
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

/* added by Dai Inukai 2001-03-21*/
#ifdef __FreeBSD__
# include <floatingpoint.h>
#endif

#ifdef _WIN32
# include <io.h>
#endif

#include "scm.h"
#include "patchlvl.h"

#ifdef __IBMC__
# include <io.h>
#endif
#ifdef __NetBSD__
# include <unistd.h>
#endif
#ifdef __OpenBSD__
# include <unistd.h>
#endif
#ifndef GENERIC_NAME
# define GENERIC_NAME "scm"
#endif
#ifndef INIT_GETENV
# define INIT_GETENV "SCM_INIT_PATH"
#endif

char *scm_find_implpath(execpath)
     const char *execpath;
{
  char *implpath = 0;
#ifndef nosve
# ifndef POCKETCONSOLE
  char *getenvpath = getenv(INIT_GETENV);
  /* fprintf(stderr, "%s=%s\n", INIT_GETENV, getenvpath); fflush(stderr); */
  if (getenvpath) implpath = scm_cat_path(0L, getenvpath, 0L);
  if (implpath) {/* The value of the environment variable supersedes
		    other locations, only if the file exists. */
    implpath = scm_try_path(implpath);
    if (!implpath) {
      fputs("Value of "INIT_GETENV" (=\"", stderr);
      fputs(getenvpath, stderr);
      fputs("\") not found; Trying elsewhere\n", stderr);
    }
  }
# endif
#endif
  if (!implpath && execpath)
    implpath = find_impl_file(execpath, GENERIC_NAME, INIT_FILE_NAME, dirsep);
#ifdef IMPLINIT
  if (!implpath) implpath = scm_cat_path(0L, IMPLINIT, 0L);
#endif
  return implpath;
}
const char * const generic_name[] = { GENERIC_NAME };

#ifdef WINSIGNALS
SCM_EXPORT HANDLE scm_hMainThread;
#endif

void scmmain_init_user_scm();

int main(argc, argv)
     int argc;
     const char **argv;
{
  char *script_arg = 0;		/* location of SCSH style script file or 0. */
  char *implpath = 0, **nargv;
  int nargc, iverbose = 0, buf0stdin;
  SCM retval;
/* added by Dai Inukai 2001-03-21 */
#ifdef __FreeBSD__
  fp_prec_t fpspec;
#endif

#ifdef WINSIGNALS
  /* need a handle to access the main thread from the signal handler thread */
  DuplicateHandle(GetCurrentProcess(), GetCurrentThread(), GetCurrentProcess(),
                  &scm_hMainThread, 0, TRUE, DUPLICATE_SAME_ACCESS);
#endif

/*    {char ** argvv = argv; */
/*    for (;*argvv;argvv++) {fputs(*argvv, stderr); fputs(" ", stderr);} */
/*    fputs("\n", stderr);} */

  init_user_scm = scmmain_init_user_scm;

  if (0==argc) {argc = 1; argv = generic_name;} /* for macintosh */
#ifndef LACK_SBRK
  init_sbrk();			/* Do this before malloc()s. */
#endif
/* added by Dai Inukai 2001-03-21 */
#ifdef __FreeBSD__
   fpspec = fpsetprec(FP_PE); /* IEEE 64 bit FP mantissa*/
#endif
  execpath = 0;			/* even when dumped */
  if ((nargv = script_process_argv(argc, argv))) { /* SCSH style scripts */
    script_arg = argv[2];	/* Save for scm_find_execpath() call */
    nargc = script_count_argv(nargv);
  }
  else {nargv = argv; nargc = argc;}
  /* execpath must be set to executable's path in order to use DUMP or DLD. */
  execpath = scm_find_execpath(nargc, nargv, script_arg);
  implpath = scm_find_implpath(execpath);
  if (isatty(fileno(stdin)) && isatty(fileno(stdout)))
    iverbose = (nargc <= 1) ? 2 : 1;
  buf0stdin = init_buf0(stdin);
  do {				/* You must call scm_init_from_argv()
				   or init_scm() to initialize SCM */
    scm_init_from_argv(nargc, nargv, script_arg, iverbose, buf0stdin);
    init_signals();		/* signals are optional */
				/* Now we are ready to run Scheme code! */
    retval = scm_top_level(implpath, 0L);
    restore_signals();		/* signals are optional */
				/* final_scm() when you are done with SCM. */
    if (retval) break;
    dumped = 0;
    if (2 <= iverbose) fputs(";RESTART\n", stderr);
    final_scm(!0);
  } while (!0);
  final_scm(
#ifdef CAREFUL_INTS
	    1
#else
	    1 /* freeall || (2 <= scm_verbose) */ /* Free storage when we're done. */
#endif
	    );
  if (2 <= iverbose) fputs(";EXIT\n", stderr);
  fflush(stderr);
  if (implpath) free(implpath);
  if (execpath) free(execpath);
  execpath = 0;
/* added by Dai Inukai 2001-03-27 */
#ifdef __FreeBSD__
  fpspec = fpsetprec(fpspec); /* Set back to FP_PD which is 53 bit FP. */
                              /* This may not be needed because the    */
                              /* kernel is set to FP_PD by default.    */
#endif
  return (int)INUM(retval);
}

/* init_user_scm() is called by the scheme procedure
   SCM_INIT_EXTENSIONS in "Init5xx.scm" */
void scmmain_init_user_scm()
{
  /* Put calls to your C initialization routines here. */
}
