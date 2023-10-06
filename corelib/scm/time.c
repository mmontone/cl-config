/* "time.c" functions dealing with time.
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

#ifdef STDC_HEADERS
# include <time.h>
# ifdef M_SYSV
#  include <sys/types.h>
#  include <sys/times.h>
# endif
# ifdef sun
#  include <sys/types.h>
#  include <sys/times.h>
# endif
# ifdef ultrix
#  include <sys/types.h>
#  include <sys/times.h>
# endif
# ifdef nosve
#  include <sys/types.h>
#  include <sys/times.h>
# endif
# ifdef _UNICOS
#  include <sys/types.h>
#  include <sys/times.h>
# endif
# ifdef __IBMC__
#  include <sys/timeb.h>
# endif
#else
# ifdef SVR2
#  include <time.h>
# else
#  ifndef ARM_ULIB
#   include <sys/time.h>
#  else
#   include <time.h>
#  endif
# endif
# include <sys/types.h>

# ifndef ARM_ULIB
#  include <sys/times.h>
# else
#  include <time.h>
# endif

#endif

/* Define this if your system lacks ftime(). */
/* #define LACK_FTIME */
/* Define this if your system has gettimeofday()
   (LACK_FTIME should not be defined). */
/* #define USE_GETTIMEOFDAY */
/* Define this if your system lacks times(). */
/* #define LACK_TIMES */

#ifdef linux
# include <sys/types.h>
# include <sys/time.h>
# include <sys/timeb.h>
# include <sys/times.h>
# include <unistd.h>
# define CLKTCK (sysconf(_SC_CLK_TCK))
# define USE_GETTIMEOFDAY
#endif
#ifdef __MACH__
# define unix
# include <sys/types.h>
# include <sys/time.h>
# include <sys/timeb.h>
# include <sys/times.h>
# define USE_GETTIMEOFDAY
#endif
#ifdef __FreeBSD__
# include <sys/types.h>
# include <sys/time.h>
# include <sys/timeb.h>
# define USE_GETTIMEOFDAY
#endif
#ifdef __NetBSD__
# include <sys/timeb.h>
# include <sys/times.h>
# define USE_GETTIMEOFDAY
#endif
#ifdef __OpenBSD__
# include <sys/types.h>
# include <sys/time.h>
# define USE_GETTIMEOFDAY
#endif
#ifdef __TURBOC__
# define LACK_TIMES
#endif
#if (__TURBOC__==1) /* Needed for TURBOC V1.0 */
# define LACK_FTIME
# undef MSDOS
#endif
#ifdef __HIGHC__
# define LACK_TIMES
#endif
#ifdef macintosh
# define LACK_FTIME
# define LACK_TIMES
# define CLK_TCK 60
#endif
#ifdef SVR2
# define LACK_FTIME
#endif
#ifdef SVR4
# define LACK_FTIME
#endif
#ifdef __SVR4
# define LACK_FTIME
#endif
#ifdef PLAN9
# define LACK_FTIME
# define LACK_TIMES
#endif
#ifdef nosve
# define LACK_FTIME
#endif
#ifdef GO32
# define LACK_FTIME
# define LACK_TIMES
#endif
#ifdef atarist
# define LACK_FTIME
# define LACK_TIMES
#endif
#ifdef ARM_ULIB
# define LACK_FTIME
# define LACK_TIMES
#endif
#ifdef _DCC
# define LACK_FTIME
#endif
#ifdef MSDOS
# ifndef GO32
#  include <sys/types.h>
#  include <sys/timeb.h>
# endif
#endif
#ifdef _UNICOS
# define LACK_FTIME
#endif
#ifdef __amigaos__
# include <sys/time.h>
# include <sys/timeb.h>
# include <sys/times.h>
# define USE_GETTIMEOFDAY
#endif

#ifndef LACK_FTIME
# ifdef HAVE_UNIX
#  ifndef GO32
#   include <sys/timeb.h>
#  endif
# endif
#endif

#ifdef __EMX__
# define LACK_TIMES
# include <sys/types.h>
# include <sys/timeb.h>
#endif

#ifdef MWC
# include <time.h>
# include <sys/timeb.h>
#endif

#ifdef ARM_ULIB
# include <sys/types.h>
# include <time.h>
#endif

#ifdef vms
# define LACK_TIMES
# define LACK_FTIME
#endif

#ifndef CLKTCK
# ifdef CLK_TCK
#  define CLKTCK CLK_TCK
#  ifdef CLOCKS_PER_SEC
#   ifdef HAVE_UNIX
#    ifndef ARM_ULIB
#     include <sys/times.h>
#    endif
#    define LACK_CLOCK
    /* This is because clock() might be POSIX rather than ANSI.
       This occurs on HP-UX machines */
#   endif
#  endif
# else
#  ifdef CLOCKS_PER_SEC
#   define CLKTCK CLOCKS_PER_SEC
#  else
#   define LACK_CLOCK
#   ifdef AMIGA
#    include <stddef.h>
#    define LACK_TIMES
#    define LACK_FTIME
#    define CLKTCK 1000
#   else
#    define CLKTCK 60
#   endif
#  endif
# endif
#endif

#ifdef __STDC__
# define timet time_t
#else
# define timet long
#endif

#ifdef LACK_TIMES
# ifdef LACK_CLOCK
#  ifdef AMIGA
/* From: "Fred Bayer" <bayerf@lan.informatik.tu-muenchen.de> */
#   ifdef AZTEC_C		/* AZTEC_C */
#    include <devices/timer.h>
static long mytime()
{
	long sec, mic, mili = 0;
	struct timerequest *timermsg;
	struct MsgPort *timerport;
	if (!(timerport = (struct MsgPort *)CreatePort(0, 0))){
	lputs("No mem for port.\n", cur_errp);
		return mili;
	}
	if (!(timermsg = (struct timerequest *)
		 CreateExtIO(timerport, sizeof(struct timerequest)))){
		lputs("No mem for timerequest.\n", cur_errp);
		DeletePort(timermsg->tr_node.io_Message.mn_ReplyPort);
	return mili;
	}
	if (!(OpenDevice(TIMERNAME, UNIT_MICROHZ, timermsg, 0))){
		timermsg->tr_node.io_Command = TR_GETSYSTIME;
		timermsg->tr_node.io_Flags = 0;
		DoIO(timermsg);
		sec = timermsg->tr_time.tv_secs;
		mic = timermsg->tr_time.tv_micro;
		mili = sec*1000+mic/1000;
		CloseDevice(timermsg);
	}
	else lputs("No Timer available.\n", cur_errp);
	DeletePort(timermsg->tr_node.io_Message.mn_ReplyPort);
	DeleteExtIO(timermsg);
	return mili ;
}
#   else			/* this is for SAS/C */
static long mytime()
{
   unsigned int cl[2];
   timer(cl);
   return(cl[0]*1000+cl[1]/1000);
}
#   endif /* AZTEC_C */
#  else /* AMIGA */
#   define mytime() ((time((timet*)0) - your_base) * CLKTCK)
#  endif /* AMIGA */
# else /* LACK_CLOCK */
#  define mytime clock
# endif /* LACK_CLOCK */
#else /* LACK_TIMES */
static long mytime()
{
  struct tms time_buffer;
  times(&time_buffer);
  return time_buffer.tms_utime + time_buffer.tms_stime;
}
#endif /* LACK_TIMES */

#ifdef LACK_FTIME
# ifdef AMIGA
SCM your_time()
{
  return MAKINUM(mytime());
}
# else
timet your_base = 0;
SCM your_time()
{
	return MAKINUM((time((timet*)0) - your_base) * (int)CLKTCK);
}
# endif /* AMIGA */
#else /* LACK_FTIME */
# ifdef USE_GETTIMEOFDAY
int scm_ftime(time_buffer)
     struct timeval *time_buffer;
{
  struct timezone t_z; 
  if (gettimeofday(time_buffer, &t_z) < 0) return -1;
  return 0;}

struct timeval your_base = {0, 0};
# define TIMETRIES 10
SCM your_time()
{
  long tmp;
  struct timeval time_buffer1;
  struct timeval time_buffer2;
  int cnt = 0;
 tryagain:
  cnt++;
  scm_ftime(&time_buffer1);
  scm_ftime(&time_buffer2);
  if (time_buffer1.tv_sec==time_buffer2.tv_sec) {
    if (time_buffer1.tv_usec > time_buffer2.tv_usec)
      time_buffer2.tv_sec = time_buffer2.tv_sec + 1;
  }
  else if ((1 + time_buffer1.tv_sec)==time_buffer2.tv_sec) ;
  else if (cnt < TIMETRIES) goto tryagain;
  else { /* could not read two ftime()s within one second in 10 tries */
    scm_warn("ftime()s too fast", "", MAKINUM(TIMETRIES));
    return MAKINUM(-1);
  }
  tmp = CLKTCK*(time_buffer2.tv_usec - your_base.tv_usec);
  tmp = CLKTCK*(time_buffer2.tv_sec - your_base.tv_sec) + tmp/1000000;
  return MAKINUM(tmp);
}
# else /* USE_GETTIMEOFDAY */
#  define scm_ftime ftime
struct timeb your_base = {0};
# define TIMETRIES 10
SCM your_time()
{
  long tmp;
  struct timeb time_buffer1;
  struct timeb time_buffer2;
  int cnt = 0;
 tryagain:
  cnt++;
  scm_ftime(&time_buffer1);
  scm_ftime(&time_buffer2);
  if (time_buffer1.time==time_buffer2.time) {
    if (time_buffer1.millitm > time_buffer2.millitm)
      time_buffer2.time = time_buffer2.time + 1;
  }
  else if ((1 + time_buffer1.time)==time_buffer2.time) ;
  else if (cnt < TIMETRIES) goto tryagain;
  else { /* could not read two ftime()s within one second in 10 tries */
    scm_warn("ftime()s too fast", "", MAKINUM(TIMETRIES));
    return MAKINUM(-1);
  }
  tmp = CLKTCK*(time_buffer2.millitm - your_base.millitm);
  tmp = CLKTCK*(time_buffer2.time - your_base.time) + tmp/1000;
  return MAKINUM(tmp);
}
# endif /* USE_GETTIMEOFDAY */
#endif /* LACK_FTIME */

long my_base = 0;
SCM my_time()
{
  return MAKINUM(mytime()-my_base);
}

SCM curtime()
{
  timet timv = time((timet*)0);
  SCM ans;
#ifndef _DCC
# ifdef STDC_HEADERS
#  if (__TURBOC__ > 0x201)
  timv = mktime(gmtime(&timv));
#  endif
# endif
#endif
  ans = ulong2num(timv);
  return BOOL_F==ans ? MAKINUM(timv) : ans;
}

long time_in_msec(x)
     long x;
{
  if (CLKTCK==60) return (x*50)/3;
  else
    return (CLKTCK < 1000 ? x*(1000L/(long)CLKTCK) : (x*1000L)/(long)CLKTCK);
}

static iproc subr0s[] = {
	{"get-internal-run-time", my_time},
	{"get-internal-real-time", your_time},
	{"current-time", curtime},
	{0, 0}};

void reset_time()
{
#ifdef LACK_FTIME
# ifndef AMIGA
	time(&your_base);
# endif
#else
	scm_ftime(&your_base);
#endif
	my_base = 0;
	my_base = mytime();
}
void init_time()
{
	sysintern("internal-time-units-per-second",
		  MAKINUM((long)CLKTCK));
	reset_time();
	init_iprocs(subr0s, tc7_subr_0);
}
