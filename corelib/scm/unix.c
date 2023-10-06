/* "unix.c" functions only in Unix (unix).
 * Copyright (C) 1994, 1995 Free Software Foundation, Inc.
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

#include <pwd.h>
#include <sys/types.h>
/* #include <sys/wait.h> */
#include <sys/stat.h>

extern SCM stat2scm P((struct stat *stat_temp));

SCM scm_mknod P((SCM path, SCM mode, SCM dev));
SCM scm_acct P((SCM path));
SCM scm_nice P((SCM incr));
SCM scm_sync P((void));
SCM scm_symlink P((SCM oldpath, SCM newpath));
SCM scm_readlink P((SCM path));
SCM scm_lstat P((SCM str));

#ifndef STDC_HEADERS
	void sync P((void));
	int symlink P((const char *oldpath, const char *newpath));
	int readlink P((const char *path, char *buf, sizet bufsiz));
	int acct P((const char *filename));
	int nice P((int inc));
#else /* added by Denys Duchier: for acct, etc... */
# ifdef SVR4
#  include <unistd.h>
# endif
# ifdef __NetBSD__
#  include <unistd.h>
# endif
# ifdef __OpenBSD__
#  include <unistd.h>
# endif
#endif /* STDC_HEADERS */

   /* Only the superuser can successfully execute mknod and acct */
/* int mknod P((const char *path, mode_t mode, dev_t dev));
   should be in stat.h */
static char s_mknod[] = "mknod";
SCM scm_mknod(path, mode, dev)
     SCM path, mode, dev;
{
  int val;
  ASRTER(NIMP(path) && STRINGP(path), path, ARG1, s_mknod);
  ASRTER(INUMP(mode), mode, ARG2, s_mknod);
  ASRTER(INUMP(dev), dev, ARG3, s_mknod);
  SYSCALL(val = mknod(CHARS(path), INUM(mode), INUM(dev)););
  return val ? BOOL_F : BOOL_T;
}
static char s_acct[] = "acct";
SCM scm_acct(path)
     SCM path;
{
  int val;
  if (FALSEP(path)) {
    SYSCALL(val = acct(0););
    return val ? BOOL_F : BOOL_T;
  }
  ASRTER(NIMP(path) && STRINGP(path), path, ARG1, s_acct);
  SYSCALL(val = acct(CHARS(path)););
  return val ? BOOL_F : BOOL_T;
}

static char s_nice[] = "nice";
SCM scm_nice(incr)
     SCM incr;
{
  ASRTER(INUMP(incr), incr, ARG1, s_nice);
  return nice(INUM(incr)) ? BOOL_F : BOOL_T;
}

SCM scm_sync()
{
  sync();
  return UNSPECIFIED;
}

static char s_symlink[] = "symlink";
SCM scm_symlink(oldpath, newpath)
     SCM oldpath, newpath;
{
  int val;
  ASRTER(NIMP(oldpath) && STRINGP(oldpath), oldpath, ARG1, s_symlink);
  ASRTER(NIMP(newpath) && STRINGP(newpath), newpath, ARG2, s_symlink);
  SYSCALL(val = symlink(CHARS(oldpath), CHARS(newpath)););
  return val ? BOOL_F : BOOL_T;
}
static char s_readlink[] = "readlink";
SCM scm_readlink(path)
  SCM path;
{
  int i;
  char buf[1024];
  ASRTER(NIMP(path) && STRINGP(path), path, ARG1, s_readlink);
  SYSCALL(i = readlink(CHARS(path), buf, (sizet)sizeof(buf)););
  if (-1==i) return BOOL_F;
  return makfromstr(buf, (sizet)i);
}
static char s_lstat[] = "lstat";
SCM scm_lstat(str)
  SCM str;
{
  int i;
  struct stat stat_temp;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_lstat);
  SYSCALL(i = lstat(CHARS(str), &stat_temp););
  if (i) return BOOL_F;
  return stat2scm(&stat_temp);
}

static iproc subr1s[] = {
	{s_nice, scm_nice},
	{s_acct, scm_acct},
	{s_lstat, scm_lstat},
	{s_readlink, scm_readlink},
	{0, 0}};

void init_unix()
{
	make_subr("sync", tc7_subr_0, scm_sync);
	init_iprocs(subr1s, tc7_subr_1);
	make_subr(s_symlink, tc7_subr_2, scm_symlink);
	make_subr(s_mknod, tc7_subr_3, scm_mknod);
	add_feature("unix");
}
