/* "ioext.c" code for system calls in common between PC compilers and unix.
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1997 Free Software Foundation, Inc.
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

#ifdef __EMX__
# include <sys/types.h>
#endif

#ifndef macintosh
# ifdef vms
#  include <stat.h>
# else
#  include <sys/stat.h>
# endif

# ifdef __TURBOC__
#  include <io.h>
# endif
SCM	stat2scm P((struct stat *stat_temp));
/* int	mkdir P((const char *path, mode_t mode)); */
#endif
#ifdef hpux
# include <unistd.h>
#endif
#ifdef __sgi__
# include <unistd.h>
#endif
#ifdef __FreeBSD__
# include <unistd.h>
#endif
#ifdef __NetBSD__
# include <unistd.h>
#endif
#ifdef __OpenBSD__
# include <unistd.h>
#endif
/* added by Denys Duchier */
#ifdef __SVR4
# include <sys/types.h>
# include <unistd.h>
#endif
#ifdef linux
# include <unistd.h>
#endif
#ifdef GO32
# include <unistd.h>
#endif
#ifdef __osf__
# include <unistd.h>
#endif
#ifdef __MACH__
# include <unistd.h>
#endif
#ifdef __CYGWIN__
# include <unistd.h>
#endif

#ifndef STDC_HEADERS
	int chdir P((const char *path));
	int unlink P((const char *name));
	int link P((const char *from, const char *to));
	char *getcwd P((char *buf, sizet size));
	int access P((const char *name, int type));
	int dup P((int fd));
	int dup2 P((int fd, int fd2));
	int close P((int fd));
	int rmdir P((const char *path));
	int execv P((const char *, char *const *));
	int execvp P((const char *, char *const *));
	int putenv P((const char *));
#else
# ifdef _WIN32
#  include <direct.h>
#  include <io.h>
#  include <process.h>
# endif
# ifdef __HIGHC__
#  include <direct.h>
#  include <dirent.h>
#  include <process.h>
#  define mkdir(foo,bar) mkdir(foo)
# endif
#endif /* STDC_HEADERS */

#ifdef __EMX__
	int execv P((const char *, char *const *));
	int execvp P((const char *, char *const *));
	int putenv P((const char *));
#endif

static char s_read_line[] = "read-line";
SCM read_line(port)
     SCM port;
{
  register int c;
  register int j = 0;
  sizet len = 30;
  SCM tok_buf = makstr((long) len);
  register char *p = CHARS(tok_buf);
  if (UNBNDP(port)) port = cur_inp;
  else ASRTER(NIMP(port) && OPINPORTP(port), port, ARG1, s_read_line);
  if (EOF==(c = lgetc(port))) return EOF_VAL;
  while(1) {
    switch (c) {
    case LINE_INCREMENTORS:
    case EOF:
      if (j>0 && '\r'==p[j-1]) j--;
      if (len==j) return tok_buf;
      return resizuve(tok_buf, (SCM)MAKINUM(j));
    default:
      if (j >= len) {
	p = grow_tok_buf(tok_buf);
	len = LENGTH(tok_buf);
      }
      p[j++] = c;
      c = lgetc(port);
    }
  }
}
static char s_read_line1[] = "read-line!";
SCM read_line1(str, port)
     SCM str, port;
{
  register int c;
  register int j = 0;
  register char *p;
  sizet len;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_read_line1);
  p = CHARS(str);
  len = LENGTH(str);
  if (UNBNDP(port)) port = cur_inp;
  else ASRTER(NIMP(port) && OPINPORTP(port), port, ARG2, s_read_line1);
  c = lgetc(port);
  if (EOF==c) return EOF_VAL;
  while(1) {
    switch (c) {
    case LINE_INCREMENTORS:
    case EOF:
      return MAKINUM(j);
    default:
      if (j >= len) {
	lungetc(c, port);
	return BOOL_F;
      }
      p[j++] = c;
      c = lgetc(port);
    }
  }
}
static char s_write_line[] = "write-line";
SCM l_write_line(obj, port)
     SCM obj, port;
{
  scm_display(obj, port);
  return scm_newline(port);
}

static char s_reopen_file[] = "reopen-file";
SCM reopen_file(filename, modes, port)
     SCM filename, modes, port;
{
  FILE *f;
  char cmodes[4];
  long flags;
  ASRTER(NIMP(filename) && STRINGP(filename), filename, ARG1, s_reopen_file);
  ASRTER(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_reopen_file);
  flags =  mode_bits(CHARS(modes), cmodes);
  ASRTER(flags, modes, ARG2, s_reopen_file);
  DEFER_INTS;
  ASRTER(NIMP(port) && FPORTP(port) && OPENP(port), port, ARG3, s_reopen_file);
  SCM_OPENCALL(f = freopen(CHARS(filename), cmodes, STREAM(port)));
  if (!f) {
    ALLOW_INTS;
    return BOOL_F;
  }
  else {
    SCM_PORTFLAGS(port) = flags;
    SCM_SETFLAGS(port, flags);
    if (BUF0 & flags)
      i_setbuf0(port);
    SCM_PORTDATA(port) = filename;
  }
  ALLOW_INTS;
  return port;
}

#ifndef MCH_AMIGA
# ifndef macintosh
static char s_dup[]="duplicate-port";
SCM l_dup(oldpt, modes)
     SCM oldpt, modes;
{
  long flags;
  char cmodes[4];
  int tfd;
  FILE *f;
  SCM newpt;
  ASRTER(NIMP(oldpt) && OPFPORTP(oldpt), oldpt, ARG1, s_dup);
  ASRTER(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_dup);
  flags = mode_bits(CHARS(modes), cmodes);
  ASRTER(flags, modes, ARG2, s_dup);
  NEWCELL(newpt);
  DEFER_INTS;
  SCM_OPENCALL(tfd = dup(fileno(STREAM(oldpt))));
  if (-1==tfd) {ALLOW_INTS;return BOOL_F;};
  SYSCALL(f = fdopen(tfd, cmodes););
  if (!f) {
    int lerrno = errno;
    close(tfd);
    errno = lerrno;
#  ifdef EINVAL
    if (lerrno==EINVAL) wta(modes, (char *)ARG2, s_dup);
#  endif
    wta(MAKINUM(tfd), (char *)NALLOC, s_port_type);
  }
  newpt = scm_port_entry(f, tc16_fport, flags);
  SCM_PORTDATA(newpt) = SCM_PORTDATA(oldpt);
  if (BUF0 & flags)
    i_setbuf0(newpt);
  ALLOW_INTS;
  return newpt;
}
static char s_dup2[]="redirect-port!";
SCM l_dup2(into_pt, from_pt)
     SCM into_pt, from_pt;
{
  int ans, oldfd, newfd;
  DEFER_INTS;
  ASRTER(NIMP(into_pt) && OPFPORTP(into_pt), into_pt, ARG1, s_dup2);
  ASRTER(NIMP(from_pt) && OPFPORTP(from_pt), from_pt, ARG1, s_dup2);
  oldfd = fileno(STREAM(into_pt));
  newfd = fileno(STREAM(from_pt));
  SCM_OPENCALL(ans = dup2(oldfd, newfd));
  if (-1==ans) {ALLOW_INTS;return BOOL_F;};
  ALLOW_INTS;
  return into_pt;
}
# endif

# ifndef vms
static char s_opendir[]="opendir";
static char s_readdir[]="readdir";
static char s_rewinddir[]="rewinddir";
static char s_closedir[]="closedir";
#  ifndef _WIN32
#   include <dirent.h>
SCM l_opendir(dirname)
     SCM dirname;
{
  DIR *ds;
  SCM dir;
  ASRTER(NIMP(dirname) && STRINGP(dirname), dirname, ARG1, s_opendir);
  NEWCELL(dir);
  DEFER_INTS;
  SCM_OPENCALL(ds = opendir(CHARS(dirname)));
  if (!ds) {ALLOW_INTS; return BOOL_F;}
  CAR(dir) = tc16_dir | OPN;
  SETCDR(dir, ds);
  ALLOW_INTS;
  return dir;
}

SCM l_readdir(port)
     SCM port;
{
  struct dirent *rdent;
  DEFER_INTS;
  ASRTER(OPDIRP(port), port, ARG1, s_readdir);
  SYSCALL(rdent = readdir((DIR *)CDR(port)););
  if (!rdent) {ALLOW_INTS; return BOOL_F;}
  ALLOW_INTS;
  /* rdent could be overwritten by another readdir to the same handle */
  return makfrom0str((char *)rdent->d_name);
}

SCM l_rewinddir(port)
     SCM port;
{
  ASRTER(OPDIRP(port), port, ARG1, s_rewinddir);
  rewinddir((DIR *)CDR(port));
  return UNSPECIFIED;
}

SCM l_closedir(port)
     SCM port;
{
  int sts;
  ASRTER(DIRP(port), port, ARG1, s_closedir);
  DEFER_INTS;
  if (CLOSEDP(port)) {ALLOW_INTS;return BOOL_F;}
  SYSCALL(sts = closedir((DIR *)CDR(port)););
  if (sts) {ALLOW_INTS; return BOOL_F;}
  CAR(port) = tc16_dir;
  ALLOW_INTS;
  return BOOL_T;
}

int dir_print(sexp, port, writing)
     SCM sexp; SCM port; int writing;
{
  prinport(sexp, port, "directory");
  return !0;
}
sizet dir_free(p)
     CELLPTR p;
{
  if (OPENP((SCM)p)) closedir((DIR *)CDR((SCM)p));
  return 0;
}
#   define dir_mark mark0
#  else /* _WIN32 */
struct WDIR {
  long handle;                  //-1 if at end of list.
  struct _finddata_t info;
  SCM fspec;                  //for rewind, needs gc protection.
};

SCM l_opendir(dirname)
     SCM dirname;
{
  long handle;
  SCM fspec, dir;
  struct _finddata_t info;
  struct WDIR *wdir;
  int dlen;
  ASRTER(NIMP(dirname) && STRINGP(dirname), dirname, ARG1, s_opendir);
  dlen = LENGTH(dirname);
  fspec = makstr(dlen + 2);
  strcpy(CHARS(fspec), CHARS(dirname));
  if ('/' != CHARS(fspec)[dlen - 1] && '\\' != CHARS(fspec)[dlen - 1])
    CHARS(fspec)[dlen++] = '/';
  CHARS(fspec)[dlen++] = '*';
  CHARS(fspec)[dlen] = 0;
  DEFER_INTS;
  dir = must_malloc_cell(sizeof(struct WDIR)+0L, tc16_dir, s_opendir);
  wdir = (struct WDIR*)CHARS(dir);
  wdir->fspec = fspec;
  SCM_OPENCALL(handle = _findfirst(CHARS(fspec), &(wdir->info)));
  if (-1 == handle) {ALLOW_INTS; return BOOL_F;}
  wdir->handle = handle;
  CAR(dir) |= OPN;
  ALLOW_INTS;
  return dir;
}

SCM l_readdir(port)
     SCM port;
{
  SCM fname;
  struct WDIR *wdir;
  int ret;
  ASRTER(OPDIRP(port), port, ARG1, s_readdir);
  wdir = (struct WDIR*)CHARS(port);
  if (-1 == wdir->handle) return BOOL_F;
  fname = makfrom0str(wdir->info.name);
  DEFER_INTS;
  SYSCALL(ret = _findnext(wdir->handle, &(wdir->info)););
  if (0 != ret) {
    SYSCALL(_findclose(wdir->handle););
    wdir->handle = -1;
  }
  ALLOW_INTS;
  return fname;
}

SCM l_rewinddir(port)
     SCM port;
{
  struct WDIR *wdir;
  ASRTER(OPDIRP(port), port, ARG1, s_rewinddir);
  wdir = (struct WDIR*)CHARS(port);
  DEFER_INTS;
  if (-1 != wdir->handle)
    SYSCALL(_findclose(wdir->handle););
  SYSCALL(wdir->handle = _findfirst(CHARS(wdir->fspec), &(wdir->info)););
  ALLOW_INTS;
  return UNSPECIFIED;
}

SCM l_closedir(port)
     SCM port;
{
  struct WDIR *wdir;
  ASRTER(DIRP(port), port, ARG1, s_closedir);
  wdir = (struct WDIR*)CHARS(port);
  DEFER_INTS;
  if (CLOSEDP(port)) {ALLOW_INTS;return BOOL_F;}
  if (-1 != wdir->handle) {
    SYSCALL(_findclose(wdir->handle););
    wdir->handle = -1;
  }
  CAR(port) = tc16_dir;
  wdir->fspec = UNSPECIFIED;
  ALLOW_INTS;
  return BOOL_T;
}

int dir_print(sexp, port, writing)
     SCM sexp; SCM port; int writing;
{
  prinport(sexp, port, "directory");
  return !0;
}
sizet dir_free(p)
     CELLPTR p;
{
  struct WDIR *wdir = (struct WDIR*)CHARS((SCM)p);
  if (-1 != wdir->handle)
    _findclose(wdir->handle);
  must_free(CHARS((SCM)p), (sizet)sizeof(struct WDIR));
  return 0;
}
SCM dir_mark(ptr)
     SCM ptr;
{
  return ((struct WDIR*)CHARS(ptr))->fspec;
}
#  endif /* _WIN32 */
long tc16_dir;
static smobfuns dir_smob = {dir_mark, dir_free, dir_print, 0};
# endif /* vms */

static char s_mkdir[] = "mkdir";
SCM l_mkdir(path, mode)
     SCM path, mode;
{
  int val;
  ASRTER(NIMP(path) && STRINGP(path), path, ARG1, s_mkdir);
  ASRTER(INUMP(mode), mode, ARG2, s_mkdir);
# ifdef _WIN32
  SYSCALL(val = mkdir(CHARS(path)););
# else
  SYSCALL(val = mkdir(CHARS(path), INUM(mode)););
				/* (mode_t)INUM(mode) might be needed */
# endif
  return val ? BOOL_F : BOOL_T;
}
# ifdef vms
static char s_dot_dir[] = ".DIR";
# endif
static char s_rmdir[] = "rmdir";
SCM l_rmdir(path)
     SCM path;
{
  int val;
  ASRTER(NIMP(path) && STRINGP(path), path, ARG1, s_rmdir);
# ifdef vms
  return del_fil(st_append(cons2(path, s_dot_dir, EOL)));
# else
  SYSCALL(val = rmdir(CHARS(path)););
  return val ? BOOL_F : BOOL_T;
# endif
}
#endif /* MCH_AMIGA */

#ifndef THINK_C
static char s_chdir[] = "chdir";
SCM lchdir(str)
     SCM str;
{
  int ans;
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_chdir);
  SYSCALL(ans = chdir(CHARS(str)););
  return ans ? BOOL_F : BOOL_T;
}
# ifndef MCH_AMIGA
#  ifdef __TURBOC__
#   include <dir.h>
#  endif
SCM l_getcwd()
{
  char *ans;
#  ifndef vms
  char wd[256];
  SYSCALL(ans = getcwd(wd, 256););
  return ans ? makfrom0str(wd) : BOOL_F;
#  else
  SYSCALL(ans = getenv("PATH"););
  return ans ? makfrom0str(ans) : BOOL_F;
#  endif
}

#  ifndef __MWERKS__
static char s_chmod[] = "chmod";
SCM l_chmod(pathname, mode)
     SCM pathname, mode;
{
  int val;
  ASRTER(NIMP(pathname) && STRINGP(pathname), pathname, ARG1, s_chmod);
  ASRTER(INUMP(mode), mode, ARG2, s_chmod);
  SYSCALL(val = chmod(CHARS(pathname), INUM(mode)););
  return val ? BOOL_F : BOOL_T;
}
#  endif

#  ifndef vms
#   ifdef __EMX__
#    include <sys/utime.h>
#   else
#    ifdef _WIN32
#     include <sys/utime.h>
#    else
#     include <utime.h>
#    endif
#   endif
static char s_utime[] = "utime";
SCM l_utime(pathname, acctime, modtime)
     SCM pathname, acctime, modtime;
{
  int val;
  struct utimbuf utm_tmp;
  utm_tmp.actime = num2ulong(acctime, (char *)ARG2, s_utime);
  utm_tmp.modtime = num2ulong(modtime, (char *)ARG3, s_utime);
  ASRTER(NIMP(pathname) && STRINGP(pathname), pathname, ARG1, s_utime);
  SYSCALL(val = utime(CHARS(pathname), &utm_tmp););
  return val ? BOOL_F : BOOL_T;
}
#  endif /* vms */

#  ifndef __MWERKS__
static char s_umask[] = "umask";
SCM l_umask(mode)
     SCM mode;
{
  ASRTER(INUMP(mode), mode, ARG1, s_umask);
  return MAKINUM(umask(INUM(mode)));
}
#  endif
# endif /* MCH_AMIGA */
#endif /* THINK_C */

static char s_ren_fil[] = "rename-file";
SCM ren_fil(oldname, newname)
     SCM oldname, newname;
{
  SCM ans;
  ASRTER(NIMP(oldname) && STRINGP(oldname), oldname, ARG1, s_ren_fil);
  ASRTER(NIMP(newname) && STRINGP(newname), newname, ARG2, s_ren_fil);
#if 1 /* def STDC_HEADERS */
  SYSCALL(ans = (rename(CHARS(oldname), CHARS(newname))) ? BOOL_F: BOOL_T;);
  return ans;
#else
  DEFER_INTS;
  SYSCALL(ans = link(CHARS(oldname), CHARS(newname)) ? BOOL_F : BOOL_T;);
  if (!FALSEP(ans)) {
    SYSCALL(ans = unlink(CHARS(oldname)) ? BOOL_F : BOOL_T;);
    if (FALSEP(ans))
      SYSCALL(unlink(CHARS(newname));); /* unlink failed.  remove new name */
  }
  ALLOW_INTS;
  return ans;
#endif
}
static char s_copy_file[] = "copy-file";
SCM scm_copy_file(oldname, newname)
     SCM oldname, newname;
{
  ASRTER(NIMP(oldname) && STRINGP(oldname), oldname, ARG1, s_copy_file);
  ASRTER(NIMP(newname) && STRINGP(newname), newname, ARG2, s_copy_file);
  {
    FILE* fin = fopen(CHARS(oldname), "rb");
    FILE* fout;
    unsigned char buff[1024];
    int cnt, retval = BOOL_T;
    if (!fin) return BOOL_F;
    fout = fopen(CHARS(newname), "wb");
    if (!fout) {fclose(fin); return BOOL_F;}
    {
#ifndef THINK_C
# ifndef MCH_AMIGA
#  ifndef vms
      int i;
      struct stat stat_temp;
      struct utimbuf utm_tmp;
      SYSCALL(i = fstat(fileno(fin), &stat_temp););
#  endif
# endif
#endif
      while ((cnt = fread(buff, 1, 1024, fin))) {
	if ((cnt > 0) && (cnt != fwrite(buff, 1, cnt, fout))) retval = BOOL_F;
      }
      if (!feof(fin)) retval = BOOL_F;
      fclose(fin);
      fclose(fout);
#ifndef THINK_C
# ifndef MCH_AMIGA
#  ifndef vms
      if (!i) {
	utm_tmp.actime = stat_temp.st_atime;
	utm_tmp.modtime = stat_temp.st_mtime;
	SYSCALL(i = utime(CHARS(newname), &utm_tmp););
      }
      if (i) return BOOL_F;
#  endif
# endif
#endif
      return retval;
    }
  }
}

static char s_fileno[] = "fileno";
SCM l_fileno(port)
     SCM port;
{
  ASRTER(NIMP(port) && OPPORTP(port), port, ARG1, s_fileno);
  if (tc16_fport != TYP16(port)) return BOOL_F;
  return MAKINUM(fileno(STREAM(port)));
}
#ifndef THINK_C
# ifndef __MWERKS__
#  ifndef F_OK
#   define F_OK 00
#   define X_OK 01
#   define W_OK 02
#   define R_OK 04
#  endif
static char s_access[] = "access";
SCM l_access(pathname, mode)
     SCM pathname, mode;
{
  int val;
  int imodes;
  ASRTER(NIMP(pathname) && STRINGP(pathname), pathname, ARG1, s_access);
  if (INUMP(mode)) imodes = INUM(mode);
  else {
    ASRTER(NIMP(mode) && STRINGP(mode), mode, ARG2, s_access);
    imodes = F_OK | (strchr(CHARS(mode), 'r') ? R_OK : 0)
      | (strchr(CHARS(mode), 'w') ? W_OK : 0)
	| (strchr(CHARS(mode), 'x') ? X_OK : 0);
  }
  SYSCALL(val = access(CHARS(pathname), imodes););
  return val ? BOOL_F : BOOL_T;
}
# endif /* __MWERKS__ */

SCM stat2scm P((struct stat *stat_temp));

char s_stat[] = "stat";
SCM l_stat(str)
  SCM str;
{
  int i;
  struct stat stat_temp;
  if (IMP(str))
  badarg1: wta(str, (char *)ARG1, s_stat);
  if (STRINGP(str)) {SYSCALL(i = stat(CHARS(str), &stat_temp););}
  else {
# ifndef MCH_AMIGA
    if (!OPFPORTP(str)) goto badarg1;
    SYSCALL(i = fstat(fileno(STREAM(str)), &stat_temp););
# else
    goto badarg1;
# endif
  }
  if (i) return BOOL_F;
  return stat2scm(&stat_temp);
}
# ifdef MCH_AMIGA
SCM stat2scm(stat_temp)
     struct stat *stat_temp;
{
  SCM ans = make_vector(MAKINUM(3), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  ve[ 0] = ulong2num((unsigned long)stat_temp->st_attr);
  ve[ 1] = ulong2num((unsigned long)stat_temp->st_mtime);
  ve[ 2] = ulong2num((unsigned long)stat_temp->st_size);
  return ans;
}
# else
SCM stat2scm(stat_temp)
     struct stat *stat_temp;
{
  SCM ans = make_vector(MAKINUM(11), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  ve[ 0] = ulong2num((unsigned long)stat_temp->st_dev);
  ve[ 1] = ulong2num((unsigned long)stat_temp->st_ino);
  ve[ 2] = ulong2num((unsigned long)stat_temp->st_mode);
  ve[ 3] = ulong2num((unsigned long)stat_temp->st_nlink);
  ve[ 4] = ulong2num((unsigned long)stat_temp->st_uid);
  ve[ 5] = ulong2num((unsigned long)stat_temp->st_gid);
  ve[ 6] = ulong2num((unsigned long)stat_temp->st_rdev);
  ve[ 7] = ulong2num((unsigned long)stat_temp->st_size);
  ve[ 8] = ulong2num((unsigned long)stat_temp->st_atime);
  ve[ 9] = ulong2num((unsigned long)stat_temp->st_mtime);
  ve[10] = ulong2num((unsigned long)stat_temp->st_ctime);
  return ans;
}
#  ifdef __TURBOC__
#   include <process.h>
#  endif
SCM l_getpid()
{
  return MAKINUM((unsigned long)getpid());
}
# endif /* MCH_AMIGA */
#endif /* THINK_C */

#ifndef __IBMC__
# ifndef macintosh
#  ifndef __WATCOMC__
#   ifndef _Windows
#    ifdef __TURBOC__
#     include <process.h>
#    endif
char s_execv[] = "execv";
char s_execvp[] = "execvp";
SCM i_execv(modes, path, args)
     char * modes;
     SCM path, args;
{
  char **execargv;
  int i = ilength(args);
  ASRTER(i>0, args, WNA, s_execv);
  ASRTER(NIMP(path) && STRINGP(path), path, ARG1, s_execv);
  /*  dowinds(EOL); */
  args = cons(path, args);
  DEFER_INTS;
  execargv = makargvfrmstrs(args, s_execv);
  ignore_signals();
  (strchr(modes, 'p') ? execvp : execv)(execargv[0], &execargv[1]);
  unignore_signals();
  ALLOW_INTS;
  perror(execargv[0]);
  return MAKINUM(errno);
}
SCM lexec(path, arg0, args)
     SCM path, arg0, args;
{
  return i_execv("", path, cons(arg0, args));
}
SCM lexecp(path, arg0, args)
     SCM path, arg0, args;
{
  return i_execv("p", path, cons(arg0, args));
}
SCM lexecv(path, args)
     SCM path, args;
{
  return i_execv("", path, args);
}
SCM lexecvp(path, args)
     SCM path, args;
{
  return i_execv("p", path, args);
}
static char s_putenv[] = "putenv";
SCM l_putenv(str)
     SCM str;
{
  ASRTER(NIMP(str) && STRINGP(str), str, ARG1, s_putenv);
  return putenv(CHARS(str)) ? BOOL_F : BOOL_T;
}
#   endif
#  endif
# endif
#endif

static iproc subr1s[] = {
	{s_fileno, l_fileno},
#ifndef MCH_AMIGA
# ifndef vms
	{s_opendir, l_opendir},
	{s_readdir, l_readdir},
	{s_rewinddir, l_rewinddir},
	{s_closedir, l_closedir},
# endif
	{s_rmdir, l_rmdir},
#endif
#ifndef THINK_C
# ifndef MCH_AMIGA
#  ifndef __MWERKS__
	{s_umask, l_umask},
#  endif
# endif
	{s_stat, l_stat},
	{s_chdir, lchdir},
#endif
	{0, 0}};

static iproc subr1os[] = {
	{s_read_line, read_line},
	{0, 0}};

static iproc subr2s[] = {
	{s_ren_fil, ren_fil},
	{s_copy_file, scm_copy_file},
#ifndef macintosh
	{s_access, l_access},
#endif
#ifndef MCH_AMIGA
	{s_mkdir, l_mkdir},
# ifndef macintosh
	{s_dup, l_dup},
	{s_dup2, l_dup2},
	{s_chmod, l_chmod},
# endif
#endif
	{0, 0}};

#include <fcntl.h>		/* for O_RDONLY, O_RDWR, O_EXCL */
#ifdef O_EXCL
SCM scm_try_create_file(fname, modes, perms)
     SCM fname, modes, perms;
{
  SCM port;
  FILE *f;
  char cmodes[4];
  long flags;
  int fd, fdflags = O_CREAT | O_EXCL;
# ifdef S_IROTH
  mode_t cperms = S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH;
# else
  int cperms = 0666;
# endif
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_try_create_file);
  ASRTER(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_try_create_file);
  if (NNULLP(perms)) {
    perms = CAR(perms);
    ASRTER(INUMP(perms), perms, ARG3, s_try_create_file);
# ifdef S_IROTH
    cperms = (mode_t)INUM(perms);
# else
    cperms = INUM(perms);
# endif
  }
  flags = mode_bits(CHARS(modes), cmodes);
  ASRTER(flags, modes, ARG2, s_try_create_file);
  fdflags |= (RDNG & flags) ? O_RDWR : O_WRONLY;
  DEFER_INTS;
  SCM_OPENCALL(fd = open(CHARS(fname), fdflags, cperms));
  if (fd >= 0 && (f = fdopen(fd, cmodes))) {
    port = scm_port_entry(f, tc16_fport, flags);
    if (BUF0 & flags) i_setbuf0(port);
    SCM_PORTDATA(port) = fname;
  }
  else
    port = BOOL_F;
  ALLOW_INTS;
  return port;
}
#endif

static iproc subr2os[] = {
	{s_read_line1, read_line1},
	{s_write_line, l_write_line},
	{0, 0}};

SCM_DLL_EXPORT void init_ioext P((void));

void init_ioext()
{
	init_iprocs(subr1os, tc7_subr_1o);
	init_iprocs(subr1s, tc7_subr_1);
	init_iprocs(subr2os, tc7_subr_2o);
	init_iprocs(subr2s, tc7_subr_2);
#ifdef O_EXCL
	make_subr(s_try_create_file, tc7_lsubr_2, scm_try_create_file);
#endif
	make_subr(s_reopen_file, tc7_subr_3, reopen_file);
#ifndef THINK_C
# ifndef MCH_AMIGA
#  ifndef __MWERKS__
	make_subr("getpid", tc7_subr_0, l_getpid);
#  endif
	make_subr("getcwd", tc7_subr_0, l_getcwd);
#  ifndef vms
#   ifndef _WIN32
	make_subr(s_utime, tc7_subr_3, l_utime);
#   endif
	tc16_dir = newsmob(&dir_smob);
#  endif
# endif
#endif
#ifndef __IBMC__
# ifndef macintosh
#  ifndef __WATCOMC__
#   ifndef _Windows
	make_subr(s_execv, tc7_subr_2, lexecv);
	make_subr(s_execvp, tc7_subr_2, lexecvp);
	make_subr("execl", tc7_lsubr_2, lexec);
	make_subr("execlp", tc7_lsubr_2, lexecp);
	make_subr(s_putenv, tc7_subr_1, l_putenv);
#   endif
#  endif
# endif
#endif
	add_feature("i/o-extensions");
	add_feature("line-i/o");
	scm_ldstr("\n\
(define (file-exists? path) (access path \"r\"))\n\
(define (make-directory path)\n\
  (define umsk (umask #o022))\n\
  (let ((success? (mkdir path (logxor #o777 umsk))))\n\
    (umask umsk)\n\
    success?))\n\
(define current-directory getcwd)\n\
(define (directory-for-each proc dirname . args)\n\
  (define dir (opendir (if (symbol? dirname)\n\
			   (symbol->string dirname)\n\
			   dirname)))\n\
  (if dir\n\
      (let ((selector\n\
	     (cond ((null? args) identity)\n\
		   ((> (length args) 1)\n\
		    (slib:error 'directory-for-each\n\
				'too-many-arguments\n\
				(cdr args)))\n\
		   ((procedure? (car args)) (car args))\n\
		   ((string? (car args))\n\
		    (require 'filename)\n\
		    (filename:match?? (car args)))\n\
		   (else (slib:error 'directory-for-each\n\
				     'unknown-selector-type\n\
				     (car args))))))\n\
	(do ((filename (readdir dir) (readdir dir)))\n\
	    ((not filename) (closedir dir))\n\
	  (and (selector filename) (proc filename))))))\n\
(define (directory*-for-each proc path-glob)\n\
  (define dir (pathname->vicinity path-glob))\n\
  (let ((glob (substring path-glob\n\
			 (string-length dir)\n\
			 (string-length path-glob))))\n\
    (directory-for-each proc\n\
			(if (equal? \"\" dir) \".\" dir)\n\
			glob)))\n\
(define (system->line command . tmp)\n\
  (require 'filename)\n\
  (cond ((null? tmp)\n\
         (call-with-tmpnam\n\
          (lambda (tmp) (system->line command tmp))))\n\
        (else\n\
         (set! tmp (car tmp))\n\
         (and (zero? (system (string-append command \" > \" tmp)))\n\
              (file-exists? tmp)\n\
              (let ((line (call-with-input-file tmp read-line)))\n\
                (if (eof-object? line) \"\" line))))))\n\
");
	add_feature("directory");
}
