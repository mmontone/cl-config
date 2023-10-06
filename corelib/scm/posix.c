/* "posix.c" functions only in Posix (unix).
 * Copyright (C) 1994, 1995, 1998, 2006 Free Software Foundation, Inc.
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
#include <sys/wait.h>
/* added by Denys Duchier: for kill */
#include <signal.h>

#ifndef STDC_HEADERS
	char *ttyname P((int fd));
	FILE *popen P((const char* command, const char* type));
	int pclose P((FILE* stream));
#else /* added by Denys Duchier */
# ifdef SVR4
#  include <unistd.h>
# endif
# ifdef linux
#  include <unistd.h>
# endif
# ifdef __OpenBSD__
#  include <unistd.h>
# endif
# ifdef __NetBSD__
#  include <unistd.h>
# endif
#endif

static char s_chown[] = "chown";
SCM l_chown(path, owner, group)
     SCM path, owner, group;
{
  int val;
  ASRTER(NIMP(path) && STRINGP(path), path, ARG1, s_chown);
  ASRTER(INUMP(owner), owner, ARG2, s_chown);
  ASRTER(INUMP(group), group, ARG3, s_chown);
  SYSCALL(val = chown(CHARS(path), INUM(owner), INUM(group)););
  return val ? BOOL_F : BOOL_T;
}

static char s_link[] = "link";
SCM l_link(oldpath, newpath)
     SCM oldpath, newpath;
{
  int val;
  ASRTER(NIMP(oldpath) && STRINGP(oldpath), oldpath, ARG1, s_link);
  ASRTER(NIMP(newpath) && STRINGP(newpath), newpath, ARG2, s_link);
  SYSCALL(val = link(CHARS(oldpath), CHARS(newpath)););
  return val ? BOOL_F : BOOL_T;
}

SCM l_pipe()
{
  int fd[2], ret;
  FILE *f_rd, *f_wt;
  SCM p_rd, p_wt;
  NEWCELL(p_rd); NEWCELL(p_wt);
  DEFER_INTS;
  SYSCALL(ret = pipe(fd););
  if (ret) {ALLOW_INTS; return BOOL_F;}
  SYSCALL(f_rd = fdopen(fd[0], "r"););
  if (!f_rd) {
    close(fd[0]);
    goto errout;
  }
  SCM_OPENCALL(f_wt = fdopen(fd[1], "w"));
  if (!f_wt) {
    fclose(f_rd);
  errout:
    close(fd[1]);
    wta(UNDEFINED, (char *)NALLOC, s_port_type);
  }
  p_rd = scm_port_entry(f_rd, tc16_fport, mode_bits("r", (char *)0));
  p_wt = scm_port_entry(f_wt, tc16_fport, mode_bits("w", (char *)0));
  ALLOW_INTS;
  return cons(p_rd, p_wt);
}

char	s_op_pipe[] = "open-pipe";
SCM open_pipe(pipestr, modes)
     SCM pipestr, modes;
{
	FILE *f;
	register SCM z;
	ASRTER(NIMP(pipestr) && STRINGP(pipestr), pipestr, ARG1, s_op_pipe);
	ASRTER(NIMP(modes) && (STRINGP(modes) || SYMBOLP(modes)), modes, ARG2, s_op_pipe);
	NEWCELL(z);
	/* DEFER_INTS, SYSCALL, and ALLOW_INTS are probably paranoid here*/
	DEFER_INTS;
	ignore_signals();
	SCM_OPENCALL(f = popen(CHARS(pipestr), CHARS(modes)));
	unignore_signals();
	z = f ?
	  scm_port_entry(f, tc16_pipe,
			 OPN | (strchr(CHARS(modes), 'r') ? RDNG : WRTNG)) :
	  BOOL_F;
	ALLOW_INTS;
	return z;
}

static char scm_s_getgroups[] = "getgroups";
SCM scm_getgroups()
{
  SCM grps, ans;
  int ngroups = getgroups(0, 0);
  if (!ngroups) return BOOL_F;
  scm_protect_temp(&grps);
  DEFER_INTS;
  /* grps is used as a gc protect, its type used to be tc7_string, but
     strings are now checked for null termination during gc.
     The length needs not be exactly right */
  grps = must_malloc_cell((0L + ngroups) * sizeof(gid_t),
			  MAKE_LENGTH(((0L + ngroups) * sizeof(gid_t))/sizeof(long),
				      tc7_VfixN32),
			  scm_s_getgroups);
  ALLOW_INTS;
  {
    gid_t *groups = (gid_t *)CHARS(grps);
    int val = getgroups(ngroups, groups);
    if (val < 0) return BOOL_F;
    ans = make_vector(MAKINUM(ngroups), UNDEFINED);
    while (--ngroups >= 0) VELTS(ans)[ngroups] = MAKINUM(groups[ngroups]);
    return ans;
  }
}

/* These 2 routines are not protected against `entry' being reused
   before access to that structure is completed */

static char s_pwinfo[] = "getpw";
SCM l_pwinfo(user)
     SCM user;
{
  SCM ans = make_vector(MAKINUM(7), UNSPECIFIED);
  struct passwd *entry;
  SCM *ve = VELTS(ans);
  DEFER_INTS;
  if (UNBNDP(user)) SYSCALL(entry = getpwent(););
  else if (INUMP(user)) SYSCALL(entry = getpwuid(INUM(user)););
  else {
    ASRTER(NIMP(user) && STRINGP(user), user, ARG1, s_pwinfo);
    SYSCALL(entry = getpwnam(CHARS(user)););
  }
  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->pw_name);
  ve[ 1] = makfrom0str(entry->pw_passwd);
  ve[ 2] = ulong2num((unsigned long)entry->pw_uid);
  ve[ 3] = ulong2num((unsigned long)entry->pw_gid);
  ve[ 4] = makfrom0str(entry->pw_gecos);
  ve[ 5] = makfrom0str(entry->pw_dir);
  ve[ 6] = makfrom0str(entry->pw_shell);
  return ans;
}
#include <grp.h>
static char s_grinfo[] = "getgr";
SCM l_grinfo(name)
     SCM name;
{
  SCM ans = make_vector(MAKINUM(4), UNSPECIFIED);
  struct group *entry;
  SCM *ve = VELTS(ans);
  DEFER_INTS;
  if (UNBNDP(name)) SYSCALL(entry = getgrent(););
  else if (INUMP(name)) SYSCALL(entry = getgrgid(INUM(name)););
  else {
    ASRTER(NIMP(name) && STRINGP(name), name, ARG1, s_grinfo);
    SYSCALL(entry = getgrnam(CHARS(name)););
  }
  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->gr_name);
  ve[ 1] = makfrom0str(entry->gr_passwd);
  ve[ 2] = ulong2num((unsigned long)entry->gr_gid);
  ve[ 3] = makfromstrs(-1, entry->gr_mem);
  return ans;
}
SCM l_setgr(arg)
     SCM arg;
{
  if (UNBNDP(arg) || FALSEP(arg)) endgrent();
  else setgrent();
  return UNSPECIFIED;
}
SCM l_setpw(arg)
     SCM arg;
{
  if (UNBNDP(arg) || FALSEP(arg)) endpwent();
  else setpwent();
  return UNSPECIFIED;
}

static char s_kill[] = "kill";
SCM l_kill(pid, sig)
     SCM pid, sig;
{
  int i;
  ASRTER(INUMP(pid), pid, ARG1, s_kill);
  ASRTER(INUMP(sig), sig, ARG2, s_kill);
  SYSCALL(i = kill((int)INUM(pid), (int)INUM(sig)););
  return MAKINUM(0L+i);
}
static char s_waitpid[] = "waitpid";
SCM l_waitpid(pid, options)
     SCM pid, options;
{
  int i, status;
  ASRTER(INUMP(pid), pid, ARG1, s_waitpid);
  ASRTER(INUMP(options), options, ARG2, s_waitpid);
  SYSCALL(i = waitpid(INUM(pid), &status, INUM(options)););
  return i < 0 ? BOOL_F : MAKINUM(0L+status);
}

SCM l_getppid()
{
  return MAKINUM(0L+getppid());
}

SCM l_getuid()
{
  return MAKINUM(0L+getuid());
}
SCM l_getgid()
{
  return MAKINUM(0L+getgid());
}
#ifndef LACK_E_IDs
SCM l_geteuid()
{
  return MAKINUM(0L+geteuid());
}
SCM l_getegid()
{
  return MAKINUM(0L+getegid());
}
#endif

static char s_setuid[] = "setuid";
SCM l_setuid(id)
     SCM id;
{
  ASRTER(INUMP(id), id, ARG1, s_setuid);
  return setuid(INUM(id)) ? BOOL_F : BOOL_T;
}
static char s_setgid[] = "setgid";
SCM l_setgid(id)
     SCM id;
{
  ASRTER(INUMP(id), id, ARG1, s_setgid);
  return setgid(INUM(id)) ? BOOL_F : BOOL_T;
}

#ifndef LACK_E_IDs
static char s_seteuid[] = "seteuid";
SCM l_seteuid(id)
     SCM id;
{
  ASRTER(INUMP(id), id, ARG1, s_seteuid);
  return seteuid(INUM(id)) ? BOOL_F : BOOL_T;
}
static char s_setegid[] = "setegid";
SCM l_setegid(id)
     SCM id;
{
  ASRTER(INUMP(id), id, ARG1, s_setegid);
  return setegid(INUM(id)) ? BOOL_F : BOOL_T;
}
#endif

static char s_ttyname[] = "ttyname";
SCM l_ttyname(port)
     SCM port;
{
  char *ans;
  ASRTER(NIMP(port) && OPPORTP(port), port, ARG1, s_ttyname);
  if (tc16_fport != TYP16(port)) return BOOL_F;
  SYSCALL(ans = ttyname(fileno(STREAM(port))););
  /* ans could be overwritten by another call to ttyname */
  return ans ? makfrom0str(ans) : BOOL_F;
}

SCM l_fork()
{
  long pid = 0L + fork();
  return -1L==pid ? BOOL_F : MAKINUM(pid);
}

#include <sys/utsname.h>
SCM l_uname()
{
  struct utsname buf;
  SCM ans = make_vector(MAKINUM(5), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  if (uname(&buf)) return BOOL_F;
  ve[ 0] = makfrom0str(buf.sysname);
  ve[ 1] = makfrom0str(buf.nodename);
  ve[ 2] = makfrom0str(buf.release);
  ve[ 3] = makfrom0str(buf.version);
  ve[ 4] = makfrom0str(buf.machine);
  /* ve[ 5] = makfrom0str(buf.domainname); */
  return ans;
}

static iproc subr0s[] = {
	{"pipe", l_pipe},
	{scm_s_getgroups, scm_getgroups},
	{"getppid", l_getppid},
	{"getuid", l_getuid},
	{"getgid", l_getgid},
#ifndef LACK_E_IDs
	{"getegid", l_getegid},
	{"geteuid", l_geteuid},
#endif
	{"uname", l_uname},
	{"fork", l_fork},
	{0, 0}};

static iproc subr1os[] = {
	{s_pwinfo, l_pwinfo},
	{s_grinfo, l_grinfo},
	{"setpwent", l_setpw},
	{"setgrent", l_setgr},
	{0, 0}};

static iproc subr1s[] = {
	{"setuid", l_setuid},
	{"setgid", l_setgid},
#ifndef LACK_E_IDs
	{"setegid", l_setegid},
	{"seteuid", l_seteuid},
#endif
	{s_ttyname, l_ttyname},
	{0, 0}};

static iproc subr2s[] = {
	{s_link, l_link},
	{s_kill, l_kill},
	{s_waitpid, l_waitpid},
	{s_op_pipe, open_pipe},
	{0, 0}};

static iproc subr3s[] = {
	{s_chown, l_chown},
	{0, 0}};

void init_posix()
{
	init_iprocs(subr0s, tc7_subr_0);
	init_iprocs(subr1s, tc7_subr_1);
	init_iprocs(subr1os, tc7_subr_1o);
	init_iprocs(subr2s, tc7_subr_2);
	init_iprocs(subr3s, tc7_subr_3);
	add_feature("posix");
	ptobs[0x0ff & (tc16_pipe>>8)].name = s_pipe;
	ptobs[0x0ff & (tc16_pipe>>8)].fclose = pclose;
	ptobs[0x0ff & (tc16_pipe>>8)].free = pclose;
	add_feature(s_pipe);
	scm_ldstr("\n\
(define (open-input-pipe cmd) (open-pipe cmd \"r\"))\n\
(define (open-output-pipe cmd) (open-pipe cmd \"w\"))\n\
(define (system->line command . tmp)\n\
  (define line\n\
    (call-with-open-ports\n\
     read-line\n\
     (open-input-pipe command)))\n\
  (if (eof-object? line) \"\" line))\n\
");
}
