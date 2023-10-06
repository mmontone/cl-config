/* "script.c" argv tricks for `#!' scripts.
 * Copyright (C) 1994-1999 Free Software Foundation, Inc.
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

#include <ctype.h>
#include "scm.h"

#ifdef __IBMC__
# include <io.h>
#endif /* def __IBMC__ */

#ifdef linux
# include <unistd.h>     /* for X_OK define */
#endif /* def linux */
#ifdef __SVR4
# include <unistd.h>     /* for X_OK define */
#else
# ifdef __sgi__
#  include <unistd.h>     /* for X_OK define */
# endif /* def __sgi__ */
#endif /* def __SVR4 */

#ifdef _WIN32
# define WIN32_LEAN_AND_MEAN
# include <windows.h>           /* GetModuleFileName */
#endif

/* Concatentate str2 onto str1 at position n and return concatenated
   string if file exists; 0 otherwise. */

char *scm_cat_path(str1, str2, n)
     char *str1;
     const char *str2;
     long n;
{
  if (!n) n = strlen(str2);
  if (str1)
    {
      long len = strlen(str1);
      str1 = (char *)realloc(str1, (sizet)(len + n + 1));
      if (!str1) return 0L;
      strncat(str1 + len, str2, n);
      return str1;
    }
  str1 = (char *)malloc((sizet)(n + 1));
  if (!str1) return 0L;
  str1[0] = 0;
  strncat(str1, str2, n);
  return str1;
}

char *scm_try_path(path)
     char *path;
{
  FILE *f;
  /* fprintf(stderr, "Trying %s\n", path);fflush(stderr); */
  if (!path) return 0L;
  SYSCALL(f = fopen(path, "r"););
  if (f) {
    fclose(f);
    return path;
  }
  free(path);
  return 0L;
}

char *scm_sep_init_try(path, sep, initname)
     char *path;
     const char *sep, *initname;
{
  if (path) path = scm_cat_path(path, sep, 0L);
  if (path) path = scm_cat_path(path, initname, 0L);
  return scm_try_path(path);
}

#ifndef LINE_INCREMENTORS
# define LINE_INCREMENTORS  '\n'
# ifdef MSDOS
#  define WHITE_SPACES  ' ':case '\t':case '\r':case '\f':case 26
# else
#  define WHITE_SPACES  ' ':case '\t':case '\r':case '\f'
# endif /* def MSDOS */
#endif /* ndef LINE_INCREMENTORS */

#ifndef MAXPATHLEN
# define MAXPATHLEN 80
#endif /* ndef MAXPATHLEN */
#ifndef X_OK
# define X_OK 1
#endif /* ndef X_OK */

#ifdef HAVE_UNIX
# include <stdio.h>

char *script_find_executable(name)
     const char *name;
{
  char tbuf[MAXPATHLEN];
  int i = 0;
  FILE *f;

  /* fprintf(stderr, "s_f_e checking access %s ->%d\n", name, access(name, X_OK)); fflush(stderr); */
  if (access(name, X_OK)) return 0L;
  f = fopen(name, "r");
  if (!f) return 0L;
  if ((fgetc(f)=='#') && (fgetc(f)=='!')) {
    while (1) switch (tbuf[i++] = fgetc(f)) {
    case ' ':
      if (1==i) {i--; break;}
    case '\t':case '\r':case '\f':
    case EOF:
      tbuf[--i] = 0;
      fclose(f);
      if (0==i) return 0L;
      return scm_cat_path(0L, tbuf, 0L);
    }
  }
  fclose(f);
  return scm_cat_path(0L, name, 0L);
}
#endif /* unix */

/* Given dld_find_executable()'s best guess for the pathname of this
   executable, find (and verify the existence of) initname in the
   implementation-vicinity of this program.  Returns a newly allocated
   string if successful, 0 if not */

char *find_impl_file(exec_path, generic_name, initname, sep)
     const char *exec_path;
     const char *generic_name, *initname, *sep;
{
  char *sepptr = strrchr(exec_path, sep[0]);
  char *extptr = exec_path + strlen(exec_path);
  char *path = 0;

#ifdef _WIN32
  char exec_buf[MAX_PATH];
  HMODULE mod = GetModuleHandle(0); /* Returns module handle to current executable. */

  if (mod) {
    GetModuleFileName(mod, exec_buf, sizeof(exec_buf));
    exec_path = exec_buf;
  }
#endif

  /*fprintf(stderr, "dld_find_e %s\n", exec_path); fflush(stderr);*/

  sepptr = strrchr(exec_path, sep[0]);
  extptr = exec_path + strlen(exec_path);

  if (sepptr) {
    long sepind = sepptr - exec_path + 1L;

    /* In case exec_path is in the source directory, look first in
       exec_path's directory. */
    path = scm_cat_path(0L, exec_path, sepind - 1L);
    path = scm_sep_init_try(path, sep, initname);
    if (path) return path;

#ifdef MSDOS
    if (!strcmp(extptr - 4, ".exe") || !strcmp(extptr - 4, ".com") ||
	!strcmp(extptr - 4, ".EXE") || !strcmp(extptr - 4, ".COM"))
      extptr = extptr - 4;
#endif /* def MSDOS */

    if (generic_name &&
	!strncmp(exec_path + sepind, generic_name, extptr - exec_path))
      generic_name = 0;

    /* If exec_path is in directory "exe" or "bin": */
    path = scm_cat_path(0L, exec_path, sepind - 1L);
    sepptr = path + sepind - 4;
    if (!strcmp(sepptr, "exe") || !strcmp(sepptr, "bin") ||
	!strcmp(sepptr, "EXE") || !strcmp(sepptr, "BIN")) {
      char *peer;

      /* Look for initname in peer directory "lib". */
      if (path) {
	strncpy(sepptr, "lib", 3);
	path = scm_sep_init_try(path, sep, initname);
	if (path) return path;
      }

      /* Look for initname in peer directories "lib" and "src" in
	 subdirectory with the name of the executable (sans any type
	 extension like .EXE). */
      for (peer="lib";!0;peer="src") {
	path = scm_cat_path(0L, exec_path, extptr - exec_path + 0L);
	if (path) {
	  strncpy(path + sepind - 4, peer, 3);
	  path[extptr - exec_path] = 0;
	  path = scm_sep_init_try(path, sep, initname);
	  if (path) return path;
	}
	if (!strcmp(peer, "src")) break;
      }

      if (generic_name) {

	/* Look for initname in peer directories "lib" and "src" in
	   subdirectory with the generic name. */
	for (peer="lib";!0;peer="src") {
	  path = scm_cat_path(0L, exec_path, sepind);
	  if (path) {
	    strncpy(path + sepind - 4, peer, 3);
	    path = scm_cat_path(path, generic_name, 0L);
	    path = scm_sep_init_try(path, sep, initname);
	    if (path) return path;
	  }
	  if (!strcmp(peer, "src")) break;
	}}

      /* Look for initname in executable-name peer directory. */
      path = scm_cat_path(0L, exec_path, sepind);
      if (path) {
	path[sepind - 4] = 0;
	path = scm_cat_path(path, &exec_path[sepind], 0L);
	path = scm_sep_init_try(path, sep, initname);
	if (path) return path;
      }

      if (generic_name) {

	/* Look for initname in generic peer directory. */
	path = scm_cat_path(0L, exec_path, sepind);
	if (path) {
	  path[sepind - 4] = 0;
	  path = scm_cat_path(path, generic_name, 0L);
	  path = scm_sep_init_try(path, sep, initname);
	  if (path) return path;
	}
      }
    }

#ifdef MSDOS
    if (strlen(extptr)) {
      /* If exec_path has type extension, look in a subdirectory with
	 the name of the executable sans the executable file's type
	 extension. */
      path = scm_cat_path(0L, exec_path, extptr - exec_path + 0L);
      path = scm_sep_init_try(path, sep, initname);
      if (path) return path;

      if (generic_name) {

	/* Also look in generic_name subdirectory. */
	path = scm_cat_path(0L, exec_path, sepind);
	if (path) path = scm_cat_path(path, generic_name, 0L);
	path = scm_sep_init_try(path, sep, initname);
	if (path) return path;
      }}
#endif /* def MSDOS */
  }
  else {

    /* We don't have a parse-able exec_path.  The only path to try is
       just initname. */
    path = scm_cat_path(0L, initname, 0L);
    if (path) path = scm_try_path(path);
    if (path) return path;
  }
  return 0L;
}

char *script_read_arg(f)
     FILE *f;
{
  sizet tlen = 1;
  int tind = 0, qted = 0, chr;
  char *tbuf = (char *)malloc((1 + tlen) * sizeof(char));
  if (!tbuf) return 0L;
  while (1) switch (chr = getc(f)) {
  case WHITE_SPACES:
    continue;
  case LINE_INCREMENTORS:
  case EOF:
    free(tbuf);
    return 0L;
  default:
    goto morearg;
  }
morearg:
  while (1) {
    switch (tbuf[tind++] = chr) {
    case WHITE_SPACES:
    case LINE_INCREMENTORS:
      if (qted) break;
    case EOF: goto endarg;
    case '!':
      if (qted) break;
      switch (chr = getc(f)) {
      case '#':
	if (1==tind) return 0L;
	goto endarg;
      default: tbuf[tind++] = chr; break;
      }
      break;
    case '"': qted = !qted; tind--; break;
    case '\\':
      switch (tbuf[tind - 1] = getc(f)) {
      case '\n': --tind; break;
      case 'n': tbuf[tind - 1] = '\n'; break;
      case 'r': tbuf[tind - 1] = '\r'; break;
      case 't': tbuf[tind - 1] = '\t'; break;
      case 'b': tbuf[tind - 1] = '\b'; break;
	/* case '0': tbuf[tind - 1] = '\0'; break; */
      default:;
      }
    default:;
    }
    if (tind >= tlen) {
      tbuf = (char *)realloc(tbuf, (1 + (2 * tlen)) * sizeof(char));
      if (!tbuf) return 0L;
      tlen = 2 * tlen;
    }
    chr = getc(f);
  }
endarg:
  tbuf[--tind] = 0;
  return tbuf;
}

int script_meta_arg_P(arg)
     const char *arg;
{
  if ('\\' != arg[0]) return 0L;
#ifdef MSDOS
  return !arg[1];
#else
  switch (arg[1]) {
  case 0:
  case '%':
  case WHITE_SPACES: return !0;
  default: return 0L;}
#endif
}

char **script_process_argv(argc, argv)
     int argc;
     const char **argv;
{
  int nargc = argc, argi = 1, nargi = 1;
  char *narg, **nargv;
  if (!(argc > 2 && script_meta_arg_P(argv[1]))) return 0L;
  if (!(nargv = (char **)malloc((1 + nargc) * sizeof(char *)))) return 0L;
  nargv[0] = argv[0];
  while (((argi+1) < argc) && (script_meta_arg_P(argv[argi]))) {
    FILE *f = fopen(argv[++argi], "r");
    if (f) {
      nargc--;		/* to compensate for replacement of '\\' */
      while (1) switch (getc(f)) {
      case EOF: return 0L;
      default: continue;
      case '\n': goto found_args;
      }
    found_args: while ((narg = script_read_arg(f)))
      if (!(nargv = (char **)realloc(nargv, (1 + ++nargc) * sizeof(char *))))
	return 0L;
      else nargv[nargi++] = narg;
      fclose(f);
      nargv[nargi++] = argv[argi++];
    }
  }
  while (argi <= argc) nargv[nargi++] = argv[argi++];
  return nargv;
}

int script_count_argv(argv)
     const char **argv;
{
  int argc = 0;
  while (argv[argc]) argc++;
  return argc;
}

#ifdef __IBMC__
# define MSDOS
#endif

#ifdef MSDOS
char *dld_find_executable(file)
     const char *file;
{
  /* fprintf(stderr, "dld_find_executable %s -> %s\n", file, scm_cat_path(0L, file, 0L)); fflush(stderr); */
  return scm_cat_path(0L, file, 0L);
}
#endif /* def MSDOS */
