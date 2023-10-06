/* "dynl.c" dynamically link&load object files.
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

#include "scm.h"
#ifndef STDC_HEADERS
	int free ();		/* P((char *ptr)) */
#endif

/* linkpath holds the filename which just got linked.  Scheme
   *loadpath* will get set to linkpath and then restored around the
   initialization call */
/* static SCM linkpath; */

#ifdef DLD
# include "dld.h"

void listundefs()
{
  int i;
  char **undefs = dld_list_undefined_sym();
  puts("   undefs:");
  for (i = dld_undefined_sym_count;i--;) {
    putc('"', stdout);
    fputs(undefs[i], stdout);
    puts("\"");
  }
  free(undefs);
}

static char s_link[] = "dyn:link", s_call[] = "dyn:call";
SCM l_dyn_link(fname)
     SCM fname;
{
  int status;
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_link);
  DEFER_INTS;
  status = dld_link(CHARS(fname));
  ALLOW_INTS;
  if (!status) {/* linkpath = fname; */ return fname;}
  if (DLD_ENOFILE==status) return BOOL_F;
  if (DLD_EBADOBJECT==status) return BOOL_F;
  dld_perror("DLD");
  return BOOL_F;
}
SCM l_dyn_call(symb, shl)
     SCM symb, shl;
{
  int i;
  void (*func)() = 0;
/*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_call);
  DEFER_INTS;
  if ((i = dld_function_executable_p(CHARS(symb))))
    func = (void (*) ()) dld_get_func(CHARS(symb));
  else dld_perror("DLDP");
  ALLOW_INTS;
  if (!i) listundefs();
  if (!func) {
    dld_perror("DLD");
    return BOOL_F;
  }
/*  *loc_loadpath = linkpath; */
  (*func) ();
/*  *loc_loadpath = oloadpath; */
  return BOOL_T;
}
static char s_main_call[] = "dyn:main-call";
SCM l_dyn_main_call(symb, shl, args)
     SCM symb, shl, args;
{
  int i;
  int (*func)(int argc, const char **argv) = 0;
  const char **argv;
/*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_main_call);
  DEFER_INTS;
  argv = makargvfrmstrs(args, s_main_call);
  if ((i = dld_function_executable_p(CHARS(symb))))
    func = (int (*) (int argc, const char **argv)) dld_get_func(CHARS(symb));
  else dld_perror("DLDP");
  if (!i) listundefs();
  if (!func) {
    must_free_argv(argv);
    ALLOW_INTS;
    dld_perror("DLD");
    return BOOL_F;
  }
  ALLOW_INTS;
/*  *loc_loadpath = linkpath; */
  i = (*func) ((int)ilength(args), argv);
/*  *loc_loadpath = oloadpath; */
  DEFER_INTS;
  must_free_argv(argv);
  ALLOW_INTS;
  return MAKINUM(0L+i);
}

static char s_unlink[] = "dyn:unlink";
SCM l_dyn_unlink(fname)
     SCM fname;
{
  int status;
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_unlink);
  DEFER_INTS;
  status = dld_unlink_by_file(CHARS(fname), 1);
  ALLOW_INTS;
  if (!status) return BOOL_T;
  dld_perror("DLD");
  return BOOL_F;
}
static iproc subr1s[] = {
	{s_link, l_dyn_link},
	{s_unlink, l_dyn_unlink},
	{0, 0}};
void init_dynl()
{
  /* if (!execpath) execpath = scm_find_execpath(); */
  if ((!execpath) || dld_init(execpath)) {
    dld_perror("DLD:");
    return;
  }
  if (!dumped) {
    init_iprocs(subr1s, tc7_subr_1);
    make_subr(s_call, tc7_subr_2, l_dyn_call);
    make_subr(s_main_call, tc7_lsubr_2, l_dyn_main_call);
    add_feature("dld");
# ifdef DLD_DYNCM
    add_feature("dld:dyncm");
# endif
  }
}
#else

# ifdef hpux
#  include "dl.h"

#  define P_SHL(obj) ((shl_t*)(&CDR(obj)))
#  define SHL(obj) (*P_SHL(obj))
int prinshl(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  lputs("#<shl ", port);
  scm_intprint(CDR(exp), -16, port);
  lputc('>', port);
  return 1;
}
int tc16_shl;
static smobfuns shlsmob = {mark0, free0, prinshl};

static char s_link[] = "dyn:link", s_call[] = "dyn:call";
SCM l_dyn_link(fname)
     SCM fname;
{
  SCM z;
  shl_t shl;
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_link);
  NEWCELL(z);
  DEFER_INTS;
  shl = shl_load(CHARS(fname), BIND_DEFERRED , 0L);
  if (NULL==shl) {
    ALLOW_INTS;
    return BOOL_F;
  }
  SETCHARS(z, shl);
  CAR(z) = tc16_shl;
  ALLOW_INTS;
/*  linkpath = fname; */
  return z;
}
SCM l_dyn_call(symb, shl)
     SCM symb, shl;
{
  void (*func)() = 0;
  int i;
/*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_call);
  DEFER_INTS;
  if ((i = shl_findsym(P_SHL(shl),
		       CHARS(symb),
		       TYPE_PROCEDURE, &func)) != 0) {
    puts("    undef:"); puts(CHARS(symb));
  }
  ALLOW_INTS;
  if (i != 0) return BOOL_F;
/*  *loc_loadpath = linkpath; */
  (*func) ();
/*  *loc_loadpath = oloadpath; */
  return BOOL_T;
}

static char s_main_call[] = "dyn:main-call";
SCM l_dyn_main_call(symb, shl, args)
     SCM symb, shl, args;
{
  int i;
  int (*func)P((int argc, const char **argv)) = 0;
  const char **argv;
/*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_main_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_main_call);
  DEFER_INTS;
  if ((i = shl_findsym(P_SHL(shl),
		       CHARS(symb),
		       TYPE_PROCEDURE, &func)) != 0) {
    puts("    undef:"); puts(CHARS(symb));
  }
  argv = makargvfrmstrs(args, s_main_call);
  ALLOW_INTS;
  if (i != 0) return BOOL_F;
/*  *loc_loadpath = linkpath; */
  i = (*func) ((int)ilength(args), argv);
/*  *loc_loadpath = oloadpath; */
  DEFER_INTS;
  must_free_argv(argv);
  ALLOW_INTS;
  return MAKINUM(0L+i);
}

static char s_unlink[] = "dyn:unlink";
SCM l_dyn_unlink(shl)
     SCM shl;
{
  int status;
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG1, s_unlink);
  DEFER_INTS;
  status = shl_unload(SHL(shl));
  ALLOW_INTS;
  if (!status) return BOOL_T;
  return BOOL_F;
}
static iproc subr1s[] = {
	{s_link, l_dyn_link},
	{s_unlink, l_dyn_unlink},
	{0, 0}};
void init_dynl()
{
  if (!dumped) {
    tc16_shl = newsmob(&shlsmob);
    init_iprocs(subr1s, tc7_subr_1);
    make_subr(s_call, tc7_subr_2, l_dyn_call);
    make_subr(s_main_call, tc7_lsubr_2, l_dyn_main_call);
    add_feature("shl");
  }
}
# endif
#endif

#ifdef vms
/* This permits dynamic linking. For example, the procedure of 0 arguments
   from a file could be the initialization procedure.
   (vms:dynamic-link-call "MYDISK:[MYDIR].EXE" "foo" "INIT_FOO")
   The first argument specifies the directory where the file specified
   by the second argument resides.  The current directory would be
   "SYS$DISK:[].EXE".
   The second argument cannot contain any punctuation.
   The third argument probably needs to be uppercased to mimic the VMS linker.
   */

# include <descrip.h>
# include <ssdef.h>
# include <rmsdef.h>

struct dsc$descriptor *descriptorize(x, buff)
     struct dsc$descriptor *x;
     SCM buff;
{(*x).dsc$w_length = LENGTH(buff);
 (*x).dsc$a_pointer = CHARS(buff);
 (*x).dsc$b_class = DSC$K_CLASS_S;
 (*x).dsc$b_dtype = DSC$K_DTYPE_T;
 return(x);}

static char s_dynl[] = "vms:dynamic-link-call";
SCM dynl(dir, symbol, fname)
     SCM dir, symbol, fname;
{
  struct dsc$descriptor fnamed, symbold, dird;
  void (*fcn)();
  long retval;
  ASRTER(IMP(dir) || STRINGP(dir), dir, ARG1, s_dynl);
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG2, s_dynl);
  ASRTER(NIMP(symbol) && STRINGP(symbol), symbol, ARG3, s_dynl);
  descriptorize(&fnamed, fname);
  descriptorize(&symbold, symbol);
  DEFER_INTS;
  retval = lib$find_image_symbol(&fnamed, &symbold, &fcn,
				 IMP(dir) ? 0 : descriptorize(&dird, dir));
  if (SS$_NORMAL != retval) {
    /* wta(MAKINUM(retval), "vms error", s_dynl); */
    ALLOW_INTS;
    return BOOL_F;
  }
  ALLOW_INTS;
/*  *loc_loadpath = dir; */
  (*fcn)();
/*  *loc_loadpath = oloadpath; */
  return BOOL_T;
}

void init_dynl()
{
  if (!dumped) {
  make_subr(s_dynl, tc7_subr_3, dynl);
  }
}
#endif


#ifdef SUN_DL
# include <dlfcn.h>

# define SHL(obj) ((void*)CDR(obj))

# ifdef RTLD_GLOBAL
#  define DLOPEN_MODE (RTLD_NOW | RTLD_GLOBAL)
# else
#  ifdef RTLD_LAZY		/* This is here out of conservatism, not
				   because it's known to be right. */
#   define DLOPEN_MODE RTLD_LAZY
#  else
#   define DLOPEN_MODE 1	/* Thats what it says in the man page. */
#  endif
# endif

sizet frshl(ptr)
	CELLPTR ptr;
{
# if 0
  /* Should freeing a shl close and possibly unmap the object file it */
  /* refers to? */
  if (SHL(ptr))
    dlclose(SHL(ptr));
# endif
  return 0;
}

int prinshl(exp, port, writing)
	SCM exp; SCM port; int writing;
{
  lputs("#<shl ", port);
  scm_intprint(CDR(exp), -16, port);
  lputc('>', port);
  return 1;
}
int tc16_shl;
static smobfuns shlsmob = {mark0, frshl, prinshl};

static char s_link[] = "dyn:link", s_call[] = "dyn:call";
SCM l_dyn_link(fname)
	SCM fname;
{
  SCM z;
  void *handle;
  if (FALSEP(fname)) return fname;
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_link);
  NEWCELL(z);
  DEFER_INTS;
  handle = dlopen(CHARS(fname), DLOPEN_MODE);
  if (NULL==handle) {
    if (scm_verbose > 1) {
      char *dlr = dlerror();
      ALLOW_INTS;
      if (dlr) {
	lputs(s_link, cur_errp);
	lputs(": ", cur_errp);
	lputs(dlr, cur_errp);
	scm_newline(cur_errp);
      }}
    return BOOL_F;
  }
  SETCHARS(z, handle);
  CAR(z) = tc16_shl;
  ALLOW_INTS;
/*  linkpath = fname; */
  return z;
}

SCM l_dyn_call(symb, shl)
	SCM symb, shl;
{
  void (*func)() = 0;
/*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_call);
  DEFER_INTS;
  func = dlsym(SHL(shl), CHARS(symb));
  if (!func) {
    char *dlr = dlerror();
    ALLOW_INTS;
    if (dlr) {
      lputs(s_call, cur_errp);
      lputs(": ", cur_errp);
      lputs(dlr, cur_errp);
      scm_newline(cur_errp);
    }
    return BOOL_F;
  }
  ALLOW_INTS;
/*  *loc_loadpath = linkpath; */
  (*func) ();
/*  *loc_loadpath = oloadpath; */
  return BOOL_T;
}
static char s_main_call[] = "dyn:main-call";
SCM l_dyn_main_call(symb, shl, args)
	SCM symb, shl, args;
{
  int i;
  int (*func)P((int argc, const char **argv)) = 0;
  char **argv;
/*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_main_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_main_call);
  DEFER_INTS;
  func = dlsym(SHL(shl), CHARS(symb));
  if (!func) {
    char *dlr = dlerror();
    ALLOW_INTS;
    if (dlr) {
      lputs(s_main_call, cur_errp);
      lputs(": ", cur_errp);
      lputs(dlr, cur_errp);
      scm_newline(cur_errp);
    }
    return BOOL_F;
  }
  argv = makargvfrmstrs(args, s_main_call);
  ALLOW_INTS;
/*  *loc_loadpath = linkpath; */
  i = (*func) ((int)ilength(args), (const char**)argv);
/*  *loc_loadpath = oloadpath; */
  DEFER_INTS;
  must_free_argv(argv);
  ALLOW_INTS;
  return MAKINUM(0L+i);
}

static char s_unlink[] = "dyn:unlink";
SCM l_dyn_unlink(shl)
	SCM shl;
{
  int status;
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG1, s_unlink);
  DEFER_INTS;
  status = dlclose(SHL(shl));
  SETCHARS(shl, NULL);
  ALLOW_INTS;
  if (!status) return BOOL_T;
  return BOOL_F;
}
static iproc subr1s[] = {
{s_link, l_dyn_link},
{s_unlink, l_dyn_unlink},
{0, 0}};

void init_dynl()
{
  if (!dumped) {
    tc16_shl = newsmob(&shlsmob);
    init_iprocs(subr1s, tc7_subr_1);
    make_subr(s_call, tc7_subr_2, l_dyn_call);
    make_subr(s_main_call, tc7_lsubr_2, l_dyn_main_call);
    add_feature("sun-dl");
  }
}
#endif	/* SUN_DL */

#ifdef macintosh

# include <CodeFragments.h>
# include <Errors.h>

# define SHL(obj) ((void*)CDR(obj))

sizet frshl(ptr)
	CELLPTR ptr;
{
# if 0
  /* Should freeing a shl close and possibly unmap the object file it */
  /* refers to? */
  if (SHL(ptr))
    dlclose(SHL(ptr));
# endif
  return 0;
}

int prinshl(exp, port, writing)
	SCM exp; SCM port; int writing;
{
  lputs("#<shl ", port);
  scm_intprint(CDR(exp), 16, port);
  lputc('>', port);
  return 1;
}
int tc16_shl;
static smobfuns shlsmob = {mark0, frshl, prinshl};

static char s_link[] = "dyn:link", s_call[] = "dyn:call";
SCM l_dyn_link(fname)
	SCM fname;
{
  OSErr err;
  SCM z;
  void *handle;
  Str63 libName;
  CFragConnectionID connID;
  Ptr mainAddr;
  Str255 errMessage;

  if (FALSEP(fname)) return fname;
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_link);
  NEWCELL(z);
  DEFER_INTS;
  strcpy((char *)libName,  CHARS(fname));
  c2pstr((char *)libName);
  err = GetSharedLibrary (libName, kCompiledCFragArch, kReferenceCFrag,
			  &connID, &mainAddr, errMessage);

  if (err!=noErr) {
    ALLOW_INTS;
    return BOOL_F;
  }
  SETCHARS(z, (void *)connID);
  CAR(z) = tc16_shl;
  ALLOW_INTS;
  /*  linkpath = fname; */
  return z;
}

SCM l_dyn_call(symb, shl)
	SCM symb, shl;
{
  void (*func)() = 0;
  OSErr err;
  CFragSymbolClass symClass;
  Str255 symName;

  /*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_call);
  DEFER_INTS;

  strcpy((char *)symName, CHARS(symb));
  c2pstr((char *)symName);
  err = FindSymbol((CFragConnectionID)SHL(shl), symName,
		   (Ptr *)&func, &symClass);
  if (err!=noErr /* || symClass != kCodeCFragSymbol */) {
    ALLOW_INTS;
    if (err == cfragConnectionIDErr) puts("Invalid library connection.");
    if (err == cfragNoSymbolErr) puts("Symbol not found.");
    return BOOL_F;
  }
  ALLOW_INTS;
  /*  *loc_loadpath = linkpath; */
  (*func) ();
  /*  *loc_loadpath = oloadpath; */
  return BOOL_T;
}
static char s_main_call[] = "dyn:main-call";
SCM l_dyn_main_call(symb, shl, args)
	SCM symb, shl, args;
{
  int i;
  int (*func)P((int argc, const char **argv)) = 0;
  const char **argv;
  OSErr err;
  CFragSymbolClass symClass;
  Str255 symName;

  /*  SCM oloadpath = *loc_loadpath; */
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_main_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_main_call);
  DEFER_INTS;
  strcpy((char *)symName, CHARS(symb));
  c2pstr((char *)symName);
  err = FindSymbol((CFragConnectionID)SHL(shl), symName,
		   (Ptr *)&func, &symClass);
  if (err!=noErr || symClass != kCodeCFragSymbol) {
    ALLOW_INTS;
    if (err == cfragConnectionIDErr) puts("Invalid library connection.");
    if (err == cfragNoSymbolErr) puts("Symbol not found.");
    return BOOL_F;
  }
  argv = makargvfrmstrs(args, s_main_call);
  ALLOW_INTS;
  /*  *loc_loadpath = linkpath; */
  i = (*func) ((int)ilength(args), argv);
  /*  *loc_loadpath = oloadpath; */
  DEFER_INTS;
  must_free_argv(argv);
  ALLOW_INTS;
  return MAKINUM(0L+i);
}

static char s_unlink[] = "dyn:unlink";
SCM l_dyn_unlink(shl)
	SCM shl;
{
  OSErr status;
  CFragConnectionID connID;

  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG1, s_unlink);
  DEFER_INTS;
  connID = (CFragConnectionID)SHL(shl);
  status = CloseConnection(&connID);
  SETCHARS(shl, NULL);
  ALLOW_INTS;
  if (status!=noErr) return BOOL_T;
  return BOOL_F;
}
static iproc subr1s[] = {
  {s_link, l_dyn_link},
  {s_unlink, l_dyn_unlink},
  {0, 0}};

void init_dynl()
{
  if (!dumped) {
    tc16_shl = newsmob(&shlsmob);
    init_iprocs(subr1s, tc7_subr_1);
    make_subr(s_call, tc7_subr_2, l_dyn_call);
    make_subr(s_main_call, tc7_lsubr_2, l_dyn_main_call);
    add_feature("mac-dl");
  }
}
#endif	/* MACOS */

#ifdef _WIN32
# include <windows.h>
# define SHL(obj) ((HINSTANCE)(CDR(obj)))
int prinshl(exp, port, writing)
	SCM exp; SCM port; int writing;
{
  lputs("#<shl ", port);
  scm_intprint(CDR(exp), 16, port);
  lputc('>', port);
  return 1;
}

int tc16_shl;
static smobfuns shlsmob = {mark0, free0, prinshl};

static char s_link[] = "dyn:link";
SCM scm_dyn_link(fname)
     SCM fname;
{
  SCM z, shl = BOOL_F;
  HINSTANCE hshl;
  ASRTER(NIMP(fname) && STRINGP(fname), fname, ARG1, s_link);
  NEWCELL(z);
  DEFER_INTS;
  hshl = LoadLibrary(CHARS(fname));
  if (hshl) {
    SETCHARS(z, hshl);
    CAR(z) = tc16_shl;
    shl = z;
  }
  ALLOW_INTS;
  return shl;
}

static char s_unlink[] = "dyn:unlink";
SCM scm_dyn_unlink(shl)
     SCM shl;
{
  BOOL status;
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG1, s_unlink);
  DEFER_INTS;
  status = FreeLibrary(SHL(shl));
  ALLOW_INTS;
  return status ? BOOL_T : BOOL_F;
}

static char s_call[] = "dyn:call";
SCM scm_dyn_call(symb, shl)
     SCM symb, shl;
{
  FARPROC func;
  int i;
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_call);
  DEFER_INTS;
  func = GetProcAddress(SHL(shl), CHARS(symb));
  ALLOW_INTS;
  if (!func) return BOOL_F;
  (*func) ();
  return BOOL_T;
}

static char s_main_call[] = "dyn:main-call";
SCM scm_dyn_main_call(symb, shl, args)
	SCM symb, shl, args;
{
  int i;
  FARPROC func;
  const char **argv;
  ASRTER(NIMP(symb) && STRINGP(symb), symb, ARG1, s_main_call);
  ASRTER(NIMP(shl) && CAR(shl)==tc16_shl, shl, ARG2, s_main_call);
  DEFER_INTS;
  func = GetProcAddress(SHL(shl), CHARS(symb));
  if (!func) {
    ALLOW_INTS;
    return BOOL_F;
  }
  argv = makargvfrmstrs(args, s_main_call);
  ALLOW_INTS;
  i = (*func) ((int)ilength(args), argv);
  DEFER_INTS;
  must_free_argv(argv);
  ALLOW_INTS;
  return MAKINUM(0L+i);
}

static iproc subr1s[] = {
  {s_link, scm_dyn_link},
  {s_unlink, scm_dyn_unlink},
  {0, 0}};

void init_dynl()
{
  if (!dumped) {
    tc16_shl = newsmob(&shlsmob);
    init_iprocs(subr1s, tc7_subr_1);
    make_subr(s_call, tc7_subr_2, scm_dyn_call);
    make_subr(s_main_call, tc7_lsubr_2, scm_dyn_main_call);
    add_feature("win32-dl");
  }
}
#endif
