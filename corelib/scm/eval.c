/* "eval.c" eval and apply.
 * Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1997, 1998, 1999, 2002, 2006 Free Software Foundation, Inc.
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

/* Authors: Radey Shouman, Aubrey Jaffer, & Hugh E. Secker-Walker. */

#include "scm.h"
#include "setjump.h"

#ifdef _M_ARM
/* The Microsoft CLARM compiler has a bug in pointer arithmetic.
   It doesn't always take into account that data acceses have to be
   DWORD aligned.  The MS_CLARM_dumy assignment resolves this problem.  */
# define I_SYM(x) (CAR((SCM)(MS_CLARM_dumy = (x)-1L)))
# define I_VAL(x) (CDR((SCM)(MS_CLARM_dumy = (x)-1L)))
#else
# define I_SYM(x) (CAR((x)-1L))
# define I_VAL(x) (CDR((x)-1L))
#endif
#define ATOMP(x) (5==(5 & (int)CAR(x)))
#define EVALCELLCAR(x) (ATOMP(CAR(x))?evalatomcar(x, 0):ceval_1(CAR(x)))
#define EVALIMP(x) (ILOCP(x)?*ilookup(x):x)
#define EVALCAR(x) (NCELLP(CAR(x))?(IMP(CAR(x))?EVALIMP(CAR(x)):\
					I_VAL(CAR(x))):EVALCELLCAR(x))

/* Environment frames are initially allocated in a small cache ("ecache").
  This cache is subject to copying gc, cells in it may be moved to the
  general purpose Scheme heap by a call to any routine that allocates cells
  in the cache.

  Global variables scm_env and scm_env_tmp are used as software
  registers: scm_env is the current lexical environment, scm_env_tmp
  is used for protecting environment frames under construction and not
  yet linked into the environment.

  In order to protect environments from garbage collection, a stack of
  environments (scm_estk) is maintained. scm_env and scm_env_tmp may
  be pushed on or popped off the stack using the macros ENV_PUSH and
  ENV_POP.

  It is not safe to pass objects that may allocated in the ecache as
  arguments to C functions, or to return them from C functions, since
  such objects may be moved by the ecache gc.  Ecache gc may happen
  anywhere interrupts are not deferred, because some interrupt
  handlers may evaluate Scheme code and then return.

  Interrupts may be deferred with DEFER_INTS_EGC: This will prevent
  interrupts until an ALLOW_INTS or ALLOW_INTS_EGC, which may happen
  any time Scheme code is evaluated.  It is not necessary to strictly
  nest DEFER_INTS_EGC and ALLOW_INTS_EGC since ALLOW_INTS_EGC is
  called in ceval_1 before any subrs are called.

  Instead of using the C stack and deferring interrupts, objects which
  might have been allocated in the ecache may be passed using the
  global variables scm_env_tmp and scm_env.

  If the CAR of a cell that might be allocated in the regular heap is
  made to point to a cell allocated in the cache, then the first cell
  must be recorded as a gc root, using the macro EGC_ROOT.  There is
  no provision for allowing the CDR of a regular cell to point to a
  cache cell.  */

#ifdef NO_ENV_CACHE
# define scm_env_cons(a, b) {scm_env_tmp=cons((a), (b));}
# define scm_env_cons2(a, b, c) {scm_env_tmp=cons2((a), (b), (c));}
# define scm_env_cons3(a, b, c, d) {scm_env_tmp=cons2((a), (b), cons((c), (d)));}
# define EXTEND_VALENV {scm_env=cons(scm_env_tmp, scm_env);}
# define ENV_V2LST(argc, argv) \
   {scm_env_tmp=scm_v2lst((argc), (argv), scm_env_tmp);}
#else
# define EXTEND_VALENV {scm_extend_env();}
# define ENV_V2LST scm_env_v2lst
#endif
#define EXTEND_ENV cons

SCM scm_env, scm_env_tmp;
long tc16_env;			/* Type code for environments passed to macro
				   transformers. */

SCM nconc2copy P((SCM x));
SCM scm_cp_list P((SCM x, int minlen));
SCM scm_v2lst P((long argc, SCM *argv, SCM end));
SCM renamed_ident P((SCM id, SCM env));
SCM eqv P((SCM x, SCM y));
SCM scm_multi_set P((SCM syms, SCM vals));
SCM eval_args P((SCM x));
SCM m_quote P((SCM xorig, SCM env, SCM ctxt));
SCM m_begin P((SCM xorig, SCM env, SCM ctxt));
SCM m_if P((SCM xorig, SCM env, SCM ctxt));
SCM m_set P((SCM xorig, SCM env, SCM ctxt));
SCM m_and P((SCM xorig, SCM env, SCM ctxt));
SCM m_or P((SCM xorig, SCM env, SCM ctxt));
SCM m_cond P((SCM xorig, SCM env, SCM ctxt));
SCM m_case P((SCM xorig, SCM env, SCM ctxt));
SCM m_lambda P((SCM xorig, SCM env, SCM ctxt));
SCM m_letstar P((SCM xorig, SCM env, SCM ctxt));
SCM m_do P((SCM xorig, SCM env, SCM ctxt));
SCM m_quasiquote P((SCM xorig, SCM env, SCM ctxt));
SCM m_delay P((SCM xorig, SCM env, SCM ctxt));
SCM m_define P((SCM xorig, SCM env, SCM ctxt));
SCM m_letrec P((SCM xorig, SCM env, SCM ctxt));
SCM m_let P((SCM xorig, SCM env, SCM ctxt));
SCM m_apply P((SCM xorig, SCM env, SCM ctxt));
SCM m_syn_quote P((SCM xorig, SCM env, SCM ctxt));
SCM m_let_syntax P((SCM xorig, SCM env, SCM ctxt));
SCM m_letrec_syntax P((SCM xorig, SCM env, SCM ctxt));
SCM m_the_macro P((SCM xorig, SCM env, SCM ctxt));
void scm_dynthrow P((SCM cont, SCM arg1, SCM arg2, SCM rest));
void scm_egc P((void));
void scm_estk_grow P((void));
void scm_estk_shrink P((void));
int badargsp P((SCM formals, SCM args));

static SCM *lookupcar P((SCM vloc));
static SCM scm_lookupval P((SCM vloc, int memo));
static SCM asubr_apply P((SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM args));
static SCM ceval_1 P((SCM x));
static SCM evalatomcar P((SCM x, int no_error));
static SCM evalcar P((SCM x));
static SCM id2sym P((SCM id));
static SCM iqq P((SCM form));
static SCM m_body P((SCM xorig, SCM env, SCM ctxt));
static SCM m_iqq P((SCM form, int depth, SCM env, SCM ctxt));
static SCM m_parse_let P((SCM imm, SCM xorig, SCM x, SCM *vars, SCM *inits));
static SCM m_let_null P((SCM body, SCM env, SCM ctxt));
static SCM m_letrec1 P((SCM imm, SCM xorig, SCM env, SCM ctxt));
static SCM m_letstar1 P((SCM imm, SCM vars, SCM inits, SCM body,
			 SCM env, SCM ctxt));
static SCM macroexp1 P((SCM x, SCM env, SCM ctxt, int mode));
/* static int checking_defines_p P((SCM ctxt)); */
/* static SCM wrapenv P((void)); */
static SCM scm_case_selector P((SCM x));
static SCM acro_call P((SCM x, SCM env));
static SCM m_binding P((SCM name, SCM value, SCM env, SCM ctxt));
static SCM m_bindings P((SCM name, SCM value, SCM env, SCM ctxt));
static SCM m_seq P((SCM x, SCM env, SCM ctxt));
static SCM m_expr P((SCM x, SCM env, SCM ctxt));
static void checked_define P((SCM name, SCM val, const char *what));
static int topdenote_eq P((SCM sym, SCM id, SCM env));
static int constant_p P((SCM x));
static int prinenv P((SCM exp, SCM port, int writing));
static int prinid P((SCM exp, SCM port, int writing));
static int prinmacro P((SCM exp, SCM port, int writing));
static int prinprom P((SCM exp, SCM port, int writing));
#ifdef MAC_INLINE
static int env_depth P((void));
static void env_tail P((int depth));
#endif
static void unpaint P((SCM *p));
static void ecache_evalx P((SCM x));
static int ecache_eval_args P((SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM x));
static int varcheck P((SCM vars, SCM op, const char *what));
#ifdef CAREFUL_INTS
static void debug_env_warn P((char *fnam, int line, const char *what));
static void debug_env_save P((char *fnam, int line));
#endif

/* Flush global variable state to estk. */
#ifdef CAREFUL_INTS
# define ENV_SAVE debug_env_save(__FILE__, __LINE__)
#else
# define ENV_SAVE {scm_estk_ptr[0]=scm_env; scm_estk_ptr[1]=scm_env_tmp;}
#endif

/* Make global variable state consistent with estk. */
#define ENV_RESTORE {scm_env=scm_estk_ptr[0]; scm_env_tmp=scm_estk_ptr[1];}

#define ENV_PUSH \
  {DEFER_INTS_EGC; ENV_SAVE;\
   if (UNDEFINED==scm_estk_ptr[SCM_ESTK_FRLEN]) scm_estk_grow();\
   else scm_estk_ptr += SCM_ESTK_FRLEN;\
   STATIC_ENV=scm_estk_ptr[2 - SCM_ESTK_FRLEN];}

#define ENV_POP {DEFER_INTS_EGC;\
                 if (UNDEFINED==scm_estk_ptr[-1]) scm_estk_shrink();\
                 else scm_estk_ptr -= SCM_ESTK_FRLEN; ENV_RESTORE;}

#ifdef NO_ENV_CACHE
# define EGC_ROOT(x) /**/
#else
# ifdef CAREFUL_INTS
#  define EGC_ROOT(x) {if (!ints_disabled) \
                          debug_env_warn(__FILE__, __LINE__, "EGC_ROOT"); \
                       scm_egc_roots[--scm_egc_root_index] = (x); \
                       if (0==scm_egc_root_index) scm_egc();}
# else
#  define EGC_ROOT(x) {scm_egc_roots[--scm_egc_root_index] = (x);\
                       if (0==scm_egc_root_index) scm_egc();}
# endif
#endif

#ifndef RECKLESS
SCM scm_trace, scm_trace_env;
#endif
#define ENV_MAY_POP(p, guard) if (p>0 && !(guard)) {ENV_POP; p=-1;}
#define ENV_MAY_PUSH(p) if (p<=0) {ENV_PUSH; p=1;}
#define SIDEVAL_1(x) if (NIMP(x)) ceval_1(x)
#define STATIC_ENV (scm_estk_ptr[2])
#ifdef CAUTIOUS
# define TRACE(x) {scm_estk_ptr[3]=(x);}
# define TOP_TRACE(x, env) {scm_trace=(x); scm_trace_env=(env);}
#else
# define TRACE(x) /**/
# define TOP_TRACE(x, env) /**/
#endif
#ifndef RECKLESS
# define MACROEXP_TRACE(x, env) {scm_trace=(x); scm_trace_env=(env);}
#else
# define MACROEXP_TRACE(x, env) /**/
#endif

long tc16_macro;		/* Type code for macros */
#define MACROP(x) (tc16_macro==TYP16(x))
#define MAC_TYPE	NUMDIGS
#define MAC_PRIMITIVE	0x1L
#define MAC_MEMOIZING	0x2L
#define MAC_ACRO	0x4L
#define MAC_MACRO	0x8L
#define MAC_MMACRO	0x2L
#define MAC_IDMACRO	0x6L
/*  Uncomment this to experiment with inline procedures: */
/* #define MAC_INLINE	0x10L */

#ifdef MACRO
long tc16_ident;		/* synthetic macro identifier */
static char s_escaped[] = "escaped synthetic identifier";
# define KEYWORDP(x) (NIMP(x) && IM_KEYWORD==CAR(x))
# define KEYWORD_MACRO CDR
#else
# define KEYWORDP(x) (NIMP(x) && MACROP(x))
# define KEYWORD_MACRO(x) (x)
#endif

/* #define SCM_PROFILE */
#ifdef SCM_PROFILE
long eval_cases[128];
long eval_cases_other[NUM_ISYMS];
long ilookup_cases[10][10][2];	/* frame, dist, icdrp */
long eval_clo_cases[5][4];	/* actual args, required args */
SCM scm_profile(resetp)
     SCM resetp;
{
  SCM ev = make_uve(sizeof(eval_cases)/sizeof(long),
		    MAKINUM(-8L*sizeof(long)));
  SCM evo = make_uve(sizeof(eval_cases_other)/sizeof(long),
		     MAKINUM(-8L*sizeof(long)));
  SCM il = dims2ura(cons2(MAKINUM(10), MAKINUM(10), cons(MAKINUM(2), EOL)),
		    MAKINUM(-8L*sizeof(long)),
		    EOL);
  SCM evc = dims2ura(cons2(MAKINUM(5), MAKINUM(4), EOL),
		     MAKINUM(-8L*sizeof(long)),
		     EOL);
  long *v = (long *)VELTS(ev);
  int i;
  for (i = 0; i < sizeof(eval_cases)/sizeof(long); i++)
    v[i] = eval_cases[i];
  v = (long *)VELTS(evo);
  for (i = 0; i < sizeof(eval_cases_other)/sizeof(long); i++)
    v[i] = eval_cases_other[i];
  v = (long *)VELTS(ARRAY_V(il));
  for (i = 0; i < sizeof(ilookup_cases)/sizeof(long); i++)
    v[i] = ((long *)ilookup_cases)[i];
  v = (long *)VELTS(ARRAY_V(evc));
  for (i = 0; i < sizeof(eval_clo_cases)/sizeof(long); i++)
    v[i] = ((long *)eval_clo_cases)[i];
  if (! UNBNDP(resetp)) {
    for (i = 0; i < sizeof(eval_cases)/sizeof(long); i++)
      eval_cases[i] = 0;
    for (i = 0; i < sizeof(eval_cases_other)/sizeof(long); i++)
      eval_cases_other[i] = 0;
    for (i = 0; i < sizeof(ilookup_cases)/sizeof(long); i++)
      ((long *)ilookup_cases)[i] = 0;
    for (i = 0; i < sizeof(eval_clo_cases)/sizeof(long); i++)
      ((long *)eval_clo_cases)[i] = 0;
  }
  return cons2(ev, evo, cons2(il, evc, EOL));
}
#endif

#ifdef CAREFUL_INTS
# undef CAR
# define CAR(x) (*debug_env_car((x), __FILE__, __LINE__))
# undef CDR
# define CDR(x) (*debug_env_cdr((x), __FILE__, __LINE__))
/* Inhibit warnings for ARGC, is not changed by egc. */
# undef ARGC
# define ARGC(x) ((6L & (((cell *)(SCM2PTR(x)))->cdr))>>1)
# include <signal.h>
SCM test_ints(x)
     SCM x;
{
  static int cnt = 100;
  if (0==--cnt) {
    cnt = 100;
    DEFER_INTS;
    scm_egc();
    ALLOW_INTS;
    /*    l_raise(MAKINUM(SIGALRM)); */
  }
  return x;
}
int ecache_p(x)
     SCM x;
{
  register CELLPTR ptr;
  if (NCELLP(x)) return 0;
  ptr = (CELLPTR)SCM2PTR(x);
  if (PTR_LE(scm_ecache, ptr)
      && PTR_GT(scm_ecache+scm_ecache_len, ptr))
    return !0;
  return 0;
}
static void debug_env_warn(fnam, line, what)
     char *fnam;
     int line;
     const char *what;
{
  lputs(fnam, cur_errp);
  lputc(':', cur_errp);
  scm_intprint(line+0L, 10, cur_errp);
  lputs(": unprotected ", cur_errp);
  lputs(what, cur_errp);
  lputs(" of ecache value\n", cur_errp);
}
SCM *debug_env_car(x, fnam, line)
     SCM x;
     char *fnam;
     int line;
{
  SCM *ret;
  if (!ints_disabled && ecache_p(x))
    debug_env_warn(fnam, line, "CAR");
  ret = &(((cell *)(SCM2PTR(x)))->car);
  if (!ints_disabled && NIMP(*ret) && ecache_p(*ret))
    debug_env_warn(fnam, line, "CAR");
  return ret;
}
SCM *debug_env_cdr(x, fnam, line)
     SCM x;
     char *fnam;
     int line;
{
  SCM *ret;
  if (!ints_disabled && ecache_p(x))
    debug_env_warn(fnam, line, "CDR");
  ret = &(((cell *)(SCM2PTR(x)))->cdr);
  if (!ints_disabled && NIMP(*ret) && ecache_p(*ret))
    debug_env_warn(fnam, line, "CAR");
  return ret;
}
static void debug_env_save(fnam, line)
     char *fnam;
     int line;
{
  if (NIMP(scm_env) && (!scm_cell_p(scm_env)))
    debug_env_warn(fnam, line, "ENV_SAVE (env)");
  if (NIMP(scm_env_tmp) && (!scm_cell_p(scm_env_tmp)))
    debug_env_warn(fnam, line, "ENV_SAVE (tmp)");
  scm_estk_ptr[0]=scm_env;
  scm_estk_ptr[1]=scm_env_tmp;
}

#endif /* CAREFUL_INTS */

SCM *ilookup(iloc)
     SCM iloc;
{
  register int ir = IFRAME(iloc);
  register SCM er, *eloc;
#ifdef SCM_PROFILE
  ilookup_cases[ir<10 ? ir : 9]
    [IDIST(iloc)<10 ? IDIST(iloc) : 9][ICDRP(iloc)?1:0]++;
#endif
  DEFER_INTS_EGC;
  er = scm_env;
  /* shortcut the two most common cases. */
  if (iloc==MAKILOC(0, 0)) return &CAR(CAR(er));
  if (iloc==MAKILOC(0, 1)) return &CAR(CDR(CAR(er)));
  for (;0 != ir;--ir) er = CDR(er);
  eloc = &CAR(er);
  for (ir = IDIST(iloc); 0 != ir; --ir)
    eloc = &CDR(*eloc);
  if (ICDRP(iloc)) return eloc;
  return &CAR(*eloc);
}
SCM *farlookup(farloc)
     SCM farloc;
{
  register int ir;
  register SCM er;
  SCM x = CDR(farloc);
  DEFER_INTS_EGC;
  er = scm_env;
  for (ir = INUM(CAR(x)); 0 != ir; --ir) er = CDR(er);
  if (0==(ir = INUM(CDR(x)))) {
    if (IM_FARLOC_CDR==CAR(farloc)) return &CAR(er);
    return &CAR(CAR(er));
  }
  er = CAR(er);
  for (--ir;0 != ir;--ir) er = CDR(er);
  if (IM_FARLOC_CDR==CAR(farloc)) return &CDR(er);
  return &CAR(CDR(er));
}

char s_badenv[] = "damaged environment";
static char  s_lookup[] = "scm_env_lookup",
  s_badkey[] = "Use of keyword as variable",
  s_unbnd[] = "unbound variable: ",
  s_wtap[] = "Wrong type to apply: ",
  s_placement[] = "bad placement";

/*
  Returns:
    a symbol if VAR is not found in STENV,
    an ILOC if VAR is bound in STENV,
    a list (IM_FARLOC iframe idist) if VAR is bound very deeply in STENV,
    a pair (IM_KEYWORD . <macro>) if VAR is a syntax keyword bound in STENV.
*/
SCM scm_env_lookup(var, stenv)
     SCM var, stenv;
{
  SCM  frame, env = stenv;
  long icdr = 0L;
  unsigned int idist, iframe = 0;
#ifdef MACRO
  SCM mark = IDENT_ENV(var);
  if (NIMP(mark)) mark = CAR(mark);
#endif
  for (; NIMP(env); env = CDR(env)) {
    idist = 0;
    frame = CAR(env);
#ifdef MACRO
    if (frame==mark) {
      var = IDENT_PARENT(var);
      mark = IDENT_ENV(var);
      if (NIMP(mark)) mark = CAR(mark);
    }
#endif
    if (IMP(frame)) {
      if (NULLP(frame)) iframe++;
      else if (INUMP(frame)) {
#ifndef RECKLESS
	if (!(NIMP(env) && CONSP(env))) {
	badenv: wta(stenv, s_badenv, s_lookup);
	}
#endif
	env = CDR(env);
      }
      else {
	ASRTGO(SCM_LINUMP(frame), badenv);
      }
      continue;
    }
#ifdef MACRO
    if (NIMP(frame) && CONSP(frame) && SCM_ENV_SYNTAX==CAR(frame)) {
      /* syntax binding */
      SCM s = assq(var, CDR(frame));
      if (NIMP(s)) return cons(IM_KEYWORD, CDR(s));
      continue;
    }
#endif
    for (; NIMP(frame); frame = CDR(frame)) {
      if (NCONSP(frame)) {
	if (var==frame) {
	  icdr = ICDR;
	  goto local_out;
	}
	break;
      }
      if (CAR(frame)==var) {
      local_out:
#ifndef TEST_FARLOC
	var = MAKILOC(iframe, idist) + icdr;
	if (iframe==IFRAME(var) && idist==IDIST(var))
	  return var;
	else
#endif
	  return cons2(icdr ? IM_FARLOC_CDR : IM_FARLOC_CAR,
		       MAKINUM(iframe), MAKINUM(idist));
      }
      ASRTGO(CONSP(frame), badenv);
      idist++;
    }
    iframe++;
  }
  ASRTGO(NULLP(env), badenv);
#ifdef MACRO
  while (M_IDENTP(var)) {
    if (IMP(IDENT_ENV(var)))
      var = IDENT_PARENT(var);
    else break;
  }
#endif
  return var;
}

/* Throws error for macro keywords and undefined variables, always memoizes. */
static SCM *lookupcar(vloc)
     SCM vloc;
{
  SCM *pv, val, var = CAR(vloc), env = STATIC_ENV;
  SCM addr = scm_env_lookup(var, env);
  if (IMP(addr) || ISYMP(CAR(addr))) { /* local ref */
    DEFER_INTS_EGC;
    pv = IMP(addr) ? ilookup(addr) : farlookup(addr);
  }
#ifdef MACRO
# ifndef RECKLESS
  else if (NIMP(addr) && IM_KEYWORD==CAR(addr)) { /* local macro binding */
  badkey: wta(var, s_badkey, "");
  }
# endif
#endif
  else {			/* global ref */
#ifdef MACRO
    ASRTER(SYMBOLP(addr), var, s_escaped, "");
#endif
    val = sym2vcell(addr);
    addr = val + tc3_cons_gloc;
    pv = &CDR(val);
#ifdef MACRO
    ASRTGO(!KEYWORDP(*pv), badkey);
#endif
  }
  ASRTER(!UNBNDP(*pv) && undefineds != *pv, var, s_unbnd, "");
  CAR(vloc) = addr;
  return pv;
}

/* Throws error for undefined variables, memoizes if memo is non-zero.
   For local macros, conses new result. */
static SCM scm_lookupval(vloc, memo)
     SCM vloc;
     int memo;
{
  SCM val, env = STATIC_ENV, var = CAR(vloc);
  SCM addr = scm_env_lookup(var, env);
  if (IMP(addr)) { /* local ref */
    DEFER_INTS_EGC;
    val = *ilookup(addr);
  }
#ifdef MACRO
  else if (NIMP(addr) && IM_KEYWORD==CAR(addr)) /* local macro binding */
    val = addr;
#endif
  else if (ISYMP(CAR(addr))) {	/* local ref (farloc) */
    DEFER_INTS_EGC;
    val = *farlookup(addr);
  }
  else {			/* global ref */
#ifdef MACRO
    ASRTER(SYMBOLP(addr), var, s_escaped, "");
#endif
    addr = sym2vcell(addr);
    val = CDR(addr);
    addr += tc3_cons_gloc;
  }
  ASRTER(!UNBNDP(val) && val != undefineds, var, s_unbnd, "");
  if (memo && !KEYWORDP(val))	/* Don't memoize forms to be macroexpanded. */
    CAR(vloc) = addr;
  return val;
}

/* CAR(x) is known to be a cell but not a cons */
static SCM evalatomcar(x, no_error)
     SCM x;
     int no_error;
{
  SCM ret;
  switch TYP7(CAR(x)) {
  default:
    everr(x, STATIC_ENV, CAR(x), "Cannot evaluate: ", "", 0);
  lookup:
  case tcs_symbols:
    ret = scm_lookupval(x, !0);
    if (KEYWORDP(ret)) {
      SCM argv[3];
      SCM mac = KEYWORD_MACRO(ret);
      argv[0] = CAR(x);
      argv[1] = STATIC_ENV;
      argv[2] = EOL;
      switch (MAC_TYPE(mac) & ~MAC_PRIMITIVE) {
      default:
#ifdef MACRO
	if (!no_error)
	  everr(x, argv[1], argv[0], s_badkey, "", 0);
#endif
	return ret;
      case MAC_IDMACRO:
	ret = scm_cvapply(CDR(mac), 3L, argv);
	CAR(x) = ret;
	return evalcar(x);
      }
    }
    return ret;
  case tc7_vector:
#ifndef RECKLESS
    if (2 <= scm_verbose) scm_warn("unquoted ", s_vector, CAR(x));
#endif
    ret = cons2(IM_QUOTE, CAR(x), EOL);
    CAR(x) = ret;
    return CAR(CDR(ret));
  case tc7_smob:
#ifdef MACRO
    if (M_IDENTP(CAR(x))) goto lookup;
#endif
	/* fall through */
  case tcs_uves:
    return CAR(x);
  }
}

SCM scm_multi_set(syms, vals)
     SCM syms, vals;
{
  SCM res = EOL, *pres = &res;
  SCM *loc;
  do {
    ASRTER(NIMP(vals) && CONSP(vals), vals, WNA, s_set);
    switch (7 & (int)(CAR(syms))) {
    case 0:
      loc = lookupcar(syms);
      break;
    case 1:
      loc = &(I_VAL(CAR(syms)));
      break;
    case 4:
      loc = ilookup(CAR(syms));
      break;
    }
    *pres = cons(*loc, EOL);
    pres = &CDR(*pres);
    *loc = CAR(vals);
    syms = CDR(syms);
    vals = CDR(vals);
  } while (NIMP(syms));
  ASRTER(NULLP(vals) && NULLP(syms), vals, WNA, s_set);
  return res;
}

static SCM scm_case_selector(x)
     SCM x;
{
  SCM key, keys, *kv, *av;
  SCM actions, offset;
  long i, n;
  int op = ISYMVAL(CAR(x));
  x = CDR(x);
  key = EVALCAR(x);
  x = CDR(x);
  switch (op) {
  default: wta(MAKINUM(op), "internal error", s_case);
  case 0:			/* linear search */
    keys = CAR(x);
    kv = VELTS(keys);
    av = VELTS(CAR(CDR(x)));
    n = LENGTH(keys);
    for (i = n - 1; i > 0; i--)
      if (key == kv[i]) return av[i];
#ifndef INUMS_ONLY
    /* Bignum and flonum keys are pessimized. */
    if (NIMP(key) && NUMP(key))
      for (i = n - 1; i > 0; i--)
	if (NFALSEP(eqv(kv[i], key))) return av[i];
#endif
    return av[0];
  case 1:			/* integer jump table */
    offset = CAR(x);
    if (INUMP(key))
      i = INUM(key) - INUM(offset) + 1;
    else
      i = 0;
  jump:
    actions = CAR(CDR(x));
    if (i >= 1 && i < LENGTH(actions))
      return VELTS(actions)[i];
    else
      return VELTS(actions)[0];
  case 2:			/* character jump table */
    offset = CAR(x);
    if (ICHRP(key))
      i = ICHR(key) - ICHR(offset) + 1;
    else
      i = 0;
    goto jump;
  }
}

static SCM acro_call(x, env)
     SCM x, env;
{
  SCM proc, argv[3];
  x = CDR(x);
  proc = scm_lookupval(x, 0);
  ASRTGO(KEYWORDP(proc), errout);
  proc = KEYWORD_MACRO(proc);
  argv[0] = x;
  argv[1] = env;
  argv[2] = EOL;
  switch (MAC_TYPE(proc) & ~MAC_PRIMITIVE) {
  default:
  errout: wta(proc, CHARS(CAR(x)), "macro expected");
  case MAC_MACRO:
    x = scm_cvapply(CDR(proc), 3L, argv);
    if (ilength(x) <= 0)
      x = cons2(IM_BEGIN, x, EOL);
    return x;
  case MAC_ACRO:
    x = scm_cvapply(CDR(proc), 3L, argv);
    return cons2(IM_QUOTE, x, EOL);
  }
}

static SCM toplevel_define(xorig, env)
     SCM xorig, env;
{
  SCM x = CDR(xorig);
  SCM name = CAR(x);
  ASRTER(scm_nullenv_p(env), xorig, s_placement, s_define);
  ENV_PUSH;
  scm_env_tmp = EOL;		/* Make sure multiple values -> error */
  x = cons(m_binding(name, CAR(CDR(x)), env, EOL), EOL);
  x = evalcar(x);
  ENV_POP;
  checked_define(name, x, s_define);
#ifdef SICP
  return name;
#else
  return UNSPECIFIED;
#endif
}

SCM eval_args(l)
     SCM l;
{
	SCM res = EOL, *lloc = &res;
	while NIMP(l) {
	  *lloc = cons(EVALCAR(l), EOL);
	  lloc = &CDR(*lloc);
	  l = CDR(l);
	}
	return res;
}

/*
  Evaluate each expression in argument list x,
  and return a list allocated in the ecache of the
  results.
  The result is left in scm_env_tmp.
*/
static void ecache_evalx(x)
     SCM x;
{
  SCM argv[10];
  int i = 0, imax = sizeof(argv)/sizeof(SCM);
  scm_env_tmp = EOL;
  while NIMP(x) {
    if (imax==i) {
      ecache_evalx(x);
      break;
    }
    argv[i++] = EVALCAR(x);
    x = CDR(x);
  }
  ENV_V2LST((long)i, argv);
}

/*
  Allocate a list of UNDEFINED in the ecache, one
  for each element of the argument list x.
  The result is left in scm_env_tmp.
*/
static void ecache_undefs(x)
     SCM x;
{
  static SCM argv[10] = {UNDEFINED, UNDEFINED, UNDEFINED,
                         UNDEFINED, UNDEFINED, UNDEFINED,
                         UNDEFINED, UNDEFINED, UNDEFINED,
                         UNDEFINED};

  int imax = sizeof(argv)/sizeof(SCM);
  int i = 0;

  scm_env_tmp = EOL;
  while NIMP(x) {
    if (imax==i) {
      ecache_undefs(x);
      break;
    }
    i++;
    x = CDR(x);
  }
  ENV_V2LST((long)i, argv);
}

/* result is 1 if right number of arguments, 0 otherwise,
   environment frame is put in scm_env_tmp */
static int ecache_eval_args(proc, arg1, arg2, arg3, x)
     SCM proc, arg1, arg2, arg3, x;
{
  SCM argv[3];
  argv[0] = arg1;
  argv[1] = arg2;
  argv[2] = arg3;
  if (NIMP(x))
    ecache_evalx(x);
  else
    scm_env_tmp = EOL;
  ENV_V2LST(3L, argv);
#ifndef RECKLESS
  proc = SCM_ENV_FORMALS(CAR(CODE(proc)));
  proc = CDR(proc);
  proc = CDR(proc);
  proc = CDR(proc);
  for (; NIMP(proc); proc=CDR(proc)) {
    if (IMP(x)) return 0;
    x = CDR(x);
  }
  if (NIMP(x)) return 0;
#endif
  return 1;
}

static SCM asubr_apply(proc, arg1, arg2, arg3, args)
     SCM proc, arg1, arg2, arg3, args;
{
  switch TYP7(proc) {
  case tc7_asubr:
    arg1 = SUBRF(proc)(arg1, arg2);
    arg1 = SUBRF(proc)(arg1, arg3);
    while NIMP(args) {
      arg1 = SUBRF(proc)(arg1, CAR(args));
      args = CDR(args);
    }
    return arg1;
  case tc7_rpsubr:
    if (FALSEP(SUBRF(proc)(arg1, arg2))) return BOOL_F;
    while (!0) {
      if (FALSEP(SUBRF(proc)(arg2, arg3))) return BOOL_F;
      if (IMP(args)) return BOOL_T;
      arg2 = arg3;
      arg3 = CAR(args);
      args = CDR(args);
    }
  default: return UNDEFINED;
  }
}

static char s_values[] = "values";
static char s_call_wv[] = "call-with-values";
SCM scm_values(arg1, arg2, rest, what)
     SCM arg1, arg2, rest;
     const char *what;
{
  DEFER_INTS_EGC;
  ASRTER(IM_VALUES_TOKEN==scm_env_tmp, UNDEFINED, "one value expected", what);
  if (! UNBNDP(arg2))
    scm_env_cons(arg2, rest);
  return arg1;
}

    /* the following rewrite expressions and
     * some memoized forms have different syntax */

static char s_expression[] = "missing or extra expression";
static char s_test[] = "bad test";
static char s_body[] = "bad body";
static char s_bindings[] = "bad bindings";
static char s_variable[] = "bad variable";
static char s_bad_else_clause[] = "bad ELSE clause";
static char s_clauses[] = "bad or missing clauses";
static char s_formals[] = "bad formals";
static char s_expr[] = "bad expression";
#define ASSYNT(_cond, _arg, _pos, _subr)\
   if (!(_cond))scm_experr(_arg, (char *)_pos, _subr);

/* These symbols are needed by the reader, in repl.c */
SCM i_dot, i_quote, i_quasiquote, i_unquote, i_uq_splicing;
static SCM i_lambda, i_define, i_let, i_begin, i_arrow, i_else;	/* , i_atbind */
/* These symbols are passed in the context argument to macro expanders. */
static SCM i_bind, i_anon, i_side_effect, i_test, i_procedure,
  i_argument, i_check_defines;

static SCM f_begin, f_define;

#define ASRTSYNTAX(cond_, msg_) if (!(cond_))wta(xorig, (msg_), what);
#ifdef MACRO
# define TOPLEVELP(x, env) (topdenote_eq(UNDEFINED, (x), (env)))
# define TOPDENOTE_EQ topdenote_eq
# define TOPRENAME(v) (renamed_ident(v, BOOL_F))

static int topdenote_eq(sym, id, env)
     SCM sym, id, env;
{
  if (UNBNDP(sym)) {
    sym = scm_env_lookup(id, env);
    return NIMP(sym) && SYMBOLP(sym);
  }
  return sym==id2sym(id) && sym==scm_env_lookup(id, env);
}

static SCM id2sym(id)
     SCM id;
{
  if (NIMP(id))
    while M_IDENTP(id)
      id = IDENT_PARENT(id);
  return id;
}

#else /* def MACRO */
# define TOPDENOTE_EQ(sym, x, env) ((sym)==(x))
# define TOPLEVELP(x, env) (!0)
# define TOPRENAME(v) (v)
#endif

static void unpaint(p)
     SCM *p;
{
  SCM x;
  while NIMP((x = *p)) {
    if (CONSP(x)) {
      if (NIMP(CAR(x)))
	unpaint(&CAR(x));
      else if (SCM_LINUMP(CAR(x))) {
	*p = CDR(x);
	continue;
      }
      p = &CDR(*p);
    }
    else if (VECTORP(x)) {
      sizet i = LENGTH(x);
      if (0==i) return;
      while (i-- > 1) unpaint(&(VELTS(x)[i]));
      p = VELTS(x);
    }
    else {
#ifdef MACRO
      while M_IDENTP(x) *p = x = IDENT_PARENT(x);
#endif
      return;
    }
  }
}

SCM m_quote(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = copytree(CDR(xorig));
  ASSYNT(ilength(CDR(xorig))==1, xorig, s_expression, s_quote);
  DEFER_INTS;
  unpaint(&CAR(x));
  ALLOW_INTS;
  return cons(IM_QUOTE, x);
}

SCM m_begin(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  int len = ilength(CDR(xorig));
  if (0==len) return cons2(IM_BEGIN, UNSPECIFIED, EOL);
  if (1==len) return CAR(CDR(xorig));
  ASSYNT(len >= 1, xorig, s_expression, s_begin);
  return cons(IM_BEGIN, CDR(xorig));
}

static int constant_p(x)
     SCM x;
{
  return IMP(x) ? !0 : (CONSP(x) ? 0 : !IDENTP(x));
}

SCM m_if(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM test, x = CDR(xorig);
  int len = ilength(x);
  ASSYNT(len >= 2 && len <= 3, xorig, s_expression, s_if);
  test = CAR(x);
  x = CDR(x);
  if (FALSEP(test))
    return 3==len ? CAR(CDR(x)) : UNSPECIFIED;
  if (constant_p(test))
    return CAR(x);
  return cons2(IM_IF, m_expr(test, env, i_test),
	       cons(m_expr(CAR(x), env, ctxt),
		    NULLP(CDR(x)) ? EOL :
		    cons(m_expr(CAR(CDR(x)), env, ctxt), EOL)));
}

SCM m_set(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM var, x = CDR(xorig);
  ASSYNT(2==ilength(x), xorig, s_expression, s_set);
  varcheck((NIMP(CAR(x)) && IDENTP(CAR(x))) ? CAR(x) :
	   (ilength(CAR(x)) > 0) ? CAR(x) : UNDEFINED,
	    IM_SET, s_variable);
  var = CAR(x);
  x = CDR(x);
  return cons(IM_SET, cons2(var, m_expr(CAR(x), env, ctxt), EOL));
}

SCM m_and(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = CDR(xorig);
  int len = ilength(x);
  ASSYNT(len >= 0, xorig, s_test, s_and);
 tail:
  switch (len) {
  default:
    if (FALSEP(CAR(x))) return BOOL_F;
    if (constant_p(CAR(x))) {
      x = CDR(x);
      len--;
      goto tail;
    }
    return cons(IM_AND, x);
  case 1: return CAR(x);
  case 0: return BOOL_T;
  }
}

SCM m_or(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = CDR(xorig);
  int len = ilength(x);
  ASSYNT(len >= 0, xorig, s_test, s_or);
 tail:
  switch (len) {
  default:
    if (FALSEP(CAR(x))) {
      x = CDR(x);
      len--;
      goto tail;
    }
    if (constant_p(CAR(x)))
      return CAR(x);
    return cons(IM_OR, x);
  case 1: return CAR(x);
  case 0: return BOOL_F;
  }
}

#ifdef INUMS_ONLY
# define memv memq
#endif
static SCM *loc_atcase_aux = 0;
static int in_atcase_aux = 0;
SCM m_case(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM clause, key_expr, x = CDR(xorig);
  SCM s, keys = EOL, action, actions = EOL, else_action = list_unspecified;
  int opt = !scm_nullenv_p(env);

  ASSYNT(ilength(x) >= 2, xorig, s_clauses, s_case);
  key_expr = CAR(x);
  while(NIMP(x = CDR(x))) {
    clause = CAR(x);
    s = scm_check_linum(clause, 0L);
    ASSYNT(ilength(s) >= 2, clause /* xorig */, s_clauses, s_case);
    clause = s;
    if (TOPDENOTE_EQ(i_else, CAR(clause), env)) {
      ASSYNT(NULLP(CDR(x)), xorig, s_bad_else_clause, s_case);
      else_action = m_seq(CDR(clause), env, ctxt);
    }
    else {
      s = scm_check_linum(CAR(clause), 0L);
#ifdef MACRO
      s = scm_cp_list(s, 0);
      ASSYNT(!UNBNDP(s), CAR(clause) /* xorig */, s_clauses, s_case);
      DEFER_INTS;
      unpaint(&s);
      ALLOW_INTS;
#else
      ASSYNT(ilength(s) >= 0, CAR(clause) /* xorig */, s_clauses, s_case);
#endif
      action = m_seq(CDR(clause), env, ctxt);
      for (; NIMP(s); s = CDR(s)) {
	ASSYNT(FALSEP(memv(CAR(s), keys)), xorig, "duplicate key value", s_case);
	if (NIMP(CAR(s)) && NUMP(CAR(s))) opt = 0;
	keys = cons(CAR(s), keys);
	actions = cons(action, actions);
      }
    }
  }
  key_expr = m_expr(key_expr, env, i_test);
  if (opt && NIMP(*loc_atcase_aux) && !in_atcase_aux) {
    SCM argv[3];
    argv[0] = keys;
    argv[1] = actions;
    argv[2] = else_action;
    in_atcase_aux = !0;
    x = scm_cvapply(*loc_atcase_aux, 3L, argv);
    in_atcase_aux = 0;		/* disabled after one error.  C'est la vie. */
    if (NIMP(x) && CONSP(x)) {
      s = CAR(x);
      if (INUMP(s) && INUM(s) >= 0 && INUM(s) <= 2)
	return cons2(MAKISYMVAL(IM_CASE, INUM(s)), key_expr, CDR(x));
    }
  }
  keys = cons(UNSPECIFIED, keys);
  actions = cons(else_action, actions);
  return cons2(IM_CASE, key_expr,
	       cons2(vector(keys), vector(actions), EOL));
}

SCM m_cond(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM s, clause, cdrx = scm_cp_list(CDR(xorig), 1), x = cdrx;
  int len = ilength(x);
  ASSYNT(!UNBNDP(cdrx), xorig, s_clauses, s_cond);
  while(NIMP(x)) {
    clause = scm_check_linum(CAR(x), 0L);
    len = ilength(clause);
    ASSYNT(len >= 1, CAR(x), s_clauses, s_cond);
    if (TOPDENOTE_EQ(i_else, CAR(clause), env)) {
      ASSYNT(NULLP(CDR(x)) && len >= 2, xorig, s_bad_else_clause, s_cond);
      clause = cons(BOOL_T, m_seq(CDR(clause), env, ctxt));
    }
    else {
      s = CDR(clause);
      if (len >= 2 && TOPDENOTE_EQ(i_arrow, CAR(s), env)) {
	ASSYNT(3==len && NIMP(CAR(CDR(s))), clause, "bad recipient", s_cond);
	clause = cons2(CAR(clause), IM_ARROW, CDR(s));
      }
      else
	clause = cons(CAR(clause), m_seq(s, env, ctxt));
    }
    CAR(x) = clause;
    x = CDR(x);
  }
  return cons(IM_COND, cdrx);
}

static int varcheck(vars, op, what)
     SCM vars, op;
     const char *what;
{
  SCM v1, vs;
  char *opstr = ISYMCHARS(op) + 2;
  int argc = 0;
  vars = scm_check_linum(vars, 0L);
  for (; NIMP(vars) && CONSP(vars); vars = CDR(vars)) {
    argc++;
#ifndef RECKLESS
    v1 = CAR(vars);
    if (IMP(v1) || !IDENTP(v1))
      badvar: scm_experr(v1, what, opstr);
    for (vs = CDR(vars); NIMP(vs) && CONSP(vs); vs = CDR(vs)) {
      if (v1==CAR(vs)) {
	nonuniq:
	what = "non-unique bindings";
	goto badvar;
      }
    }
    if (v1==vs) goto nonuniq;
#endif
  }
		/* argc of 3 means no rest argument, 3+ required arguments */
  if (NULLP(vars) || ISYMP(vars)) return argc > 3 ? 3 : argc;
  ASRTGO(NIMP(vars) && IDENTP(vars), badvar);
  return argc > 2 ? 2 : argc;
}

SCM m_lambda(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = CDR(xorig), formals;
#ifdef CAUTIOUS
  SCM name, linum;
#endif
  int argc;
  ASRTER(ilength(x) > 1, x, s_body, s_lambda);
  formals = CAR(x);
  argc = varcheck(formals, IM_LAMBDA, s_formals);
  formals = scm_check_linum(formals, 0L);
  if (argc > 3) argc = 3;
  x = CDR(x);
  if (NIMP(CDR(x)) && NIMP(CAR(x)) && STRINGP(CAR(x))) {
    env = scm_env_addprop(SCM_ENV_DOC, CAR(x), env);
    x = CDR(x);
  }
#ifdef CAUTIOUS
  if (NIMP(ctxt) && i_bind==CAR(ctxt)) {
    ctxt = CDR(ctxt);
    name = CAR(ctxt);
  }
  else
    name = i_anon;
  if (NIMP(scm_trace) && xorig==scm_check_linum(scm_trace, &linum))
    if (!UNBNDP(linum)) env = EXTEND_ENV(linum, env);
  env = scm_env_addprop(SCM_ENV_PROCNAME, name, env);
#endif
  env = EXTEND_ENV(formals, env);
  return cons2(MAKISYMVAL(IM_LAMBDA, argc), env, m_body(x, env, EOL));
}

#ifdef MAC_INLINE
static int env_depth()
{
  register int depth = 0;
  register SCM env;
  DEFER_INTS_EGC;
  env = scm_env;
  while(NIMP(env)) {
    env = CDR(env);
    depth++;
  }
  return depth;
}
static void env_tail(depth)
     int depth;
{
  register SCM env;
  DEFER_INTS_EGC;
  env = scm_env;
  while(depth--) env = CDR(env);
  scm_env = env;
}
/* FIXME update for split-env */
SCM m_inline_lambda(xorig, env)
     SCM xorig, env;
{
  SCM x = CDR(xorig);
  SCM typ = (SCM)(tc16_macro | (MAC_INLINE << 16));
  int depth = env_depth();
  ASRTER(ilength(x) > 1, xorig, s_formals, s_lambda);
  ASRTER(ilength(CAR(x)) >= 0, xorig, s_formals, s_lambda);
  varcheck(CAR(x), IM_LAMBDA, s_formals);
  x = cons2(typ, MAKINUM((long)depth),
	    cons(CAR(x), m_body(CDR(x), env)));
  return cons2(IM_QUOTE, x, EOL);
}
#endif

static char s_nullenv_p[] = "scm_nullenv_p";
int scm_nullenv_p(env)
     SCM env;
{
  SCM fr, e;
  if (IMP(env)) return !0;
  for (e = env; NIMP(e); e = CDR(e)) {
    ASRTER(CONSP(e), e, s_badenv, s_nullenv_p);
    fr = CAR(e);
    if (IMP(fr)) {
      if (NULLP(fr)) return 0;
      if (INUMP(fr)) {      /* These frames are for meta-data, not bindings. */
	e = CDR(e);
	ASRTER(NIMP(e), env, s_badenv, s_nullenv_p);
      }
    } else return 0;
  }
  return !0;
}
static SCM m_letstar1(imm, vars, inits, body, env, ctxt)
     SCM imm, vars, inits, body, env, ctxt;
{
  SCM init, bdgs = cons(env, EOL); /* initial env is for debug printing. */
  SCM *loc = &CDR(bdgs);
  while (NIMP(vars)) {
    init = m_binding(CAR(vars), CAR(inits), env, ctxt);
    env = EXTEND_ENV(CAR(vars), env);
    *loc = cons2(init, env, EOL);
    loc = &CDR(CDR(*loc));
    vars = CDR(vars);
    inits = CDR(inits);
  }
  return cons2(IM_LETSTAR, bdgs, m_body(body, env, ctxt));
}

SCM m_letstar(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM vars, inits;
  SCM body = m_parse_let(EOL, xorig, CDR(xorig), &vars, &inits);
  /* IM_LETSTAR must bind at least one variable. */
  if (IMP(vars))
    return m_let_null(body, env, ctxt);
  return m_letstar1(IM_LETSTAR, vars, inits, body, env, ctxt);
}

/* DO gets the most radically altered syntax
   (do ((<var1> <init1> <step1>)
   (<var2> <init2>)
   ... )
   (<test> <return>)
   <body>)
   ;; becomes
   (do_mem (varn ... var2 var1)
   (<initn> ... <init2> <init1>)
   (<test> <return>)
   (<body>)
   <stepn> ... <step2> <step1>) ;; missing steps replaced by var
   */
SCM m_do(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = CDR(xorig), bdg, bdgs, test, body;
  SCM vars = IM_DO, inits = EOL, steps = EOL;
  int len = ilength(x);
  ASSYNT(len >= 2, xorig, s_test, s_do);
  bdgs = scm_check_linum(CAR(x), 0L);
  ASSYNT(ilength(bdgs) >= 0, CAR(x), s_bindings, s_do);
  while NIMP(bdgs) {
    bdg = scm_check_linum(CAR(bdgs), 0L);
    len = ilength(bdg);
    ASSYNT(2==len || 3==len, CAR(bdgs), s_bindings, s_do);
    vars = cons(CAR(bdg), vars); /* variable */
    bdg = CDR(bdg);
    inits = cons(CAR(bdg), inits);
    bdg = CDR(bdg);
    steps = cons(IMP(bdg) ? CAR(vars) : CAR(bdg), steps);
    bdgs = CDR(bdgs);
  }
  if (IMP(vars)) vars = EOL;
  inits = m_bindings(vars, inits, env, ctxt);
  env = EXTEND_ENV(vars, env);
  steps = m_bindings(vars, steps, env, ctxt);
  x = CDR(x);
  test = scm_check_linum(CAR(x), 0L);
  ASSYNT(ilength(test) >= 1, CAR(x), s_test, s_do);
  if (IMP(CDR(test))) test = cons(CAR(test), list_unspecified);
  ASSYNT(ilength(CDR(x))>=0, xorig, s_expression, s_do);
  varcheck(vars, IM_DO, s_variable);
  body = scm_check_linum(CDR(x), 0L);
  x = cons2(test, m_seq(body, env, i_side_effect), steps);
  x = cons2(env, inits, x);
  return cons(IM_DO, x);
}

/* evalcar is small version of inline EVALCAR when we don't care about speed */
static SCM evalcar(x)
     SCM x;
{
  return EVALCAR(x);
}

/* Here are acros which return values rather than code. */

static SCM iqq(form)
     SCM form;
{
  SCM tmp;
  if (IMP(form)) return form;
  if (VECTORP(form)) {
    long i = LENGTH(form);
    SCM *data = VELTS(form);
    tmp = EOL;
    for (;--i >= 0;) tmp = cons(data[i], tmp);
    return vector(iqq(tmp));
  }
  if (NCONSP(form)) return form;
  tmp = CAR(form);
  if (IM_UNQUOTE==tmp)
    return evalcar(CDR(form));
  if (NIMP(tmp) && IM_UQ_SPLICING==CAR(tmp))
    return append(cons2(evalcar(CDR(tmp)), iqq(CDR(form)), EOL));
  return cons(iqq(CAR(form)), iqq(CDR(form)));
}

static SCM m_iqq(form, depth, env, ctxt)
     SCM form, env, ctxt;
     int depth;
{
  SCM tmp;
  int edepth = depth;
  if (IMP(form)) return form;
  if (VECTORP(form)) {
    long i = LENGTH(form);
    SCM *data = VELTS(form);
    tmp = EOL;
    for (;--i >= 0;) tmp = cons(data[i], tmp);
    tmp = m_iqq(tmp, depth, env, ctxt);
    for (i = 0; i < LENGTH(form); i++) {
      data[i] = CAR(tmp);
      tmp = CDR(tmp);
    }
    return form;
  }
  if (NCONSP(form)) {
#ifdef MACRO
    while M_IDENTP(form) form = IDENT_PARENT(form);
#endif
    return form;
  }
 form = scm_check_linum(form, 0L); /* needed? */
  tmp = scm_check_linum(CAR(form), 0L);
  if (NIMP(tmp)) {
    if (IDENTP(tmp)) {
#ifdef MACRO
      while M_IDENTP(tmp) tmp = IDENT_PARENT(tmp);
#endif
      if (i_quasiquote==tmp && TOPLEVELP(CAR(form), env)) {
	depth++;
	if (0==depth) tmp = IM_QUASIQUOTE;
	goto label;
      }
      else if (i_unquote==tmp && TOPLEVELP(CAR(form), env)) {
	--depth;
	if (0==depth) tmp = IM_UNQUOTE;
      label:
	form = CDR(form);
	ASRTER(NIMP(form) && ECONSP(form) && NULLP(CDR(form)),
	       form, ARG1, s_quasiquote);
	if (0!=depth)
	  form = cons(m_iqq(CAR(form), depth, env, ctxt), EOL);
	return cons(tmp, form);
      }
    }
    else {
      if (TOPDENOTE_EQ(i_uq_splicing, CAR(tmp), env)) {
	if (0==--edepth)
	  return cons(cons(IM_UQ_SPLICING, CDR(tmp)),
		      m_iqq(CDR(form), depth, env, ctxt));
      }
      tmp = m_iqq(tmp, edepth, env, ctxt);
    }
  }
  return cons(tmp, m_iqq(CDR(form), depth, env, ctxt));
}
SCM m_quasiquote(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = CDR(xorig);
  ASSYNT(ilength(x)==1, xorig, s_expression, s_quasiquote);
  x = m_iqq(x, 1, env, ctxt);
  return cons(IM_QUASIQUOTE, x);
}

SCM m_delay(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  ASSYNT(ilength(xorig)==2, xorig, s_expression, s_delay);
  return cons2(IM_DELAY, EXTEND_ENV(EOL, env), CDR(xorig));
}

static int built_inp(name, x)
     SCM name, x;
{
  if (NIMP(x)) {
 tail:
    switch TYP7(x) {
    case tcs_subrs: return CHARS(name)==SNAME(x);
    case tc7_smob: if (MACROP(x)) {x = CDR(x); goto tail;}
		/* else fall through */
    }
  }
  return 0;
}

extern char s_redefining[];
#ifndef RECKLESS
char s_built_in_syntax[] = "built-in syntax ";
# define s_syntax (&s_built_in_syntax[9])
#endif
static void checked_define(name, val, what)
     SCM name, val;
     const char *what;
{
  SCM old, vcell;
#ifdef MACRO
  while (M_IDENTP(name)) {
    ASRTER(IMP(IDENT_ENV(name)), name, s_escaped, what);
    name = IDENT_PARENT(name);
  }
#endif
  vcell = sym2vcell(name);
  old = CDR(vcell);
#ifndef RECKLESS
    if ('@'==CHARS(name)[0] && UNDEFINED != old)
      scm_warn(s_redefining, "internal name ", name);
    if (KEYWORDP(old)) {
      if (1 <= scm_verbose && built_inp(name, KEYWORD_MACRO(old)))
	scm_warn(s_redefining, s_built_in_syntax, name);
      else if (3 <= scm_verbose)
	scm_warn(s_redefining, s_syntax, name);
    }
    else if (2 <= scm_verbose && built_inp(name, old) && (old != val))
      scm_warn(s_redefining, "built-in ", name);
    else if (5 <= scm_verbose && UNDEFINED != old)
      scm_warn(s_redefining, "", name);
#endif
    CDR(vcell) = val;
}

SCM m_define(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM name, linum, x = CDR(xorig);
  ASSYNT(ilength(x) >= 2, xorig, s_expression, s_define);
  name = CAR(x); x = CDR(x);
  while (NIMP(name) && CONSP(name)) { /* nested define syntax */
    name = scm_check_linum(name, &linum);
    x = scm_add_linum(linum, cons2(TOPRENAME(i_lambda), CDR(name), x));
    x = cons(x, EOL);
    name = CAR(name);
  }
  ASSYNT(NIMP(name) && IDENTP(name), xorig, s_variable, s_define);
  ASSYNT(1==ilength(x), xorig, s_expression, s_define);
  return cons2(IM_DEFINE, name, x);
}
/* end of acros */

/* returns body, x should be cdr of a LET, LET*, or LETREC form.
   vars and inits are returned in the original order. */
static SCM m_parse_let(imm, xorig, x, vars, inits)
     SCM imm, xorig, x, *vars, *inits;
{
  SCM clause, bdgs, *varloc = vars, *initloc = inits;
  int len = ilength(x);
#ifdef MACRO
  const char *what = CHARS(ident2sym(CAR(xorig)));
#else
  const char *what = CHARS(CAR(xorig));
#endif
  *varloc = imm;
  *initloc = EOL;
  ASSYNT(len >= 2, UNDEFINED, s_body, what);
  bdgs = scm_check_linum(CAR(x), 0L);
  ASSYNT(ilength(bdgs) >= 0, bdgs, s_bindings, what);
  while NIMP(bdgs) {
    clause = scm_check_linum(CAR(bdgs), 0L);
    ASSYNT(2==ilength(clause), clause, s_bindings, what);
    ASSYNT(NIMP(CAR(clause)) && IDENTP(CAR(clause)), CAR(clause),
	   s_variable, what);
    *varloc = cons(CAR(clause), imm);
    varloc = &CDR(*varloc);
    *initloc = cons(CAR(CDR(clause)), EOL);
    initloc = &CDR(*initloc);
    bdgs = CDR(bdgs);
  }
  x = CDR(x);
  ASSYNT(ilength(x)>0, scm_wrapcode(x, EOL) /* xorig */, s_body, what);
  if (IMP(*vars)) *vars = EOL;
  return x;
}

static SCM m_let_null(body, env, ctxt)
     SCM body, env, ctxt;
{
  SCM x;
  if (scm_nullenv_p(env)) {
    env = EXTEND_ENV(EOL, env);
    return cons2(IM_LET, env, cons(EOL, m_body(body, env, ctxt)));
  }
  x = m_body(body, env, ctxt);
  return NULLP(CDR(x)) ? CAR(x) : cons(IM_BEGIN, x);
}

static SCM m_letrec1(imm, xorig, env, ctxt)
     SCM imm, xorig, env, ctxt;
{
  SCM vars, inits, op = MAKSPCSYM2(IM_LETREC, imm);
  SCM body = m_parse_let(imm, xorig, CDR(xorig), &vars, &inits);
  if (IMP(vars)) return m_let_null(body, env, ctxt);
  varcheck(vars, imm, s_variable);
  env = EXTEND_ENV(vars, env);
  inits = m_bindings(vars, inits, env, ctxt);
  return cons2(op, env, cons(inits, m_body(body, env, ctxt)));
}

SCM m_letrec(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  return m_letrec1(IM_LETREC, xorig, env, ctxt);
}

SCM m_let(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM proc, body, vars, inits, x = CDR(xorig);
  ASSYNT(ilength(x) >= 2, xorig, s_body, s_let);
  proc = CAR(x);
  if (NIMP(proc) && IDENTP(proc)) { /* named let, build equiv letrec */
    x = CDR(x);
    body = m_parse_let(IM_LET, xorig, x, &vars, &inits);
    x = cons2(TOPRENAME(i_lambda), vars, body);
    x = cons2(i_let, cons(cons2(proc, x, EOL), EOL), cons(proc, EOL));
    return cons(m_letrec1(IM_LET, x, env, ctxt), inits);
  }
				/* vanilla let */
  body = m_parse_let(IM_LET, xorig, x, &vars, &inits);
  varcheck(vars, IM_LET, s_variable);
  if (IMP(vars))
    return m_let_null(body, env, ctxt);
  if (IMP(CDR(vars)))		/* single binding, let* is faster */
    return m_letstar1(IM_LET, vars, inits, body, env, ctxt);
  inits = m_bindings(vars, inits, env, ctxt);
  env = EXTEND_ENV(vars, env);
  return cons2(IM_LET, env, cons(inits, m_body(body, env, ctxt)));
}

#define s_atapply (ISYMCHARS(IM_APPLY)+1)

SCM m_apply(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  ASSYNT(ilength(CDR(xorig))==2, xorig, s_expression, s_atapply);
  return cons(IM_APPLY, CDR(xorig));
}

static SCM m_body(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM form, denv = env, x = xorig, defs = EOL;
  const char *what = 0; /* Should this be passed in? */
  ASRTSYNTAX(ilength(xorig) >= 1, s_expression);
  while NIMP(x) {
    form = scm_check_linum(CAR(x), 0L);
    if (IMP(form) || NCONSP(form)) break;
    if (IMP(CAR(form))) break;
    if (! IDENTP(CAR(form))) break;
    form = macroexp1(CAR(x), denv, i_check_defines, 1);
    if (IM_DEFINE==CAR(form)) {
      defs = cons(CDR(form), defs);
      x = CDR(x);
    }
    else if (IM_BEGIN==CAR(form)) {
      form = CDR(form);
      x = CDR(x);
      if (IMP(x))
	x = form;
      else if (UNSPECIFIED==CAR(form) && IMP(CDR(form)))
	;
      else
	x = append(cons2(form, x, EOL));
    }
    else if (NIMP(defs)) {
      break;
    }
    else {
      /* Doesn't work when m_body recursively called
	 x = cons(form, m_seq(CDR(x), env, ctxt)); */
      x = cons(form, CDR(x));
      break;
    }
  }
  ASSYNT(ilength(x) > 0, xorig, s_body, what);
  if (IMP(defs)) return x;
  return
    cons(m_letrec1(IM_DEFINE, cons2(i_define, reverse(defs), x), env, ctxt),
	 EOL);
}

static SCM m_binding(name, value, env, ctxt)
     SCM name, value, env, ctxt;
{
  if (IMP(value) || NCONSP(value)) return value;
  ctxt = cons2(i_bind, name, EOL);
  return macroexp1(value, env, ctxt, 2);
}
static SCM m_bindings(names, values, env, ctxt)
     SCM names, values, env, ctxt;
{
  SCM x;
  for (x = values; NIMP(x); x = CDR(x)) {
    CAR(x) = m_binding(CAR(names), CAR(x), env, ctxt);
    names = CDR(names);
  }
  return values;
}
static SCM m_seq(x, env, ctxt)
     SCM x, env, ctxt;
{
  SCM form, ret = EOL, *loc = &ret;
  for (; NIMP(x); x = CDR(x)) {
    form = CAR(x);
    if (NIMP(form) && CONSP(form)) {
      form = macroexp1(form, env, IMP(CDR(x)) ? ctxt : i_side_effect, 2);
      if (NIMP(form) && IM_BEGIN==CAR(form)) {
	x = append(cons2(form, CDR(x), EOL));
	continue;
      }
    }
    *loc = cons(form, EOL);
    loc = &CDR(*loc);
  }
  return ret;
}
static SCM m_expr(x, env, ctxt)
     SCM x, env, ctxt;
{
  if (NIMP(x) && CONSP(x)) {
    x = macroexp1(x, env, ctxt, 2);
    if (NIMP(x) && IM_BEGIN==CAR(x))
      x = cons(IM_BEGIN, m_seq(CDR(x), env, ctxt));
  }
  return x;
}

SCM scm_check_linum(x, linum)
     SCM x, *linum;
{
  SCM lin = UNDEFINED;
  if (NIMP(x) && CONSP(x) && SCM_LINUMP(CAR(x))) {
    lin = CAR(x);
    x = CDR(x);
  }
  if (linum) *linum = lin;
  return x;
}
SCM scm_add_linum(linum, x)
     SCM x, linum;
{
  if (UNBNDP(linum)) return x;
  if (NIMP(x) && CONSP(x) && SCM_LINUMP(CAR(x))) return x;
  return cons(linum, x);
}

/*
   mode values:
     0 expand non-primitive macros only
     1 check for defines, expand non-primitive macros and DEFINE and BEGIN
     2 expand all macros
     3 executing: all macros must be expanded, all values must be defined and
        will be memoized, the form may be destructively altered.

*/
static SCM macroexp1(xorig, env, ctxt, mode)
     SCM xorig, env, ctxt;
     int mode;
{
  SCM x = xorig, linum, proc = UNDEFINED, res = UNDEFINED;
#ifndef RECKLESS
  SCM trace = scm_trace, trace_env = scm_trace_env;
  long argc;
  const char *what = s_wtap;
  MACROEXP_TRACE(xorig, env);
#endif
  x = scm_check_linum(xorig, &linum);
  if (IMP(x) || VECTORP(x)) {	/* Happens for unquoted vectors. */
    if (NIMP(x))
      x = evalatomcar(cons(x, EOL), 0);
    x = cons2(IM_QUOTE, x, EOL);
    goto retx;
  }
  else if (IDENTP(x)) {         /* Happens for @macroexpand1 */
    ASRTER(0==mode, x, "macroexp1", "internal error");
    proc = x;
  }
  else {
    proc = CAR(x);
  }
  ASRTGO(NIMP(proc), errout);
  if (CONSP(proc)) {
    if (mode < 3) {
      x = xorig;
      goto retx;
    }
    if (NIMP(CAR(proc)))
      proc = macroexp1(cons(CAR(proc), CDR(proc)), env, i_procedure, mode);
    if ((127L & IM_LAMBDA)==(127L & CAR(proc))) {
      SCM nenv = CAR(CDR(proc));
      SCM formals = SCM_ENV_FORMALS(nenv);
#ifndef RECKLESS
      if (badargsp(formals, CDR(x))) {
	what = (char *)WNA;
	proc = CAR(x);
	goto errout;
      }
#endif
      res = CDR(x);
      if (ilength(formals) >= 0) {
	x = cons2(IM_LET, nenv, cons(res, CDR(CDR(proc))));
	goto retx;
      }
    }
#ifndef RECKLESS
    if (ilength(x) < 0) {
      what = s_expr;
      goto errout;
    }
#endif
    x = cons2(IM_FUNCALL, proc, CDR(x));
    goto retx;
  }
  ASRTGO(IDENTP(proc), errout);
 macro_tail:
  res = proc;			/* For nicer error message. */
  if (mode >= 3) {
    x = cons(CAR(x), CDR(x));
    proc = scm_lookupval(x, !0);
  }
  else {
    proc = scm_env_lookup(proc, env);
    if (IMP(proc)) {		/* local binding */
      x = scm_add_linum(linum, x);
      goto retx;
    }
    if (CONSP(proc))		/* local syntax binding. */
      proc = CDR(proc);
    else if (SYMBOLP(proc))	/* global variable */
      proc = CDR(sym2vcell(proc));
  }
  if (KEYWORDP(proc)) {
    SCM argv[3];
    long argc = 2;
    proc = KEYWORD_MACRO(proc);
    argv[0] = x;
    argv[1] = env;
    argv[2] = ctxt;
    switch (MAC_TYPE(proc)) {
    case MAC_MACRO: case MAC_MACRO | MAC_PRIMITIVE:
    case MAC_ACRO: case MAC_ACRO | MAC_PRIMITIVE:
	/* This means non-memoizing macros can't expand into internal defines.
	   That's ok with me. */
      if (mode > 1)
	x = cons2(IM_ACRO_CALL, CAR(x), CDR(x));
      goto retx;
    case MAC_MMACRO | MAC_PRIMITIVE:
    case MAC_IDMACRO | MAC_PRIMITIVE:
      if (0==mode ||
          (1==mode && f_define != CDR(proc) && f_begin != CDR(proc))) {
        x = scm_add_linum(linum, x);
        goto retx;
      }
      argv[2] = ctxt;
      argc = 3;
				/* fall through */
    case MAC_MMACRO:
    case MAC_IDMACRO:
      argv[0] = x;
      argv[1] = env;
      x = scm_cvapply(CDR(proc), argc, argv);
      if (ilength(x) <= 0)
	x = cons2((0==mode ? TOPRENAME(i_begin): IM_BEGIN), x, EOL);
      break;
#ifdef MAC_INLINE		/* FIXME this is broken */
    case MAC_INLINE:
      {
	int depth = env_depth();
	res = CDR(proc);
	depth -= INUM(CAR(res));
	res = CDR(res);
	x = cons2(MAKISYMVAL(IM_LET, depth),
		     CAR(res), cons(CDR(x), CDR(res)));
	break;
      }
#endif
    }
    MACROEXP_TRACE(xorig, env);
    x = scm_check_linum(x, 0L);
    if (NIMP(CAR(x)) && IDENTP(CAR(x))) {
      proc = CAR(x);
      goto macro_tail;
    }
#ifndef RECKLESS
    if (IM_DEFINE==CAR(x) && (mode != 1) && !scm_nullenv_p(env)) {
      what = s_placement;
      proc = res = i_define;
    errout:
      if (!UNBNDP(res))
	CAR(x) = res; /* FIXME may not be right for @macroexpand1 */
      if (UNBNDP(proc) && NIMP(x) && CONSP(x))
	proc = CAR(x);
      scm_experr(proc, what, "");
    }
#endif
  }
  else {		/* not a macro expression, car is identifier */
    if (0 == mode)
      x = BOOL_F;
    else if (mode <=2 )
      x = scm_add_linum(linum, x);
#ifndef RECKLESS
    else if (mode >= 3) {
      argc = ilength(CDR(x));
      if (! scm_arity_check(proc, argc, (char *)0)) {
	if (argc < 0) {
	  what = s_expr;
	  proc = x;
	}
	else
	  what = FALSEP(procedurep(proc)) ? s_wtap : (char *)WNA;
	goto errout;
      }
      for (proc = CDR(x); NIMP(proc); proc = CDR(proc)) {
	res = CAR(proc);
	if (NIMP(res)) {
	  if (IDENTP(res))
	    scm_lookupval(proc, !0);
	  else if (CONSP(res))
	    macroexp1(res, env, i_argument, mode);
	}
      }
    }
#endif
  }
 retx:
  if (mode >= 3 && x != xorig) {
    DEFER_INTS;
    CAR(xorig) = CAR(x);
    CDR(xorig) = CDR(x);
    x = xorig;
    ALLOW_INTS;
  }
  MACROEXP_TRACE(trace, trace_env); /* restore */
  return x;
}

#ifndef RECKLESS
int badargsp(formals, args)
     SCM formals, args;
{
  while NIMP(formals) {
    if (NCONSP(formals)) return 0;
    if (IMP(args)) return 1;
    formals = CDR(formals);
    args = CDR(args);
  }
  return NNULLP(args) ? 1 : 0;
}
/* If what is non-null, signals error instead of returning false. */
int scm_arity_check(proc, argc, what)
     SCM proc;
     long argc;
     const char *what;
{
  SCM p = proc;
  if (IMP(p) || argc < 0) goto badproc;
 cclo_tail:
  switch TYP7(p) {
  default:
  badproc:
    if (what) wta(proc, s_wtap, what);
    return 0;
  wrongnumargs:
    if (what) wta(proc, (char *)WNA, what);
    return 0;
  case tc7_subr_0: ASRTGO(0==argc, wrongnumargs) return !0;
  case tc7_contin:
    if (IM_VALUES_TOKEN == CONT(proc)->other.stkframe[1]) return !0;
				/* else fall through */
  case tc7_cxr:
  case tc7_subr_1: ASRTGO(1==argc, wrongnumargs) return !0;
  case tc7_subr_1o: ASRTGO(0==argc || 1==argc, wrongnumargs) return !0;
  case tc7_subr_2: ASRTGO(2==argc, wrongnumargs) return !0;
  case tc7_subr_2o: ASRTGO( 1==argc || 2==argc, wrongnumargs) return !0;
  case tc7_subr_3: ASRTGO(3==argc, wrongnumargs) return !0;
  case tc7_rpsubr:
  case tc7_asubr:
  case tc7_lsubr: return !0;
  case tc7_lsubr_2: ASRTGO(2<=argc, wrongnumargs) return !0;
  case tc7_specfun:
    switch TYP16(proc) {
    default: wta(proc, "internal error", "scm_arity_check");
    case tc16_apply: ASRTGO(2<=argc, wrongnumargs); return !0;
    case tc16_call_cc:
    case tc16_eval: ASRTGO(1==argc, wrongnumargs); /* fall through */
    case tc16_values: return !0;
    case tc16_call_wv: ASRTGO(2==argc, wrongnumargs); return !0;
# ifdef CCLO
    case tc16_cclo:
      p = CCLO_SUBR(p);
      argc++;
      goto cclo_tail;
# endif
    }
  case tcs_closures:
    {
      SCM formals = SCM_ENV_FORMALS(CAR(CODE(p)));
      while (argc--) {
	ASRTGO(NIMP(formals), wrongnumargs);
	if (CONSP(formals))
	  formals = CDR(formals);
	else
	  return !0;
      }
      ASRTGO(IMP(formals) || NCONSP(formals), wrongnumargs);
      return !0;
    }
  }
}
#endif

char s_map[] = "map", s_for_each[] = "for-each", s_eval[] = "@eval";
char s_call_cc[] = "call-with-current-continuation"; /* s_apply[] = "apply"; */

/* static int checking_defines_p(ctxt) SCM ctxt; */
/* {return (NIMP(ctxt) && i_check_defines==CAR(ctxt));} */
/* static SCM wrapenv() */
/* {register SCM z; */
/*  DEFER_INTS_EGC; if (NULLP(scm_env)) return EOL; */
/*  NEWCELL(z); DEFER_INTS_EGC; */
/*  if (NIMP(scm_env) && ENVP(scm_env)) return scm_env; */
/*  CDR(z) = scm_env; CAR(z) = tc16_env; */
/*  EGC_ROOT(z); return z;} */

SCM scm_current_env()
{
  if (NFALSEP(scm_estk))
    return STATIC_ENV;
  return EOL;
}

SCM ceval(x, static_env, env)
     SCM x, static_env, env;
{
  ENV_PUSH;
#ifdef CAUTIOUS
  scm_trace = BOOL_F;
#endif
  TRACE(x);
  STATIC_ENV = static_env;
  scm_env = env;
  x = ceval_1(x);
  ENV_POP;
  ALLOW_INTS_EGC;
  return x;
}
SCM scm_eval_values(x, env, valenv)
     SCM x, env, valenv;
{
  SCM res;
  ENV_PUSH;
#ifdef CAUTIOUS
  scm_trace = BOOL_F;
#endif
  TRACE(x);
  STATIC_ENV = env;
  scm_env = valenv;
  scm_env_tmp = IM_VALUES_TOKEN;
  if (NIMP(x)) x = ceval_1(cons2(IM_EVAL_VALUES, x, EOL));
  DEFER_INTS_EGC;
  if (IM_VALUES_TOKEN==scm_env_tmp) {
    if (UNBNDP(x))
      res = EOL;
    else
      res = cons(x, EOL);
  }
  else
    res = cons2(x, CAR(scm_env_tmp), CDR(scm_env_tmp));
  ENV_POP;
  ALLOW_INTS_EGC;
  return res;
}

SCM scm_apply_cxr(proc, arg1)
     SCM proc, arg1;
{
  double y;
#ifdef FLOATS
  if (SUBRF(proc)) {
    if (INUMP(arg1)) {
      y = DSUBRF(proc)((double) INUM(arg1));
      goto ret;
    }
    ASRTGO(NIMP(arg1), floerr);
    if (REALP(arg1)) {
      y = DSUBRF(proc)(REALPART(arg1));
    ret:
      if (y==y) return makdbl(y, 0.0);
      goto floerr;
    }
# ifdef BIGDIG
    if (BIGP(arg1)) {
      y = DSUBRF(proc)(int2dbl(arg1));
      goto ret;
    }
# endif
  floerr:
    wta(arg1, (char *)ARG1, SNAME(proc));
  }
#endif
  {
    int op = CXR_OP(proc);
#ifndef RECKLESS
    SCM x = arg1;
#endif
    while (op) {
      ASRTER(NIMP(arg1) && CONSP(arg1),
	     x, ARG1, SNAME(proc));
      arg1 = (1 & op ? CAR(arg1) : CDR(arg1));
      op >>= 2;
    }
    return arg1;
  }
}

#ifdef __GNUC__
# define GCC_VERSION (__GNUC__ * 100 + __GNUC_MINOR__)
/* __GNUC_PATCHLEVEL__ */
# if 302 == GCC_VERSION
#  ifdef sparc
#   define GCC_SPARC_BUG
#  endif
# endif
#endif

static SCM ceval_1(x)
     SCM x;
{
#ifdef GCC_SPARC_BUG
  SCM arg1;
#else
  struct {SCM arg_1;} t;
# define arg1 t.arg_1
#endif
  SCM arg2, arg3, proc;
  int envpp = 0;	/* 1 means an environment has been pushed in this
		   invocation of ceval_1, -1 means pushed and then popped. */
#ifdef CAUTIOUS
  SCM xorig;
#endif
  CHECK_STACK;
 loop: POLL;
#ifdef CAUTIOUS
  xorig = x;
#endif
#ifdef SCM_PROFILE
  eval_cases[TYP7(x)]++;
#endif
  switch TYP7(x) {
  case tcs_symbols:
    /* only happens when called at top level */
    x = evalatomcar(cons(x, UNDEFINED), !0);
    goto retx;
  case (127 & IM_AND):
    x = CDR(x);
    arg1 = x;
    while(NNULLP(arg1 = CDR(arg1)))
      if (FALSEP(EVALCAR(x))) {x = BOOL_F; goto retx;}
      else x = arg1;
    goto carloop;
 cdrxbegin:
  case (127 & IM_BEGIN):
    x = CDR(x);
 begin:
    arg1 = x;
    while(NNULLP(arg1 = CDR(arg1))) {
      if (NIMP(CAR(x))) ceval_1(CAR(x));
      x = arg1;
    }
 carloop:			/* eval car of last form in list */
    if (NCELLP(CAR(x))) {
      x = CAR(x);
      x = IMP(x) ? EVALIMP(x) : I_VAL(x);
    }
    else if (ATOMP(CAR(x)))
      x = evalatomcar(x, 0);
    else {
      x = CAR(x);
      goto loop;			/* tail recurse */
    }
 retx:
    ENV_MAY_POP(envpp, 0);
    ALLOW_INTS_EGC;
    return x;

  case (127 & IM_CASE):
    x = scm_case_selector(x);
    goto begin;
  case (127 & IM_COND):
    while(NIMP(x = CDR(x))) {
      proc = CAR(x);
      arg1 = EVALCAR(proc);
      if (NFALSEP(arg1)) {
	x = CDR(proc);
	if (NULLP(x)) {
	  x = arg1;
	  goto retx;
	}
	if (IM_ARROW != CAR(x)) goto begin;
	proc = CDR(x);
	proc = EVALCAR(proc);
	ASRTGO(NIMP(proc), badfun);
	goto evap1;
      }
    }
    x = UNSPECIFIED;
    goto retx;
  case (127 & IM_DO):
    ENV_MAY_PUSH(envpp);
    TRACE(x);
    x = CDR(x);
    ecache_evalx(CAR(CDR(x)));	/* inits */
    STATIC_ENV = CAR(x);
    EXTEND_VALENV;
    x = CDR(CDR(x));
    while (proc = CAR(x), FALSEP(EVALCAR(proc))) {
      for (proc = CAR(CDR(x));NIMP(proc);proc = CDR(proc)) {
	arg1 = CAR(proc);	/* body */
	SIDEVAL_1(arg1);
      }
      ecache_evalx(CDR(CDR(x))); /* steps */
      scm_env = CDR(scm_env);
      EXTEND_VALENV;
    }
    x = CDR(proc);
    if (NULLP(x)) {x = UNSPECIFIED; goto retx;}
    goto begin;
  case (127 & IM_IF):
    x = CDR(x);
    if (NFALSEP(EVALCAR(x))) x = CDR(x);
    else if (IMP(x = CDR(CDR(x)))) {x = UNSPECIFIED; goto retx;}
    goto carloop;
  case (127 & IM_LET):
    ENV_MAY_PUSH(envpp);
    TRACE(x);
#ifdef MAC_INLINE
    arg1 = CAR(x);
#endif
    x = CDR(x);
    ecache_evalx(CAR(CDR(x)));
#ifdef MAC_INLINE
    if (arg1 != IM_LET)	/* inline call */
      env_tail(ISYMVAL(arg1));
#endif
    STATIC_ENV = CAR(x);
    EXTEND_VALENV;
    x = CDR(x);
    goto cdrxbegin;
  case (127 & IM_LETREC):
    ENV_MAY_PUSH(envpp);
    TRACE(x);
    x = CDR(x);
    STATIC_ENV = CAR(x);
#if 0    /*
           The block below signals an error if any variable
           bound in a LETREC is referenced in any init.
         */
    scm_env_tmp = undefineds;
    EXTEND_VALENV;
    x = CDR(x);
    ecache_evalx(CAR(x));
    EGC_ROOT(scm_env);
    CAR(scm_env) = scm_env_tmp;

#else  /* The block below implements LETREC* */
    ecache_undefs(CAR(CAR(x)));
    EXTEND_VALENV;
    x = CDR(x);
    proc = CAR(x);
    while (NIMP(proc)) {
      arg1 = EVALCAR(proc);
      proc = CDR(proc);
      DEFER_INTS_EGC;
      CAR(scm_env_tmp) = arg1;
      scm_env_tmp = CDR(scm_env_tmp);
    }
#endif
    scm_env_tmp = EOL;
    goto cdrxbegin;
  case (127 & IM_LETSTAR):
    ENV_MAY_PUSH(envpp);
    TRACE(x);
    x = CDR(x);
    proc = CDR(CAR(x));
    /* No longer happens.
      if (IMP(proc)) {
        scm_env_tmp = EOL;
	EXTEND_VALENV;
	goto cdrxbegin;
	}
    */
    scm_env_tmp = EOL;		/* needed so multiple values cause an error
                               to be signaled when this is a top-level form. */
    do {
      scm_env_tmp = EVALCAR(proc);
      proc = CDR(proc);
      STATIC_ENV = CAR(proc);
      EXTEND_VALENV;
    } while NIMP(proc = CDR(proc));
    goto cdrxbegin;
  case (127 & IM_OR):
    x = CDR(x);
    arg1 = x;
    while(NNULLP(arg1 = CDR(arg1))) {
      x = EVALCAR(x);
      if (NFALSEP(x)) goto retx;
      x = arg1;
    }
    goto carloop;
  case (127 & IM_LAMBDA):
    x = closure(CDR(x), ISYMVAL(CAR(x)));
    goto retx;
  case (127 & IM_QUOTE):
    x = CAR(CDR(x));
    goto retx;
  case (127 & IM_SET):
    x = CDR(x);
    arg2 = EVALCAR(CDR(x));
    proc = CAR(x);
    switch (7 & (int)proc) {
    case 0:
      if (ECONSP(proc))
	if (ISYMP(CAR(proc))) *farlookup(proc) = arg2;
	else {
	  x = scm_multi_set(proc, arg2);
	  goto retx;
	}
      else *lookupcar(x) = arg2;
      break;
    case 1:
      I_VAL(proc) = arg2;
      break;
    case 4:
      *ilookup(proc) = arg2;
      break;
    }
#ifdef SICP
    x = arg2;
#else
    x = UNSPECIFIED;
#endif
    goto retx;
  case (127 & IM_FUNCALL):
    x = CDR(x);
    proc = ceval_1(CAR(x));
    break;
  case (127 & MAKISYM(0)):
    proc = CAR(x);
    ASRTGO(ISYMP(proc), badfun);
#ifdef SCM_PROFILE
    eval_cases_other[ISYMNUM(proc)]++;
#endif
    switch ISYMNUM(proc) {
    case (ISYMNUM(IM_APPLY)):
      x = CDR(x);
      proc = evalcar(x);
      ASRTGO(NIMP(proc), badfun);
      arg1 = evalcar(CDR(x));
      if (CLOSUREP(proc)) {
	ENV_MAY_PUSH(envpp);
	TRACE(x);
	scm_env_tmp = arg1;
#ifndef RECKLESS
	goto clo_checked;
#else
	goto clo_unchecked;
#endif
      }
      x = apply(proc, arg1, EOL);
      goto retx;
    case (ISYMNUM(IM_DELAY)):
      x = makprom(closure(CDR(x), 0));
      goto retx;
    case (ISYMNUM(IM_QUASIQUOTE)):
      ALLOW_INTS_EGC;
      x = iqq(CAR(CDR(x)));
      goto retx;
    case (ISYMNUM(IM_FARLOC_CAR)):
    case (ISYMNUM(IM_FARLOC_CDR)):
      x = *farlookup(x);
      goto retx;
    case (ISYMNUM(IM_EVAL_FOR_APPLY)):
      /* only happens when called from C-level apply or cvapply */
      envpp = 1;
      proc = CAR(scm_env_tmp);
      scm_env_tmp = CDR(scm_env_tmp);
      goto clo_unchecked;
    case (ISYMNUM(IM_LET_SYNTAX)):
      x = CDR(x);
      STATIC_ENV = CAR(x);
      goto cdrxbegin;
    case (ISYMNUM(IM_ACRO_CALL)):
      x = acro_call(x, STATIC_ENV);
      goto loop;
    case (ISYMNUM(IM_LINUM)):
#ifndef MEMOIZE_LOCALS
      x = CDR(x);               /* For non-memoizing case,
                                   just throw away line number. */
      goto loop;
#else
      goto expand;
#endif
    case (ISYMNUM(IM_DEFINE)):
      x = toplevel_define(x, STATIC_ENV);
      goto retx;
    case (ISYMNUM(IM_EVAL_VALUES)):
                                /* Push magic VALUES token on estk until
                                   tail call occurs.  Only happens when called
                                   from scm_eval_values. */
      ENV_MAY_PUSH(envpp);
      scm_env_tmp = EOL;
      goto cdrxbegin;
    /* new syntactic forms go here. */
    default:
      goto badfun;
    }
  default:
    proc = x;
  badfun:
#ifdef CAUTIOUS
    scm_trace = BOOL_F;
    everr(xorig, STATIC_ENV, proc, s_wtap, "", 0);
#else
    everr(x, STATIC_ENV, proc, s_wtap, "", 0);
#endif
  case tc7_vector:
  case tcs_uves:
  case tc7_smob:
    goto retx;
  case (127 & ILOC00):
    proc = *ilookup(CAR(x));
    break;
  case tcs_cons_gloc:
    proc = I_VAL(CAR(x));
    break;
  case tcs_cons_nimcar:
  expand:
    TOP_TRACE(x, STATIC_ENV);
#ifdef MEMOIZE_LOCALS
    x = macroexp1(x, STATIC_ENV, EOL, 3);
    goto loop;
#else
    if (ATOMP(CAR(x))) {
      proc = scm_lookupval(x, 0);
      if (KEYWORDP(proc)) {
	x = macroexp1(x, STATIC_ENV, EOL, 3);
	goto loop;
      }
    }
    else proc = ceval_1(CAR(x));
#endif
  }
    /* At this point proc is the evaluated procedure from the function
       position and x has the form which is being evaluated. */
  ASRTGO(NIMP(proc), badfun);
  scm_estk_ptr[0] = scm_env; /* For error reporting at wrongnumargs. */
  if (NULLP(CDR(x))) {
  evap0:
    TOP_TRACE(xorig, STATIC_ENV);
    ENV_MAY_POP(envpp, CLOSUREP(proc));
    ALLOW_INTS_EGC;
    switch TYP7(proc) { /* no arguments given */
    case tc7_subr_0:
      return SUBRF(proc)();
    case tc7_subr_1o:
      return SUBRF(proc) (UNDEFINED);
    case tc7_lsubr:
      return SUBRF(proc)(EOL);
    case tc7_rpsubr:
      return BOOL_T;
    case tc7_asubr:
      return SUBRF(proc)(UNDEFINED, UNDEFINED);
    case tcs_closures:
      DEFER_INTS_EGC;
      ENV_MAY_PUSH(envpp);
      scm_env_tmp = EOL;
#ifdef SCM_PROFILE
      eval_clo_cases[0][0]++;
#endif
#ifdef CAUTIOUS
      if (0!=ARGC(proc)) {
      clo_checked:
	arg1 = SCM_ENV_FORMALS(CAR(CODE(proc)));
	DEFER_INTS_EGC;
	arg2 = scm_env_tmp;
	while NIMP(arg1) {
	  if (NCONSP(arg1)) goto clo_unchecked;
	  if (IMP(arg2)) goto umwrongnumargs;
	  arg1 = CDR(arg1);
	  arg2 = CDR(arg2);
	}
	if (NNULLP(arg2)) goto umwrongnumargs;
      }
#else /* def CAUTIOUS */
    clo_checked:
#endif
    clo_unchecked:
      x = CODE(proc);
      scm_env = ENV(proc);
      STATIC_ENV = CAR(x);
      EXTEND_VALENV;
      TRACE(CDR(x));
      goto cdrxbegin;
    case tc7_specfun:
      switch TYP16(proc) {
	/* default: break; */
#ifdef CCLO
      case tc16_cclo:
	arg1 = proc;
	proc = CCLO_SUBR(proc);
	goto evap1;
#endif
      case tc16_values:
	return scm_values(UNDEFINED, UNDEFINED, EOL, s_values);
      }
    case tc7_contin:
      scm_dynthrow(proc, UNDEFINED, UNDEFINED, EOL);
    case tc7_subr_1:
    case tc7_subr_2:
    case tc7_subr_2o:
    case tc7_cxr:
    case tc7_subr_3:
    case tc7_lsubr_2:
    umwrongnumargs:
    wrongnumargs:
      if (envpp < 0) {
	scm_estk_ptr += SCM_ESTK_FRLEN;
	scm_env = scm_estk_ptr[0];
      }
#ifdef CAUTIOUS
      if (xorig==scm_trace) STATIC_ENV = scm_trace_env;
      TOP_TRACE(BOOL_F, BOOL_F);
      everr(xorig, STATIC_ENV, proc, (char *)WNA, "", 0);
#else
      everr(x, STATIC_ENV, proc, (char *)WNA, "", 0);
#endif
    default:
      goto badfun;
    }
  }
  x = CDR(x);
#ifdef CAUTIOUS
  if (IMP(x))
    goto wrongnumargs;
#endif
  arg1 = EVALCAR(x);
  x = CDR(x);
  if (NULLP(x)) {
    TOP_TRACE(xorig, STATIC_ENV);
evap1:
    ENV_MAY_POP(envpp, CLOSUREP(proc));
    ALLOW_INTS_EGC;
    switch TYP7(proc) { /* have one argument in arg1 */
    case tc7_subr_2o:
      return SUBRF(proc)(arg1, UNDEFINED);
    case tc7_subr_1:
    case tc7_subr_1o:
      return SUBRF(proc)(arg1);
    case tc7_cxr:
      return scm_apply_cxr(proc, arg1);
    case tc7_rpsubr:
      return BOOL_T;
    case tc7_asubr:
      return SUBRF(proc)(arg1, UNDEFINED);
    case tc7_lsubr:
      return SUBRF(proc)(cons(arg1, EOL));
    case tcs_closures:
      ENV_MAY_PUSH(envpp);
#ifdef SCM_PROFILE
      eval_clo_cases[1][ARGC(proc)]++;
#endif
      if (1==ARGC(proc)) {
	scm_env_cons(arg1, EOL);
	goto clo_unchecked;
      }
      else {
	scm_env_tmp = cons(arg1, EOL);
	goto clo_checked;
      }
    case tc7_contin:
      scm_dynthrow(proc, arg1, UNDEFINED, EOL);
    case tc7_specfun:
      switch TYP16(proc) {
      case tc16_call_cc:
	proc = arg1;
	DEFER_INTS_EGC;
	arg1 = scm_make_cont();
	EGC_ROOT(arg1);
	x = setjump(CONT(arg1)->jmpbuf);
	if (x) {
#ifdef SHORT_INT
	  x = (SCM)thrown_value;
#endif
#ifdef CHEAP_CONTINUATIONS
	  envpp = 0;
#endif
	  goto retx;
	}
	ASRTGO(NIMP(proc), badfun);
	goto evap1;
      case tc16_eval:
	ENV_MAY_PUSH(envpp);
	TRACE(arg1);
	STATIC_ENV = eval_env;
	scm_env = EOL;
	x = arg1;
	if (IMP(x)) goto retx;
	goto loop;
#ifdef CCLO
      case tc16_cclo:
	arg2 = UNDEFINED;
	goto cclon;
	/* arg2 = arg1;
	   arg1 = proc;
	   proc = CCLO_SUBR(proc);
	   goto evap2; */
#endif
      case tc16_values: return arg1;
      }
    case tc7_subr_2:
    case tc7_subr_0:
    case tc7_subr_3:
    case tc7_lsubr_2:
      goto wrongnumargs;
    default:
      goto badfun;
    }
  }
#ifdef CAUTIOUS
  if (IMP(x)) goto wrongnumargs;
#endif
  {				/* have two or more arguments */
    arg2 = EVALCAR(x);
    x = CDR(x);
    if (NULLP(x)) {		/* have two arguments */
      TOP_TRACE(xorig, STATIC_ENV);
  evap2:
      ENV_MAY_POP(envpp, CLOSUREP(proc));
      ALLOW_INTS_EGC;
      switch TYP7(proc) {
      case tc7_subr_2:
      case tc7_subr_2o:
	return SUBRF(proc)(arg1, arg2);
      case tc7_lsubr:
	return SUBRF(proc)(cons2(arg1, arg2, EOL));
      case tc7_lsubr_2:
	return SUBRF(proc)(arg1, arg2, EOL);
      case tc7_rpsubr:
      case tc7_asubr:
	return SUBRF(proc)(arg1, arg2);
      case tc7_specfun:
	switch TYP16(proc) {
	case tc16_apply:
	  proc = arg1;
	  ASRTGO(NIMP(proc), badfun);
	  if (NULLP(arg2)) goto evap0;
	  if (IMP(arg2) || NCONSP(arg2)) {
	    x = arg2;
	  badlst: wta(x, (char *)ARGn, s_apply);
	  }
	  arg1 = CAR(arg2);
	  x = CDR(arg2);
	apply3:
	  if (NULLP(x)) goto evap1;
	  ASRTGO(NIMP(x) && CONSP(x), badlst);
	  arg2 = CAR(x);
	  x = CDR(x);
	apply4:
	  if (NULLP(x)) goto evap2;
	  ASRTGO(NIMP(x) && CONSP(x), badlst);
	  arg3 = x;
	  x = scm_cp_list(CDR(x), 0);
#ifndef RECKLESS
	  if (UNBNDP(x)) {x = arg3; goto badlst;}
#endif
	  arg3 = CAR(arg3);
	  goto evap3;
#ifdef CCLO
	case tc16_cclo: cclon:
	  arg3 = arg2;
	  arg2 = arg1;
	  arg1 = proc;
	  proc = CCLO_SUBR(proc);
	  if (UNBNDP(arg3)) goto evap2;
	  goto evap3;
	  /* return apply(CCLO_SUBR(proc),
		       cons2(proc, arg1, cons(arg2, x)), EOL); */
#endif
	case tc16_values:
	  return scm_values(arg1, arg2, EOL, s_values);
	case tc16_call_wv:
	  ENV_MAY_PUSH(envpp);
	  scm_env_tmp = IM_VALUES_TOKEN; /* Magic value recognized by VALUES */
	  arg1 = apply(arg1, EOL, EOL);
	  proc = arg2;
	  DEFER_INTS_EGC;
	  if (IM_VALUES_TOKEN==scm_env_tmp) {
	    scm_env_tmp = EOL;
	    if (UNBNDP(arg1)) goto evap0;
	    goto evap1;
	  }
	  arg2 = CAR(scm_env_tmp);
	  x = CDR(scm_env_tmp);
	  goto apply4;   /* Jumping to apply code results in extra list copy
			    for >=3 args, but we want to minimize bloat. */
	}
      case tc7_contin:
	scm_dynthrow(proc, arg1, arg2, EOL);
      case tc7_subr_0:
      case tc7_cxr:
      case tc7_subr_1o:
      case tc7_subr_1:
      case tc7_subr_3:
	goto wrongnumargs;
      default:
	goto badfun;
      case tcs_closures:
	ENV_MAY_PUSH(envpp);
#ifdef SCM_PROFILE
	eval_clo_cases[2][ARGC(proc)]++;
#endif
	switch ARGC(proc) {
	case 2:
	  scm_env_cons2(arg1, arg2, EOL);
	  goto clo_unchecked;
	case 1:
	  scm_env_cons(arg1, cons(arg2, EOL));
	  goto clo_checked;
	case 0:
	case 3:		/* Error, will be caught at clo_checked: */
	  scm_env_tmp = cons2(arg1, arg2, EOL);
	  goto clo_checked;
	}
      }
    }
    {				/* have 3 or more arguments */
      arg3 = EVALCAR(x);
      x = CDR(x);
      if (NIMP(x)) {
	if (CLOSUREP(proc) && 3==ARGC(proc)) {
	  ALLOW_INTS_EGC;
	  ENV_MAY_PUSH(envpp);
	  if (ecache_eval_args(proc, arg1, arg2, arg3, x))
	    goto clo_unchecked;
	  goto umwrongnumargs;
	}
	x = eval_args(x);
      }
      TOP_TRACE(xorig, STATIC_ENV);
    evap3:
      ENV_MAY_POP(envpp, CLOSUREP(proc));
      ALLOW_INTS_EGC;
      switch TYP7(proc) {
      case tc7_subr_3:
	ASRTGO(NULLP(x), wrongnumargs);
	return SUBRF(proc)(arg1, arg2, arg3);
      case tc7_asubr:
      case tc7_rpsubr:
	return asubr_apply(proc, arg1, arg2, arg3, x);
	/* return apply(proc, cons2(arg1, arg2, cons(arg3, x)), EOL); */
      case tc7_lsubr_2:
	return SUBRF(proc)(arg1, arg2, cons(arg3, x));
      case tc7_lsubr:
	return SUBRF(proc)(cons2(arg1, arg2, cons(arg3, x)));
      case tcs_closures:
	ENV_MAY_PUSH(envpp);
#ifdef SCM_PROFILE
	eval_clo_cases[IMP(x)?3:4][ARGC(proc)]++;
#endif
	switch ARGC(proc) {
	case 3:
	  scm_env_cons3(arg1, arg2, arg3, x);
	  goto clo_checked;
	case 2:
	  scm_env_cons2(arg1, arg2, cons(arg3, x));
	  goto clo_checked;
	case 1:
	  scm_env_cons(arg1, cons2(arg2, arg3, x));
	  goto clo_checked;
	case 0:
	  scm_env_tmp = cons2(arg1, arg2, cons(arg3, x));
	  goto clo_checked;
	}
      case tc7_specfun:
	switch TYP16(proc) {
	case tc16_apply:
	  proc = arg1;
	  ASRTGO(NIMP(proc), badfun);
	  arg1 = arg2;
	  if (IMP(x)) {
	    x = arg3;
	    goto apply3;
	  }
	  arg2 = arg3;
	  if (IMP(CDR(x))) {
	    x = CAR(x);
	    goto apply4;
	  }
	  arg3 = CAR(x);
	  x = nconc2copy(CDR(x));
	  goto evap3;
#ifdef CCLO
	case tc16_cclo:
	  x = cons(arg3, x);
	  goto cclon;
#endif
	case tc16_values:
	  return scm_values(arg1, arg2, cons(arg3, x), s_values);
	}
      case tc7_contin:
	  scm_dynthrow(proc, arg1, arg2, cons(arg3, x));
      case tc7_subr_2:
      case tc7_subr_1o:
      case tc7_subr_2o:
      case tc7_subr_0:
      case tc7_cxr:
      case tc7_subr_1:
	goto wrongnumargs;
      default:
	goto badfun;
      }
    }
  }
#undef arg1
}

SCM procedurep(obj)
     SCM obj;
{
	if (NIMP(obj)) switch TYP7(obj) {
	case tcs_closures:
	case tc7_contin:
	case tcs_subrs:
	case tc7_specfun:
	  return BOOL_T;
	}
	return BOOL_F;
}

static char s_proc_doc[] = "procedure-documentation";
SCM l_proc_doc(proc)
     SCM proc;
{
  SCM env;
  ASRTER(BOOL_T==procedurep(proc) && NIMP(proc) && TYP7(proc) != tc7_contin,
	 proc, ARG1, s_proc_doc);
  switch TYP7(proc) {
  case tcs_closures:
    env = CAR(CODE(proc));
    env = scm_env_getprop(SCM_ENV_DOC, CAR(CODE(proc)));
    return IMP(env) ? BOOL_F : CAR(env);
  default:
    return BOOL_F;
/*
  case tcs_subrs:
  case tc7_specfun:
*/
  }
}

/* This code is for apply. it is destructive on multiple args.
   This will only screw you if you do (apply apply '( ... )) */
/* Copy last (list) argument, so SET! in a closure can't mutate it. */
SCM nconc2copy(lst)
     SCM lst;
{
  SCM last, *lloc = &lst;
#ifdef CAUTIOUS
  ASRTER(ilength(lst) >= 1, lst, WNA, s_apply);
#endif
  while NNULLP(CDR(*lloc)) lloc = &CDR(*lloc);
#ifdef CAUTIOUS
  ASRTER(ilength(CAR(*lloc)) >= 0, lst, ARGn, s_apply);
#endif
  last = CAR(*lloc);
  *lloc = EOL;
  for (; NIMP(last); last=CDR(last)) {
    *lloc = cons(CAR(last), EOL);
    lloc = &CDR(*lloc);
  }
  return lst;
}
/* Shallow copy.  If LST is not a proper list of length at least
   MINLEN, returns UNDEFINED */
SCM scm_cp_list(lst, minlen)
     SCM lst;
     int minlen;
{
  SCM res, *lloc = &res;
  res = EOL;
  for (; NIMP(lst) && CONSP(lst); lst = CDR(lst)) {
    *lloc = cons(CAR(lst), EOL);
    lloc = &CDR(*lloc);
    minlen--;
  }
  if (NULLP(lst) && minlen <= 0)
    return res;
  return UNDEFINED;
}
SCM scm_v2lst(n, v, end)
     long n;
     SCM *v, end;
{
  SCM res = end;
  for (n--; n >= 0; n--) res = cons(v[n], res);
  return res;
}
SCM apply(proc, arg1, args)
     SCM proc, arg1, args;
{
  ASRTGO(NIMP(proc), badproc);
  if (NULLP(args))
    if (NULLP(arg1)) arg1 = UNDEFINED;
    else {
      args = CDR(arg1);
      arg1 = CAR(arg1);
    }
  else
    args = nconc2copy(args);
 cc_tail:
  ALLOW_INTS_EGC;
  switch TYP7(proc) {
  default:
  badproc:
    wta(proc, (char *)ARG1, s_apply);
  wrongnumargs:
    wta(proc, (char *)WNA, s_apply);
  case tc7_subr_2o:
    if (NULLP(args)) {
      args = UNDEFINED;
      return SUBRF(proc)(arg1, args);
    }
    /* Fall through */
  case tc7_subr_2:
    ASRTGO(NIMP(args) && NULLP(CDR(args)), wrongnumargs);
    args = CAR(args);
    return SUBRF(proc)(arg1, args);
  case tc7_subr_0:
    ASRTGO(UNBNDP(arg1), wrongnumargs);
    return SUBRF(proc)();
  case tc7_subr_1:
  case tc7_subr_1o:
    ASRTGO(NULLP(args), wrongnumargs);
    return SUBRF(proc)(arg1);
  case tc7_cxr:
    ASRTGO(NULLP(args), wrongnumargs);
    return scm_apply_cxr(proc, arg1);
  case tc7_subr_3:
    ASRTGO(NIMP(args) && NIMP(CDR(args)) && NULLP(CDR(CDR(args))),
	   wrongnumargs);
    return SUBRF(proc)(arg1, CAR(args), CAR(CDR(args)));
  case tc7_lsubr:
    return SUBRF(proc)(UNBNDP(arg1) ? EOL : cons(arg1, args));
  case tc7_lsubr_2:
    ASRTGO(NIMP(args) && CONSP(args), wrongnumargs);
    return SUBRF(proc)(arg1, CAR(args), CDR(args));
  case tc7_asubr:
    if (NULLP(args)) return SUBRF(proc)(arg1, UNDEFINED);
    while NIMP(args) {
      ASRTER(CONSP(args), args, ARG2, s_apply);
      arg1 = SUBRF(proc)(arg1, CAR(args));
      args = CDR(args);
    }
    return arg1;
  case tc7_rpsubr:
    if (NULLP(args)) return BOOL_T;
    while NIMP(args) {
      ASRTER(CONSP(args), args, ARG2, s_apply);
      if (FALSEP(SUBRF(proc)(arg1, CAR(args)))) return BOOL_F;
      arg1 = CAR(args);
      args = CDR(args);
    }
    return BOOL_T;
  case tcs_closures: {
    arg1 = (UNBNDP(arg1) ? EOL : cons(arg1, args));
#ifndef RECKLESS
    if (badargsp(SCM_ENV_FORMALS(CAR(CODE(proc))), arg1)) goto wrongnumargs;
#endif
    ENV_PUSH;
    scm_env_cons(proc, arg1);
    arg1 = ceval_1(f_evapply);
    return arg1;
  }
  case tc7_contin:
    if (NULLP(args)) scm_dynthrow(proc, arg1, UNDEFINED, EOL);
				/* else fall through */
  case tc7_specfun:
    args = UNBNDP(arg1) ? EOL : cons(arg1, args);
    arg1 = proc;
#ifdef CCLO
    proc = (TYP16(proc)==tc16_cclo ? CCLO_SUBR(proc) : f_apply_closure);
#else
    proc = f_apply_closure;
#endif
    goto cc_tail;
  }
}

/* This function does not check that proc is a procedure, nor that
   it accepts n arguments.  Call scm_arity_check to do that. */
SCM scm_cvapply(proc, n, argv)
     SCM proc, *argv;
     long n;
{
  SCM res;
  long i;
 tail:
  ALLOW_INTS_EGC;
  switch TYP7(proc) {
  default: return UNSPECIFIED;
  case tc7_subr_2o:
    if (1==n) return SUBRF(proc)(argv[0], UNDEFINED);
    /* Fall through */
  case tc7_subr_2:
    return SUBRF(proc)(argv[0], argv[1]);
  case tc7_subr_0:
    return SUBRF(proc)();
  case tc7_subr_1o:
    if (0==n) return SUBRF(proc)(UNDEFINED);
    /* Fall through */
  case tc7_subr_1:
    return SUBRF(proc)(argv[0]);
  case tc7_cxr:
    return scm_apply_cxr(proc, argv[0]);
  case tc7_subr_3:
    return SUBRF(proc)(argv[0], argv[1], argv[2]);
  case tc7_lsubr:
    return SUBRF(proc)(0==n ? EOL : scm_v2lst(n, argv, EOL));
  case tc7_lsubr_2:
    return SUBRF(proc)(argv[0], argv[1],
		       2==n ? EOL : scm_v2lst(n-2, &argv[2], EOL));
  case tc7_asubr:
    if (1 >= n) return SUBRF(proc)(0==n ? UNDEFINED: argv[0], UNDEFINED);
    res = argv[0];
    for (i = 1; i < n; i++)
      res = SUBRF(proc)(res, argv[i]);
    return res;
  case tc7_rpsubr:
    if (1 >= n) return BOOL_T;
    for (i = 0; i < n-1; i++)
      if (FALSEP(SUBRF(proc)(argv[i], argv[i+1]))) return BOOL_F;
    return BOOL_T;
  case tcs_closures: {
    SCM p = proc;
    ENV_PUSH;
    i = ARGC(proc);
    if (3==i) {
      scm_env_tmp = EOL;
      ENV_V2LST(n, argv);
    }
    else {
      scm_env_tmp = (i < n) ? scm_v2lst(n-i, &argv[i], EOL) : EOL;
      if (i>0)
	ENV_V2LST(i, argv);
    }
    ENV_V2LST(1L, &p);
    res = ceval_1(f_evapply);
    return res;
  }
  case tc7_contin:
    if (1 == n) scm_dynthrow(proc, argv[0], UNDEFINED, EOL);
    goto call_apply;
  case tc7_specfun:
    if (tc16_apply==TYP16(proc)) {
      proc = argv[0];
      argv++;
      n--;
#ifndef RECKLESS
      scm_arity_check(proc, n, s_apply);
#endif
      goto tail;
    }
  call_apply:
    res = cons(proc, 0==n ? EOL : scm_v2lst(n, argv, EOL));
#ifdef CCLO
    proc = (TYP16(proc)==tc16_cclo ? CCLO_SUBR(proc) : f_apply_closure);
#else
    proc = f_apply_closure;
#endif
    return apply(proc, res, EOL);
  }
}

SCM map(proc, arg1, args)
     SCM proc, arg1, args;
{
  SCM res = EOL, *pres = &res;
  SCM heap_ve, auto_ve[5], auto_ave[5];
  SCM *ve = auto_ve, *ave = auto_ave;
  long i, n = ilength(args) + 1;
  scm_protect_temp(&heap_ve);  /* Keep heap_ve from being optimized away. */
  if (NULLP(arg1)) return res;
#ifndef RECKLESS
  scm_arity_check(proc, n, s_map);
#endif
  ASRTER(NIMP(arg1), arg1, ARG2, s_map);
#ifdef CCLO
  if (tc16_cclo==TYP16(proc)) {
    args = cons(arg1, args);
    arg1 = cons(proc, EOL);
    SETCDR(arg1, arg1);		/* circular list */
    proc = CCLO_SUBR(proc);
    n++;
  }
#endif
  if (n > 5) {
    heap_ve = make_vector(MAKINUM(2*n), BOOL_F);
    ve = VELTS(heap_ve);
    ave = &(ve[n]);
  }
  ve[0] = arg1;
  ASRTER(NIMP(ve[0]), arg1, ARG2, s_map);
  for (i = 1; i < n; i++) {
    ve[i] = CAR(args);
    ASRTER(NIMP(ve[i]), ve[i], ARGn, s_map);
    args = CDR(args);
  }
  while (1) {
    arg1 = EOL;
    for (i = n-1;i >= 0;i--) {
      if (IMP(ve[i])) {
	/* We could check for lists the same length here. */
	return res;
      }
      ASRTER(CONSP(ve[i]), ve[i], 0==i ? ARG2 : ARGn, s_map);
      ave[i] = CAR(ve[i]);
      ve[i] = CDR(ve[i]);
    }
    *pres = cons(scm_cvapply(proc, n, ave), EOL);
    pres = &CDR(*pres);
  }
}
SCM for_each(proc, arg1, args)
     SCM proc, arg1, args;
{
  SCM heap_ve, auto_ve[5], auto_ave[5];
  SCM *ve = auto_ve, *ave = auto_ave;
  long i, n = ilength(args) + 1;
  scm_protect_temp(&heap_ve);  /* Keep heap_ve from being optimized away. */
  if (NULLP(arg1)) return UNSPECIFIED;
#ifndef RECKLESS
  scm_arity_check(proc, n, s_for_each);
#endif
  ASRTER(NIMP(arg1), arg1, ARG2, s_for_each);
#ifdef CCLO
  if (tc16_cclo==TYP16(proc)) {
    args = cons(arg1, args);
    arg1 = cons(proc, EOL);
    SETCDR(arg1, arg1);		/* circular list */
    proc = CCLO_SUBR(proc);
    n++;
  }
#endif
  if (n > 5) {
    heap_ve = make_vector(MAKINUM(2*n), BOOL_F);
    ve = VELTS(heap_ve);
    ave = &(ve[n]);
  }
  ve[0] = arg1;
  ASRTER(NIMP(ve[0]), arg1, ARG2, s_for_each);
  for (i = 1; i < n; i++) {
    ve[i] = CAR(args);
    ASRTER(NIMP(ve[i]), args, ARGn, s_for_each);
    args = CDR(args);
  }
  while (1) {
    arg1 = EOL;
    for (i = n-1;i >= 0;i--) {
      if (IMP(ve[i])) {
	return UNSPECIFIED;
      }
      ASRTER(CONSP(ve[i]), ve[i], 0==i ? ARG2 : ARGn, s_for_each);
      ave[i] = CAR(ve[i]);
      ve[i] = CDR(ve[i]);
    }
    scm_cvapply(proc, n, ave);
  }
}

/* The number of required arguments up to 3 is encoded in the cdr of the
   closure.  A value 3 means no rest argument, 3 or more required arguments.
   This information is used to make sure that rest args are not
   allocated in the environment cache. */
SCM closure(code, argc)
     SCM code;
     int argc;
{
	register SCM z;
	NEWCELL(z);
	SETCODE(z, code);
	DEFER_INTS_EGC;
	if (IMP(scm_env))
	  CDR(z) = argc<<1;
	else {
	  CDR(z) = scm_env | (argc<<1);
	  EGC_ROOT(z);
	}
	return z;
}

long tc16_promise;
SCM makprom(code)
     SCM code;
{
	register SCM z;
	NEWCELL(z);
	CDR(z) = code;
	CAR(z) = tc16_promise;
	return z;
}
static int prinprom(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  lputs("#<promise", port);
  if ((2L<<16) & CAR(exp)) lputc('*', port);
  lputc(' ', port);
  scm_iprin1(CDR(exp), port, writing);
  lputc('>', port);
  return !0;
}

static SCM makro(code, flags, what)
     SCM code;
     long flags;
     const char *what;
{
  register SCM z;
  ASRTER(scm_arity_check(code, (MAC_PRIMITIVE & flags ? 3L : 2L),
			 (char *)0), code, ARG1, what);
  NEWCELL(z);
  CDR(z) = code;
  CAR(z) = tc16_macro | (flags << 16);
  return z;
}
static char s_makacro[] = "procedure->syntax";
SCM makacro(code)
     SCM code;
{
  return makro(code, MAC_ACRO, s_makacro);
}
static char s_makmacro[] = "procedure->macro";
SCM makmacro(code)
     SCM code;
{
  return makro(code, MAC_MACRO, s_makmacro);
}
static char s_makmmacro[] = "procedure->memoizing-macro";
SCM makmmacro(code)
     SCM code;
{
  return makro(code, MAC_MMACRO, s_makmmacro);
}
static char s_makidmacro[] = "procedure->identifier-macro";
SCM makidmacro(code)
     SCM code;
{
  return makro(code, MAC_IDMACRO, s_makidmacro);
}
#ifdef MACRO
/* Functions for  smart expansion */

/* @MACROEXPAND1 returns:
   '#F' if its argument is not a macro invocation,
   the argument if the argument is a primitive syntax invocation,
   the result of expansion if the argument is a macro invocation
   (BEGIN #F) will be returned instead of #F if #F is the result.
 */
static char s_macroexpand1[] = "@macroexpand1";
SCM scm_macroexpand1(x, env)
     SCM x, env;
{
  SCM proc;
  if (IMP(x)) return BOOL_F;
  if (CONSP(x)) {
    proc = CAR(x);
    if (IMP(proc) || !IDENTP(proc)) return BOOL_F; /* probably an error */
  }
  else if (IDENTP(x)) {
    proc = scm_env_lookup(x, env);
    if (IMP(proc))		/* local binding */
      return BOOL_F;
    if (SYMBOLP(proc)) {   /* global variable */
      proc = CDR(sym2vcell(proc));
      if (!KEYWORDP(proc))
        return BOOL_F;
    }
  }
  else
    return BOOL_F;
  return macroexp1(x, env, BOOL_F, 0);
}

static char s_eval_syntax[] = "eval-syntax";
SCM scm_eval_syntax(x, env)
     SCM x, env;
{
  SCM venv = cons(undefineds, undefineds);
  CDR(venv) = venv;
  return EVAL(x, env, venv);
}
#endif /* MACRO */

static int prinmacro(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  lputs("#<", port);
  if (MAC_TYPE(exp) & MAC_PRIMITIVE) lputs("primitive-", port);
  switch (MAC_TYPE(exp) & ~MAC_PRIMITIVE) {
  default:
    lputs("macro", port); break;
  case MAC_ACRO:
    lputs("syntax", port); break;
#ifdef MAC_INLINE
  case MAC_INLINE:
    lputs("inline function", port); break;
#endif
  }
  if (MAC_TYPE(exp) & MAC_MEMOIZING) lputc('!', port);
  lputc(' ', port);
  scm_iprin1(CDR(exp), port, writing);
  lputc('>', port);
  return !0;
}
static int prinenv(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  lputs("#<environment ", port);
  scm_intprint((long)exp, -16, port);
  /* scm_iprin1(CDR(exp), port, writing); */
  lputc('>', port);
  return !0;
}
#ifdef MACRO
static int prinid(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  SCM s = IDENT_PARENT(exp);
  while (M_IDENTP(s)) s = IDENT_PARENT(s);
  lputs("#<id ", port);
  scm_iprin1(s, port, writing);
  lputc(':', port);
  scm_intprint((long)exp, -16, port);
  lputc('>', port);
  return !0;
}
#endif
char s_force[] = "force";
SCM force(x)
     SCM x;
{
 tail:
  ASRTGO(NIMP(x) && (TYP16(x)==tc16_promise), badx);
  switch (CAR(x)>>16) {
  default:
  badx: wta(x, (char *)ARG1, s_force);
  case 0:
    {
      SCM ans;
      int mv = (IM_VALUES_TOKEN==scm_env_tmp);
      ans = scm_cvapply(CDR(x), 0L, (SCM *)0);
      if (mv) {
	DEFER_INTS_EGC;
	if (IM_VALUES_TOKEN==scm_env_tmp) {
	  if (!UNBNDP(ans)) mv = 0;
	}
	else {
	  ans = cons2(ans, CAR(scm_env_tmp), CDR(scm_env_tmp));
	  scm_env_tmp = IM_VALUES_TOKEN;
	}
	ALLOW_INTS_EGC;
      }
      if (!((1L<<16) & CAR(x))) {
	DEFER_INTS;
	CDR(x) = ans;
	CAR(x) |= mv ? (3L<<16) : (1L<<16);
	ALLOW_INTS;
      }
      goto tail;
    }
  case 1: return CDR(x);
  case 3:
    x = CDR(x);
    if (UNBNDP(x)) return scm_values(UNDEFINED, UNDEFINED, EOL, s_force);
    return scm_values(CAR(x), CAR(CDR(x)), CDR(CDR(x)), s_force);
  }
}

SCM copytree(obj)
     SCM obj;
{
  SCM ans, tl;
  if (IMP(obj)) return obj;
  if (VECTORP(obj)) {
    sizet i = LENGTH(obj);
    ans = make_vector(MAKINUM(i), UNSPECIFIED);
    while(i--) VELTS(ans)[i] = copytree(VELTS(obj)[i]);
    return ans;
  }
  if (NCONSP(obj)) return obj;
/*  return cons(copytree(CAR(obj)), copytree(CDR(obj))); */
  ans = tl = cons(copytree(CAR(obj)), UNSPECIFIED);
  while(NIMP(obj = CDR(obj)) && CONSP(obj))
    tl = (CDR(tl) = cons(copytree(CAR(obj)), UNSPECIFIED));
  CDR(tl) = obj;
  return ans;
}
SCM eval(obj)
     SCM obj;
{
  obj = copytree(obj);
  return EVAL(obj, EOL, EOL);
}

static char s_definedp[] = "defined?";
SCM definedp(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = CDR(xorig);
  SCM proc;

  ASSYNT(1 == ilength(x), xorig, s_body, s_definedp);
  proc = CAR(x);
#ifdef MACRO
  proc = id2sym(proc);
#endif
  return (ISYMP(proc)
	  || (NIMP(proc) && IDENTP(proc)
	      && !UNBNDP(CDR(sym2vcell(proc)))))?
		(SCM)BOOL_T : (SCM)BOOL_F;
}

#ifdef MACRO
static char s_identp[] = "identifier?";
SCM identp(obj)
     SCM obj;
{
  return (NIMP(obj) && IDENTP(obj)) ? BOOL_T : BOOL_F;
}

static char s_ident_eqp[] = "identifier-equal?";
SCM ident_eqp(id1, id2, env)
     SCM id1, id2, env;
{
  SCM s1 = id1, s2 = id2;
# ifndef RECKLESS
  if (IMP(id1))
  badarg1: wta(id1, (char *)ARG1, s_ident_eqp);
  if (IMP(id1))
  badarg2: wta(id2, (char *)ARG2, s_ident_eqp);
# endif
  if (id1==id2) return BOOL_T;
  while M_IDENTP(s1) s1 = IDENT_PARENT(s1);
  while M_IDENTP(s2) s2 = IDENT_PARENT(s2);
  ASRTGO(SYMBOLP(s1), badarg1);
  ASRTGO(SYMBOLP(s2), badarg2);
  if (s1 != s2) return BOOL_F;
  s1 = scm_env_lookup(id1, env);
  s2 = scm_env_lookup(id2, env);
  if (s1==s2) return BOOL_T;
  if (NIMP(s1) && ISYMP(CAR(s1)))	/* FARLOC case */
    return equal(s1, s2);
  return BOOL_F;
}

static char s_ident2sym[] = "identifier->symbol";
SCM ident2sym(id)
     SCM id;
{
  id = id2sym(id);
  ASRTER(NIMP(id) && SYMBOLP(id), id, ARG1, s_ident2sym);
  return id;
}

static char s_renamed_ident[] = "renamed-identifier";
SCM renamed_ident(id, env)
     SCM id, env;
{
  SCM z;
  ASRTER(NIMP(id) && IDENTP(id), id, ARG1, s_renamed_ident);
  NEWCELL(z);
  while (NIMP(env)) {
    if (INUMP(CAR(env))) {
      ASRTER(NIMP(CDR(env)), env, s_badenv, s_renamed_ident);
      env = CDR(CDR(env));
    }
    else if (SCM_LINUMP(CAR(env))) {
      env = CDR(env);
    }
    else {
      ASRTER(NULLP(env) || (NIMP(env) && CONSP(env)),
	     env, s_badenv, s_renamed_ident);
      break;
    }
  }
  if (scm_nullenv_p(env)) {
    CAR(z) = tc16_ident;
    CDR(z) = id;
    return z;
  }
  else {
    SCM y;
    CAR(z) = id;
    CDR(z) = env;
    NEWCELL(y);
    CAR(y) = tc16_ident | 1L<<16;
    CDR(y) = z;
    return y;
  }
}

static char s_syn_quote[] = "syntax-quote";
SCM m_syn_quote(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  ASSYNT(ilength(CDR(xorig))==1, xorig, s_expression, s_syn_quote);
  return cons(IM_QUOTE, CDR(xorig));
}

static char s_defsyntax[] = "defsyntax";
SCM m_defsyntax(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM x = CDR(xorig), name, val;
  ASSYNT(ilength(x)==2, xorig, s_expression, s_defsyntax);
  ASSYNT(scm_nullenv_p(env), xorig, s_placement, s_defsyntax);
  name = CAR(x);
  ASSYNT(NIMP(name) && IDENTP(name), name, s_variable, s_defsyntax);
  val = evalcar(CDR(x));
  ASSYNT(NIMP(val) && MACROP(val), CAR(CDR(x)), s_expr, s_defsyntax);
  checked_define(name, cons(IM_KEYWORD, val), s_defsyntax);
  return UNSPECIFIED;
}

SCM m_let_syntax(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM proc, vars, inits, fr;
  SCM body = m_parse_let(EOL, xorig, CDR(xorig), &vars, &inits);
  /*  if (IMP(vars)) return m_let_null(body, env, ctxt); */
  /* Add a unique frame for an environment mark. */
  env = EXTEND_ENV(cons(SCM_ENV_SYNTAX, EOL), env);
  for (fr = EOL; NIMP(inits); inits = CDR(inits)) {
    proc = scm_eval_syntax(CAR(inits), env);
    ASSYNT(NIMP(proc) && MACROP(proc), CAR(inits), s_expr, s_let_syntax);
    fr = acons(CAR(vars), proc, fr);
    vars = CDR(vars);
  }
  fr = cons(SCM_ENV_SYNTAX, fr);
  env = EXTEND_ENV(fr, env);
  return cons2(IM_LET_SYNTAX, env, m_body(body, env, ctxt));
}
static char s_letrec_syntax[] = "letrec-syntax";
SCM m_letrec_syntax(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM proc, vars, vals, inits, fr;
  SCM body = m_parse_let(EOL, xorig, CDR(xorig), &vars, &inits);
  /*  if (IMP(vars)) return m_let_null(body, env, ctxt); */
  for (fr = EOL; NIMP(vars); vars = CDR(vars))
    fr = acons(CAR(vars), UNDEFINED, fr);
  fr = cons(SCM_ENV_SYNTAX, fr);
  env = EXTEND_ENV(fr, env);
  for (vals = EOL; NIMP(inits); inits = CDR(inits)) {
    proc = scm_eval_syntax(CAR(inits), env);
    ASSYNT(NIMP(proc) && MACROP(proc), CAR(inits), s_expr, s_letrec_syntax);
    vals = cons(proc, vals);
  }
  for (fr = CDR(fr); NIMP(fr); fr = CDR(fr)) {
    CDR(CAR(fr)) = CAR(vals);
    vals = CDR(vals);
  }
  return cons2(IM_LET_SYNTAX, env, m_body(body, env, ctxt));
}

static char s_the_macro[] = "the-macro";
SCM m_the_macro(xorig, env, ctxt)
     SCM xorig, env, ctxt;
{
  SCM addr, x = CDR(xorig);
  ASSYNT(1==ilength(x), xorig, s_expression, s_the_macro);
  x = CAR(x);
  ASSYNT(NIMP(x) && IDENTP(x), x, s_expression, s_the_macro);
  addr = scm_env_lookup(x, env);
				/* Require global ref for now. */
  ASSYNT(NIMP(addr) && SYMBOLP(addr), x, s_expression, s_the_macro);
  x = CDR(sym2vcell(addr));
  ASSYNT(KEYWORDP(x), xorig, ARG1, s_the_macro);
  return KEYWORD_MACRO(x);
}
#endif

static iproc subr1s[] = {
	{"@copy-tree", copytree},
/*	{s_eval, eval}, now a (tail recursive) specfun */
	{s_force, force},
	{s_proc_doc, l_proc_doc},
	{s_makacro, makacro},
	{s_makmacro, makmacro},
	{s_makmmacro, makmmacro},
	{s_makidmacro, makidmacro},
	{"apply:nconc-to-last", nconc2copy},
	/*	{s_env2tree, env2tree}, */
#ifdef MACRO
	{s_identp, identp},
	{s_ident2sym, ident2sym},
#endif
	{0, 0}};

static iproc subr2s[] = {
#ifdef MACRO
	{s_macroexpand1, scm_macroexpand1},
	{s_eval_syntax, scm_eval_syntax},
#endif
	{0, 0}};

static iproc lsubr2s[] = {
/*	{s_apply, apply}, now explicity initted */
	{s_map, map},
	{s_for_each, for_each},
	{0, 0}};

static iproc subr3s[] = {
#ifdef MACRO
  {s_ident_eqp, ident_eqp},
#endif
  {0, 0}};

static smobfuns promsmob = {markcdr, free0, prinprom};
static smobfuns macrosmob = {markcdr, free0, prinmacro};
static smobfuns envsmob = {markcdr, free0, prinenv};
#ifdef MACRO
static smobfuns idsmob = {markcdr, free0, prinid};
#endif

SCM make_synt(name, flags, fcn)
     const char *name;
     long flags;
     SCM (*fcn)();
{
  SCM symcell = sysintern(name, UNDEFINED);
  SCM z = makro(scm_maksubr(name, tc7_subr_3, fcn),
		flags | MAC_PRIMITIVE, "make_synt");
#ifdef MACRO
  z = cons(IM_KEYWORD, z);
#endif
  CDR(symcell) = z;
  return CAR(symcell);
}
SCM make_specfun(name, typ, flags)
     char *name;
     int typ, flags;
{
  SCM symcell = sysintern(name, UNDEFINED);
  register SCM z;
  NEWCELL(z);
  CAR(z) = (long)typ | ((long)flags)<<16;
  CDR(z) = CAR(symcell);
  CDR(symcell) = z;
  return z;
}
void init_eval()
{
  scm_env = EOL;
  scm_env_tmp = UNSPECIFIED;
#ifndef RECKLESS
  scm_trace = BOOL_F;
  scm_trace_env = EOL;
#endif
  tc16_promise = newsmob(&promsmob);
  tc16_macro = newsmob(&macrosmob);
  tc16_env = newsmob(&envsmob);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);
  init_iprocs(lsubr2s, tc7_lsubr_2);
  init_iprocs(subr3s, tc7_subr_3);
#ifdef SCM_PROFILE
  make_subr("scm:profile", tc7_subr_1o, scm_profile);
#endif
  make_specfun(s_apply, tc16_apply, 0);
  make_specfun(s_call_cc, tc16_call_cc, 0);
  make_specfun(s_eval, tc16_eval, 0);
  make_specfun(s_values, tc16_values, 0);
  make_specfun(s_call_wv, tc16_call_wv, 0);
  add_feature(s_values);

  i_dot = CAR(sysintern(".", UNDEFINED));
  i_arrow = CAR(sysintern("=>", UNDEFINED));
  i_else = CAR(sysintern("else", UNDEFINED));
  i_unquote = CAR(sysintern("unquote", UNDEFINED));
  i_uq_splicing = CAR(sysintern("unquote-splicing", UNDEFINED));
  i_quasiquote = make_synt(s_quasiquote, MAC_MMACRO, m_quasiquote);
  i_define = make_synt(s_define, MAC_MMACRO, m_define);
  make_synt(s_delay, MAC_MMACRO, m_delay);

  i_bind = CAR(sysintern("bind", UNDEFINED));
  i_anon = CAR(sysintern("<anon>", UNDEFINED));
  i_side_effect = CAR(sysintern("side-effect", UNDEFINED));
  i_test = CAR(sysintern("test", UNDEFINED));
  i_procedure = CAR(sysintern("procedure", UNDEFINED));
  i_argument = CAR(sysintern("argument", UNDEFINED));
  i_check_defines = CAR(sysintern("check-defines", UNDEFINED));
  loc_atcase_aux = &CDR(sysintern("@case-aux", UNDEFINED));

  /* acros */
  make_synt(s_definedp, MAC_ACRO, definedp);
  /* end of acros */

  make_synt(s_and, MAC_MMACRO, m_and);
  i_begin = make_synt(s_begin, MAC_MMACRO, m_begin);
  make_synt(s_case, MAC_MMACRO, m_case);
  make_synt(s_cond, MAC_MMACRO, m_cond);
  make_synt(s_do, MAC_MMACRO, m_do);
  make_synt(s_if, MAC_MMACRO, m_if);
  i_lambda = make_synt(s_lambda, MAC_MMACRO, m_lambda);
  i_let = make_synt(s_let, MAC_MMACRO, m_let);
  make_synt(s_letrec, MAC_MMACRO, m_letrec);
  make_synt(s_letstar, MAC_MMACRO, m_letstar);
  make_synt(s_or, MAC_MMACRO, m_or);
  i_quote = make_synt(s_quote, MAC_MMACRO, m_quote);
  make_synt(s_set, MAC_MMACRO, m_set);
  make_synt(s_atapply, MAC_MMACRO, m_apply);
  /*  make_synt(s_atcall_cc, MAC_MMACRO, m_cont); */
#ifdef MAC_INLINE
  make_synt("@inline-lambda", MAC_MMACRO, m_inline_lambda);
#endif
#ifdef MACRO
  tc16_ident = newsmob(&idsmob);
  make_subr(s_renamed_ident, tc7_subr_2, renamed_ident);
  make_synt(s_syn_quote, MAC_MMACRO, m_syn_quote);
  make_synt(s_defsyntax, MAC_MMACRO, m_defsyntax);
  make_synt(s_let_syntax, MAC_MMACRO, m_let_syntax);
  make_synt(s_letrec_syntax, MAC_MMACRO, m_letrec_syntax);

  make_synt(s_the_macro, MAC_ACRO, m_the_macro);
  add_feature("primitive-hygiene");
#endif

  f_begin = CDR(CDR(KEYWORD_MACRO(sym2vcell(i_begin))));
  f_define = CDR(CDR(KEYWORD_MACRO(sym2vcell(i_define))));

  list_unspecified = cons(UNSPECIFIED, EOL);
  f_evapply = cons(IM_EVAL_FOR_APPLY, EOL);
#ifdef SCM_ENV_FILENAME
  eval_env = scm_env_addprop(SCM_ENV_FILENAME,
			     CAR(sysintern("eval", UNDEFINED)),
			     EOL);
#else
  eval_env = EOL;
#endif
  f_apply_closure = scm_evstr("(let ((ap apply)) (lambda (p . a) (ap p a)))");
}
