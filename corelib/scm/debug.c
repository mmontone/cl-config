/* "debug.c" procedures for displaying and debugging code.
 * Copyright (C) 2001 Free Software Foundation, Inc.
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

/* Authors: Radey Shouman & Aubrey Jaffer. */

#include "scm.h"
#include "setjump.h"

long tc16_codeptr;		/*  Type code for non-executable code
				    wrappers */

static SCM prinhead P((SCM x, SCM port, int writing));
static void prinbindings P((SCM names, SCM inits, SCM init_env,
			    SCM steps, SCM step_env, SCM port, int writing));

SCM scm_env_rlookup(addr, stenv, what)
     SCM addr, stenv;
     const char *what;
{
  SCM env, fr;
  int icdrp;
  unsigned int idist, iframe;
  if (IMP(addr)) {
    if (!ILOCP(addr)) return BOOL_F;
    iframe = IFRAME(addr);
    idist = IDIST(addr);
    icdrp = ICDRP(addr) && 1;
  }
  else {
    if (!ISYMP(CAR(addr))) return BOOL_F;
    icdrp = 0;
    switch (ISYMNUM(CAR(addr))) {
    default: return BOOL_F;
    case (ISYMNUM(IM_FARLOC_CDR)):
      icdrp = 1;
    case (ISYMNUM(IM_FARLOC_CAR)):
      fr = CDR(addr);
      iframe = INUM(CAR(fr));
      idist = INUM(CDR(fr));
      break;
    }
  }
  for (env = stenv; NIMP(env); env = CDR(env)) {
    fr = CAR(env);
    if (INUMP(fr)) {
      ASRTER(NIMP(env) && CONSP(env), stenv, s_badenv, what);
      env = CDR(env);
      continue;
    }
    if (SCM_LINUMP(fr)) continue;
    if (NIMP(fr) && CONSP(fr) && IMP(CAR(fr))) continue;
    if (0==iframe--) {
      while (idist--) {
	if (IMP(fr) || NCONSP(fr)) return BOOL_F;
	fr = CDR(fr);
      }
      if (!icdrp) {
	if (NIMP(fr) && CONSP(fr))
	  fr = CAR(fr);
	else
	  fr = BOOL_F;
      }
      if (NIMP(fr) && IDENTP(fr)) return fr;
      return BOOL_F;
    }
  }
  return BOOL_F;
}

SCM scm_env_addprop(prop, val, env)
     SCM prop, val, env;
{
  if (UNBNDP(prop)) return env;
  return cons2(prop, val, env);
}

SCM scm_env_getprop(prop, env)
     SCM prop, env;
{
  SCM e = env;
  if (!UNBNDP(prop)) {
    while (NIMP(e)) {
      if (INUMP(CAR(e))) {
	if (CAR(e)==prop) return CDR(e);
	e = CDR(e);
	ASRTER(NIMP(e), env, s_badenv, "env_getprop");
      }
      e = CDR(e);
    }
  }
  return BOOL_F;
}

static SCM prinhead(x, port, writing)
     SCM x, port;
     int writing;
{
  lputc('(', port);
  scm_iprin1(CAR(x), port, writing);
  lputc(' ', port);
  return CDR(x);
}

static void prinbindings(names, inits, init_env,
			 steps, step_env, port, writing)
     SCM names, inits, init_env, steps, step_env, port;
     int writing;
{
  lputc('(', port);
  while (NIMP(names) && NIMP(inits)) {
    lputc('(', port);
    scm_iprin1(CAR(names), port, writing);
    lputc(' ', port);
    scm_princode(CAR(inits), init_env, port, writing);
    if (NIMP(steps)) {
      lputc(' ', port);
      scm_princode(CAR(steps), step_env, port, writing);
      steps = CDR(steps);
    }
    lputc(')', port);
    names = CDR(names);
    inits = CDR(inits);
    if (NIMP(names)) lputc(' ', port);
  }
  lputs(") ", port);
}

void scm_princode(code, env, port, writing)
     SCM code, env, port;
     int writing;
{
  SCM oenv = env, name, init, bdgs, x = code;
  if (UNBNDP(env)) {
    scm_iprin1(code, port, writing);
    return;
  }
 tail:
  if (IMP(x)) {
    if (ILOCP(x)) {
    local:
      name = scm_env_rlookup(x, env, "princode");
      if (FALSEP(name)) goto gencase;
      lputs("#@", port);
      scm_iprin1(name, port, writing);
      return;
    }
    else
      goto gencase;
  }
  if (SCM_GLOCP(x)) {
    scm_iprin1(x, port, writing);
    return;
  }
  switch (TYP7(x)) {
  default: gencase:
    scm_iprin1(x, port, writing);
    return;
  gencode:
  case tcs_cons_gloc:
  case tcs_cons_nimcar:
  case tcs_cons_iloc:
  case (127 & IM_OR): case (127 & IM_AND): case (127 & IM_BEGIN):
  case (127 & IM_SET): case (127 & IM_COND): case (127 & IM_CASE):
  case (127 & IM_IF):
    lputc('(', port);
    scm_princode(CAR(x), env, port, writing);
  body:
    x = CDR(x);
  no_cdr:
    for (; NNULLP(x); x = CDR(x)) {
      if (IMP(x) || NECONSP(x)) {
	lputs(" . ", port);
	scm_iprin1(x, port, writing);
	break;
      }
      lputc(' ', port);
      scm_princode(CAR(x), env, port, writing);
    }
    lputc(')', port);
    return;
  case (127 & IM_LAMBDA):
    x = prinhead(x, port, writing);
    env = CAR(x);
    bdgs = SCM_ENV_FORMALS(env);
    if (IMP(bdgs) || NECONSP(bdgs))
      scm_iprin1(bdgs, port, writing);
    else {
      lputc('(', port);
      while (!0) {
	if (NECONSP(bdgs)) break;
	scm_iprin1(CAR(bdgs), port, writing);
	if (NIMP(bdgs = CDR(bdgs)))
	  lputc(' ', port);
	else break;
      }
      if (NIMP(bdgs)) {
	lputs(". ", port);
	scm_iprin1(bdgs, port, writing);
      }
      lputc(')', port);
    }
    goto body;
  case (127 & IM_LETREC):
  case (127 & IM_LET):
    x = prinhead(x, port, writing);
    env = CAR(x);
    prinbindings(SCM_ENV_FORMALS(env),
		 CAR(CDR(x)), (TYP7(x)==(127 & IM_LET) ? oenv: env),
		 UNDEFINED, UNDEFINED, port, writing);
    x = CDR(x);
    goto body;
  case (127 & IM_LETSTAR):
    x = prinhead(x, port, writing);
    lputc('(', port);
    if (NIMP(bdgs = CAR(x))) {
      oenv = CAR(bdgs);
      bdgs = CDR(bdgs);
      while (!0) {
	init = CAR(bdgs);
	bdgs = CDR(bdgs);
	env = CAR(bdgs);
	lputc('(', port);
	scm_iprin1(SCM_ENV_FORMALS(env), port, writing);
	lputc(' ', port);
	scm_princode(init, oenv, port, writing);
	oenv = env;
	lputc(')', port);
	if (IMP(bdgs = CDR(bdgs)))
	  break;
	lputc(' ', port);
      }
    }
    lputs(") ", port);
    goto body;
  case (127 & IM_DO):
    {
      /* (#@do (env (init ...) (test ...) (body ...) step ...)) */
      SCM test, steps;
      x = prinhead(x, port, writing);
      env = CAR(x);
      x = CDR(x);
      init = CAR(x);
      x = CDR(x);
      test = CAR(x);
      x = CDR(x);
      steps = CDR(x);
      x = CAR(x);
      prinbindings(SCM_ENV_FORMALS(env), init, oenv, steps, env,
		   port, writing);
      scm_princode(test, env, port, writing);
      lputc(' ', port);
      goto no_cdr;
    }
  case (127 & IM_FUNCALL):
    lputc('(', port);
    x = CDR(x);
    scm_princode(CAR(x), env, port, writing);
    goto body;
  case (127 & MAKISYM(0)):
    if (!ISYMP(CAR(x))) goto gencode;
    switch (ISYMNUM(CAR(x))) {
    default:
      goto gencode;
    case ISYMNUM(IM_LINUM):
      x = CDR(x);
      goto tail;
    case ISYMNUM(IM_FARLOC_CAR):
    case ISYMNUM(IM_FARLOC_CDR):
      goto local;
    }
  }
}

void scm_princlosure(proc, port, writing)
     SCM proc, port;
     int writing;
{
  SCM env, linum = UNDEFINED;
  proc = CODE(proc);
  lputs("#<CLOSURE ", port);
  env = CAR(proc);
#ifdef CAUTIOUS
  if (NIMP(env=scm_env_getprop(SCM_ENV_PROCNAME, env))) {
    scm_iprin1(CAR(env), port, 1);
    lputc(' ', port);
    env = CDR(env);
    if (NIMP(env) && SCM_LINUMP(CAR(env)))
      linum = CAR(env);
  }
  else
    lputs("<anon> ", port);
  env = CAR(proc);
  if (NIMP(env=scm_env_getprop(SCM_ENV_FILENAME, env)))
    scm_line_msg(CAR(env), linum, port);
#endif
  env = CAR(proc);
  scm_iprin1(SCM_ENV_FORMALS(env), port, writing);
  if (writing) {
    for (proc = CDR(proc); NIMP(proc); proc = CDR(proc)) {
      lputc(' ', port);
      scm_princode(CAR(proc), env, port, writing);
    }
  }
  lputc('>', port);
}

static char s_int2linum[] = "integer->line-number";
SCM scm_int2linum(n)
     SCM n;
{
  int i = INUM(n);
  ASRTER(INUMP(n) && i >= 0, n, ARG1, s_int2linum);
  return SCM_MAKE_LINUM(i);
}

static char s_linum2int[] = "line-number->integer";
SCM scm_linum2int(linum)
     SCM linum;
{
  ASRTER(SCM_LINUMP(linum), linum, ARG1, s_linum2int);
  return MAKINUM(SCM_LINUM(linum));
}

SCM scm_linump(obj)
     SCM obj;
{
  return SCM_LINUMP(obj) ? BOOL_T : BOOL_F;
}

static char s_remove_linums[] = "remove-line-numbers!";
SCM scm_remove_linums(x)
     SCM x;
{
  SCM ret = x;
  SCM *px = &ret;
 tail:
  x = *px;
  if (IMP(x)) return ret;
  if (CONSP(x)) {
    if (SCM_LINUMP(CAR(x))) {
      *px = CDR(x);
      px = &CDR(x);
      goto tail;
    }
    if (NIMP(CAR(x)))
      CAR(x) = scm_remove_linums(CAR(x));
    px = &CDR(x);
    goto tail;
  }
  else if (VECTORP(x)) {
    SCM *ve = VELTS(x);
    sizet i = LENGTH(x);
    while (i--) {
      if (NIMP(ve[i]))
	ve[i] = scm_remove_linums(ve[i]);
    }
    return ret;
  }
  else
    return ret;
}

#ifdef CAUTIOUS
long num_frames(estk, i)
     SCM estk;
     int i;
{
  long n = 0;
  while NIMP(estk) {
    n += (i - SCM_ESTK_BASE)/SCM_ESTK_FRLEN;
    i = INUM(SCM_ESTK_PARENT_INDEX(estk));
    estk = SCM_ESTK_PARENT(estk);
  }
  return n;
}

SCM *estk_frame(estk, i, nf)
     SCM estk;
     int i, nf;
{
  int n;
  /* Make this 1-based, because continuations have an extra frame at
     the top of the estk. */
  nf -= 1;
  while NIMP(estk) {
    n = (i - SCM_ESTK_BASE)/SCM_ESTK_FRLEN;
    if (nf <= n) return &(VELTS(estk)[i - nf*SCM_ESTK_FRLEN]);
    nf -= n;
    i = INUM(SCM_ESTK_PARENT_INDEX(estk));
    estk = SCM_ESTK_PARENT(estk);
  }
  return (SCM *)0;
}

SCM stacktrace1(estk, i)
     SCM estk;
     int i;
{
  SCM *frame, env, ste, lste = UNDEFINED;
  int n, nf = num_frames(estk, i);
  int nbrk1 = 7, nbrk2 = nf - 6;
  if (nf <= 0) return BOOL_F;
  lputs("\n;STACK TRACE", cur_errp);
  for (n = 1; n <= nf; n++) {
    if ((0 <= nbrk1--) || n >= nbrk2) {
      if (!(frame = estk_frame(estk, i, n))) continue;
      if (BOOL_F==(ste = frame[3])) continue;
      env = frame[2];
      if (ste != lste) {
	lste = ste;
	if (reset_safeport(sys_safep, 65, cur_errp)) {
	  /* The usual C setjmp, not SCM's setjump. */
	  if (0==setjmp(SAFEP_JMPBUF(sys_safep))) {
	    lputc('\n', cur_errp);
	    scm_intprint((long)n, -10, sys_safep);
	    lputs("; ", sys_safep);
	    scm_princode(ste, env, sys_safep, 1);
	  }
	}
      }
      else {
	lputs("\n...", cur_errp);
	break;
      }
    }
  }
  lputc('\n', cur_errp);
  return BOOL_T;
}

SCM *cont_frame(contin, nf)
     SCM contin;
     int nf;
{
  CONTINUATION *cont = CONT(contin);
  SCM estk = cont->other.estk;
  int i = LENGTH(estk) - SCM_ESTK_FRLEN;
  if (0 == nf) return cont->other.stkframe;
  return estk_frame(estk, i, nf);
}

static char s_stack_trace[] = "stack-trace";
SCM scm_stack_trace(contin)
     SCM contin;
{
  SCM estk;
  int i;
  if (UNBNDP(contin)) {
    estk = scm_estk;
    i = (scm_estk_ptr - VELTS(scm_estk));
  }
  else {
    CONTINUATION *cont;
    ASRTER(NIMP(contin) && (tc7_contin==TYP7(contin)), contin, ARG1,
	   s_stack_trace);
    cont = CONT(contin);
    estk = cont->other.estk;
    i = LENGTH(estk) - SCM_ESTK_FRLEN;
  }
  return stacktrace1(estk, i);
}

static char s_frame_trace[] = "frame-trace";
SCM scm_frame_trace(contin, nf)
     SCM contin, nf;
{
    SCM *stkframe, code, env;
    ASRTER(NIMP(contin) && tc7_contin==TYP7(contin), contin, ARG1,
	   s_frame_trace);
    ASRTER(INUMP(nf) && INUM(nf) >= 0, nf, ARG2, s_frame_trace);
    if (!(stkframe = cont_frame(contin, INUM(nf))))
      return BOOL_F;
    env = stkframe[2];
    code = stkframe[3];
    scm_princode(code, env, cur_errp, 1);
    scm_scope_trace(env);
    return UNSPECIFIED;
}

static char s_frame2env[] = "frame->environment";
SCM scm_frame2env(contin, nf)
     SCM contin, nf;
{
    SCM *stkframe;
    ASRTER(NIMP(contin) && tc7_contin==TYP7(contin), contin, ARG1,
	   s_frame2env);
    ASRTER(INUMP(nf) && INUM(nf) >= 0, nf, ARG2, s_frame2env);
    if (!(stkframe = cont_frame(contin, INUM(nf))))
      return BOOL_F;
    return stkframe[2];
}

static char s_frame_eval[] = "frame-eval";
SCM scm_frame_eval(contin, nf, expr)
     SCM contin, nf, expr;
{
    SCM res, env, *stkframe;
    ASRTER(NIMP(contin) && tc7_contin==TYP7(contin), contin, ARG1,
	   s_frame_eval);
    ASRTER(INUMP(nf) && INUM(nf) >= 0, nf, ARG2, s_frame_eval);
    if (!(stkframe = cont_frame(contin, INUM(nf))))
      return BOOL_F;
    env = stkframe[2];
    if (IMP(expr)) return expr;
    DEFER_INTS_EGC;
    res = ceval(expr, env, stkframe[0]);
    ALLOW_INTS_EGC;
    return res;
}

#endif

static char s_scope_trace[] = "scope-trace";
SCM scm_scope_trace(env)
     SCM env;
{
  SCM ef, file = UNDEFINED;
  int fprinted = 0;
  if (UNBNDP(env))
    env = scm_current_env();
  else if (NIMP(env) && CLOSUREP(env))
    env = CAR(CODE(env));
  if (scm_nullenv_p(env))
    lputs("\n; in top level environment.", cur_errp);
  else
    lputs("\n; in scope:", cur_errp);
#ifdef CAUTIOUS
  if (NIMP(ef=scm_env_getprop(SCM_ENV_FILENAME, env))) {
    file = CAR(ef);
  }
#endif
  for (; NIMP(env); env = CDR(env)) {
    if (NCONSP(env)) {
    badenv:
      lputs("\n; corrupted environment ", cur_errp);
      scm_iprin1(env, cur_errp, 1);
      return UNSPECIFIED;
    }
    ef = CAR(env);
    if (SCM_LINUMP(ef)) {
      lputs("\n;   ", cur_errp);
      scm_line_msg(file, ef, cur_errp);
      fprinted++;
    }
    else if (INUMP(ef)) {
      ASRTGO(NIMP(env) && CONSP(env), badenv);
      env = CDR(env);
#ifdef CAUTIOUS
      switch (ef) {
      default: break;
      case SCM_ENV_PROCNAME:
	lputs("  procedure ", cur_errp);
	scm_iprin1(CAR(env), cur_errp, 1);
	break;
      }
#endif
    }
    else if (NIMP(ef) && CONSP(ef) && NIMP(CAR(ef)) && CONSP(CAR(ef))) {
      lputs("\n;   ", cur_errp);
      scm_iprin1(CAR(ef), cur_errp, 1);
      lputs("  syntax bindings", cur_errp);
    }
    else {
      lputs("\n;   ", cur_errp);
      scm_iprin1(ef, cur_errp, 1);
    }
  }
#ifdef CAUTIOUS
  if (NIMP(file) && !fprinted) {
    lputs("\n; defined by ", cur_errp);
    if (NIMP(file) && STRINGP(file))
      lputs("load: ", cur_errp);
    scm_iprin1(file, cur_errp, 1);
    lputc('\n', cur_errp);
  }
#endif
  return UNSPECIFIED;
}

static char s_env_annotation[] = "environment-annotation";
SCM scm_env_annotation(var, stenv)
     SCM var, stenv;
{
  SCM s, frame, env = stenv;
#ifdef MACRO
  SCM mark = IDENT_ENV(var);
  if (NIMP(mark)) mark = CAR(mark);
#endif
  for (; NIMP(env); env = CDR(env)) {
    frame = CAR(env);
#ifdef MACRO
    if (frame==mark) {
      var = IDENT_PARENT(var);
      mark = IDENT_ENV(var);
      if (NIMP(mark)) mark = CAR(mark);
    }
#endif
    if (IMP(frame)) {
      if (INUMP(frame)) {
#ifndef RECKLESS
	if (!(NIMP(env) && CONSP(env))) {
	badenv: wta(stenv, s_badenv, s_env_annotation);
	}
#endif
	env = CDR(env);
      }
      continue;
    }
#ifdef MACRO
    if (NIMP(frame) && CONSP(frame) && BOOL_F==CAR(frame)) {
      /* syntax binding */
      s = assq(var, CDR(frame));
      if (NIMP(s)) goto local_out;
      continue;
    }
#endif
    for (; NIMP(frame); frame = CDR(frame)) {
      if (NCONSP(frame)) {
	if (var==frame)
	  goto local_out;
	break;
      }
      if (CAR(frame)==var) {
      local_out:
	env = CDR(env);
	if (IMP(env)) return BOOL_T;
	if (SCM_ENV_ANNOTATION != CAR(env)) return BOOL_T;
	env = CDR(env);
	ASRTGO(NIMP(env), badenv);
	s = assq(var, CAR(env));
	if (NIMP(s)) return s;
	return BOOL_T;
      }
      ASRTGO(CONSP(frame), badenv);
    }
  }
  ASRTGO(NULLP(env), badenv);
  return BOOL_F;
}

/* This is to be used for code backpointers to go into environments,
   allowing run-time reporting of error line numbers. */
SCM scm_wrapcode(code, env)
     SCM code, env;
{
  SCM z, x = cons(env, code);
  NEWCELL(z);
  CDR(z) = x;
  CAR(z) = tc16_codeptr;
  return z;
}

static int princodeptr(exp, port, writing)
     SCM exp;
     SCM port;
     int writing;
{
  SCM env = CAR(CDR(exp));
  lputs("#<code ", port);
  exp = CDR(CDR(exp));
  if (IMP(exp) || NCONSP(exp))
    scm_princode(exp, env, port, writing);
  else {
    while (!0) {
      if (IMP(exp) || NCONSP(exp)) break;
      scm_princode(CAR(exp), env, port, writing);
      lputc(' ', port);
      exp = CDR(exp);
    }
    if (NNULLP(exp)) {
      lputs(" . ", port);
      scm_princode(exp, env, port, writing);
    }
  }
  lputc('>', port);
  return !0;
}

static smobfuns codesmob = {markcdr, free0, princodeptr};

static iproc subr1os[] = {
  {s_scope_trace, scm_scope_trace},
#ifdef CAUTIOUS
  {s_stack_trace, scm_stack_trace},
#endif
	{0, 0}};

static iproc subr1s[] = {
  {s_int2linum, scm_int2linum},
  {"line-number?", scm_linump},
  {s_linum2int, scm_linum2int},
  {s_remove_linums, scm_remove_linums},
  {0, 0}};

static iproc subr2s[] = {
  {s_env_annotation, scm_env_annotation},
#ifdef CAUTIOUS
  {s_frame_trace, scm_frame_trace},
  {s_frame2env, scm_frame2env},
#endif
  {0, 0}};

void init_debug()
{
  tc16_codeptr = newsmob(&codesmob);
  init_iprocs(subr1os, tc7_subr_1o);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);
#ifdef CAUTIOUS
  make_subr(s_frame_eval, tc7_subr_3, scm_frame_eval);
#endif
}
