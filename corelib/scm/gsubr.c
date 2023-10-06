/* "gsubr.c" CCLOs taking general number of required, optional, and rest args.
 * Copyright (C) 1994, 1995, 1996 Free Software Foundation, Inc.
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

/* Author: Radey Shouman */

#include "scm.h"

#define GSUBR_MAKTYPE(req, opt, rst) ((req)|((opt)<<4)|((rst)<<8))
#define GSUBR_REQ(x) ((int)(x)&0xf)
#define GSUBR_OPT(x) (((int)(x)&0xf0)>>4)
#define GSUBR_REST(x) ((int)(x)>>8)

#define GSUBR_MAX 10
#define GSUBR_TYPE(cclo) (VELTS(cclo)[1])
#define GSUBR_PROC(cclo) (VELTS(cclo)[2])

static SCM f_gsubr_apply;
SCM make_gsubr(name, req, opt, rst, fcn)
     const char *name;
     int req, opt, rst;
     SCM (*fcn)();
{
  switch GSUBR_MAKTYPE(req, opt, rst) {
  case GSUBR_MAKTYPE(0, 0, 0): return make_subr(name, tc7_subr_0, fcn);
  case GSUBR_MAKTYPE(1, 0, 0): return make_subr(name, tc7_subr_1, fcn);
  case GSUBR_MAKTYPE(0, 1, 0): return make_subr(name, tc7_subr_1o, fcn);
  case GSUBR_MAKTYPE(1, 1, 0): return make_subr(name, tc7_subr_2o, fcn);
  case GSUBR_MAKTYPE(2, 0, 0): return make_subr(name, tc7_subr_2, fcn);
  case GSUBR_MAKTYPE(3, 0, 0): return make_subr(name, tc7_subr_3, fcn);
  case GSUBR_MAKTYPE(0, 0, 1): return make_subr(name, tc7_lsubr, fcn);
  case GSUBR_MAKTYPE(2, 0, 1): return make_subr(name, tc7_lsubr_2, fcn);
  default:
    {
      SCM symcell = sysintern(name, UNDEFINED);
      SCM z = scm_maksubr(name, tc7_subr_0, fcn);
      SCM cclo = makcclo(f_gsubr_apply, 3L);
      ASRTER(GSUBR_MAX >= req + opt + rst, MAKINUM(req + opt + rst),
	     OUTOFRANGE, "make_gsubr");
      GSUBR_PROC(cclo) = z;
      GSUBR_TYPE(cclo) = MAKINUM(GSUBR_MAKTYPE(req, opt, rst));
      CDR(symcell) = cclo;
      return cclo;
    }
  }
}

char s_gsubr_apply[] = " gsubr-apply";
SCM gsubr_apply(args)
     SCM args;
{
  SCM self = CAR(args);
  SCM (*fcn)() = SUBRF(GSUBR_PROC(self));
  int typ = INUM(GSUBR_TYPE(self));
  int i, n = GSUBR_REQ(typ) + GSUBR_OPT(typ) + GSUBR_REST(typ);
  SCM v[10];
  if (n > 10) wta(self, "internal programming error", s_gsubr_apply);
  args = CDR(args);
  for (i = 0; i < GSUBR_REQ(typ); i++) {
#ifndef RECKLESS
    if (IMP(args))
      wnargs: wta(UNDEFINED, (char *)WNA, SNAME(GSUBR_PROC(self)));
#endif
    v[i] = CAR(args);
    args = CDR(args);
  }
  for (; i < GSUBR_REQ(typ) + GSUBR_OPT(typ); i++) {
    if (NIMP(args)) {
      v[i] = CAR(args);
      args = CDR(args);
    }
    else
      v[i] = UNDEFINED;
  }
  if (GSUBR_REST(typ))
    v[i] = args;
  else
    ASRTGO(NULLP(args), wnargs);
  switch (n) {
  case 2: return (*fcn)(v[0], v[1]);
  case 3: return (*fcn)(v[0], v[1], v[2]);
  case 4: return (*fcn)(v[0], v[1], v[2], v[3]);
  case 5: return (*fcn)(v[0], v[1], v[2], v[3], v[4]);
  case 6: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5]);
  case 7: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6]);
  case 8: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7]);
  case 9: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8]);
  case 10: return (*fcn)(v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9]);
  }
}

SCM_DLL_EXPORT void init_gsubr P((void));

void init_gsubr()
{
  f_gsubr_apply = make_subr(s_gsubr_apply, tc7_lsubr, gsubr_apply);
  add_feature("generalized-c-arguments");
}
