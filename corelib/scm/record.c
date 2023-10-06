/* "record.c" code for (R5RS) proposed "Record" user definable datatypes.
 * Copyright (C) 1994, 1995, 1997 Free Software Foundation, Inc.
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
long tc16_record;

/* Record-type-descriptor for record-type-descriptors */
static SCM the_rtd_rtd;

/* Record <= [rtd, elt ... ]
   RTD <= [rtd, name, (field ...), printer]
   Predicate <= [cclo-procedure, rtd]
   Accessor, Modifier <= [cclo-procedure, rtd, index]
   Constructor <= [cclo-procedure, rtd, record-size, #(index ...)] */
#define REC_RTD(x) (VELTS(x)[0])
#define RECP(x) (tc16_record==TYP16(x))
#define RTDP(x) (RECP(x) && the_rtd_rtd==REC_RTD(x))
#define RTD_NAME(x) (VELTS(x)[1])
#define RTD_FIELDS(x) (VELTS(x)[2])
#define RTD_PRINTER(x) (VELTS(x)[3])
#define RCLO_RTD(x) (VELTS(x)[1])
#define RCLO_INDEX(x) (VELTS(x)[2]) /* For accessors, modifiers */
#define RCONSTR_SIZE(x) (VELTS(x)[2])
#define RCONSTR_INDICES(x) (VELTS(x)[3])

/* If we are compiling this as a dll, then we cannot assume that
   arrays will be available when the dll is loaded */
#ifdef ARRAYS
# ifndef DLL
#  define MAKE_REC_INDS(n) make_uve((long)n, MAKINUM(32L))
#  define REC_IND_REF(x, i) VELTS(x)[(i)]
#  define REC_IND_SET(x, i, val) VELTS(x)[(i)] = (val)
# endif
#endif
#ifndef MAKE_REC_INDS
# define MAKE_REC_INDS(n) make_vector(MAKINUM(n), INUM0)
# define REC_IND_REF(x, i) INUM(VELTS(x)[(i)])
# define REC_IND_SET(x, i, val) VELTS(x)[(i)] = MAKINUM(val)
#endif

static char s_record[] = "record";
static char s_recordp[] = "record?";
SCM recordp(obj)
     SCM obj;
{
  return (NIMP(obj) && RECP(obj) ? BOOL_T : BOOL_F);
}
SCM rec_pred1(cclo, obj)
     SCM cclo, obj;
{
  if (NIMP(obj) && RECP(obj) && (REC_RTD(obj)==RCLO_RTD(cclo)))
    return BOOL_T;
  return BOOL_F;
}
static SCM f_rec_pred1;
static char s_rec_pred[] = "record-predicate";
SCM rec_pred(rtd)
     SCM rtd;
{
  SCM cclo = makcclo(f_rec_pred1, 2L);
  ASRTER(NIMP(rtd) && RTDP(rtd), rtd, ARG1, s_rec_pred);
  RCLO_RTD(cclo) = rtd;
  return cclo;
}

static char s_rec_rtd[] = "record-type-descriptor";
SCM rec_rtd(rec)
     SCM rec;
{
  if (IMP(rec) || !RECP(rec)) return BOOL_F;
  return REC_RTD(rec);
}

static SCM f_rec_constr1;
static char s_rec_constr[] = "record-constructor";
SCM rec_constr(rtd, flds)
     SCM rtd, flds;
{
  SCM flst, fld;
  SCM cclo = makcclo(f_rec_constr1, 4L);
  SCM indices;
  sizet i, j;
  ASRTER(NIMP(rtd) && RTDP(rtd), rtd, ARG1, s_rec_constr);
  RCLO_RTD(cclo) = rtd;
  i = ilength(RTD_FIELDS(rtd));
  RCONSTR_SIZE(cclo) = MAKINUM(i);
  if (UNBNDP(flds)) {
    indices = MAKE_REC_INDS(i);
    while (i--) REC_IND_SET(indices, i, i+1);
  }
  else {
    i = ilength(flds);
    ASRTER(i>=0, flds, ARG2, s_rec_constr);
    indices = MAKE_REC_INDS(i);
    for (i = 0; NIMP(flds); i++, flds = CDR(flds)) {
      fld = CAR(flds);
      ASRTER(NIMP(fld) && SYMBOLP(fld), fld, ARG2, s_rec_constr);
      flst = RTD_FIELDS(rtd);
      for (j = 0; ; j++, flst = CDR(flst)) {
	ASRTER(NNULLP(flst), fld, ARG2, s_rec_constr);
	if (fld==CAR(flst)) {
	  REC_IND_SET(indices, i, j+1);
	  break;
	}
      }
    }
  }
  RCONSTR_INDICES(cclo) = indices;
  return cclo;
}
#ifndef RECKLESS
static void rec_error(arg, pos, what, rtd, i)
     SCM arg, rtd;
     int i;
     char *pos, *what;
{
  SCM recname = RTD_NAME(rtd);
  SCM fld = RTD_FIELDS(rtd);
  SCM mesg = makfrom0str(what);
  if (i > 0) {
    while (--i) fld = CDR(fld);
    fld = CAR(fld);
    mesg = st_append(cons2(mesg, recname,
			   cons2(makfrom0str(" -> "), symbol2string(fld), EOL)));
  }
  else
    mesg = st_append(cons2(mesg, recname, EOL));
  wta(arg, pos, CHARS(mesg));
}
#endif
static char s_rec_constr1[] = "record constructor: ";
SCM rec_constr1(args)
     SCM args;
{
  SCM cclo = CAR(args);
  SCM rec, inds = RCONSTR_INDICES(cclo);
  sizet i = INUM(RCONSTR_SIZE(cclo));
  args = CDR(args);
  DEFER_INTS;
  rec = must_malloc_cell((i+1L)*sizeof(SCM),
			 MAKE_NUMDIGS(i+1L, tc16_record), s_record);
  while (i--)
    VELTS(rec)[i+1] = UNSPECIFIED;
  REC_RTD(rec) = RCLO_RTD(cclo);
  ALLOW_INTS;
  for (i = 0; i < LENGTH(inds); i++, args = CDR(args)) {
#ifndef RECKLESS
    if (NULLP(args))
      wna: rec_error(UNDEFINED, WNA, s_rec_constr1, RCLO_RTD(cclo), -1);
#endif
    VELTS(rec)[ REC_IND_REF(inds, i) ] = CAR(args);
  }
  ASRTGO(NULLP(args), wna);
  return rec;
}

/* Makes an accessor or modifier.
   A cclo with 2 env elts -- rtd and field-number. */
static SCM makrecclo(proc, rtd, field, what)
     SCM proc, rtd, field;
     const char *what;
{
  SCM flst;
  SCM cclo = makcclo(proc, 3L);
  int i;
  ASRTER(NIMP(rtd) && RTDP(rtd), rtd, ARG1, what);
  ASRTER(NIMP(field) && SYMBOLP(field), field, ARG2, what);
  RCLO_RTD(cclo) = rtd;
  flst = RTD_FIELDS(rtd);
  for (i = 1; ; i++) {
    ASRTER(NNULLP(flst), field, ARG2, what);
    if (CAR(flst)==field) break;
    flst = CDR(flst);
  }
  RCLO_INDEX(cclo) = MAKINUM(i);
  return cclo;
}
static char s_rec_accessor1[] = "record accessor: ";
SCM rec_accessor1(cclo, rec)
     SCM cclo, rec;
{
  register int i = INUM(RCLO_INDEX(cclo));
#ifndef RECKLESS
  if (IMP(rec) || !RECP(rec) || RCLO_RTD(cclo)!=REC_RTD(rec))
    rec_error(rec, ARG1, s_rec_accessor1, RCLO_RTD(cclo), i);
#endif
  return VELTS(rec)[i];
}
static char s_rec_modifier1[] = "record modifier: ";
SCM rec_modifier1(cclo, rec, val)
     SCM cclo, rec, val;
{
  register int i = INUM(RCLO_INDEX(cclo));
#ifndef RECKLESS
  if (IMP(rec) || !RECP(rec) || RCLO_RTD(cclo)!=REC_RTD(rec))
    rec_error(rec, ARG1, s_rec_modifier1, RCLO_RTD(cclo), i);
#endif
  VELTS(rec)[i] = val;
  return UNSPECIFIED;
}
static SCM f_rec_accessor1;
static char s_rec_accessor[] = "record-accessor";
SCM rec_accessor(rtd, field)
     SCM rtd, field;
{
 return makrecclo(f_rec_accessor1, rtd, field, s_rec_accessor);
}
static SCM f_rec_modifier1;
static char s_rec_modifier[] = "record-modifier";
SCM rec_modifier(rtd, field)
     SCM rtd, field;
{
 return makrecclo(f_rec_modifier1, rtd, field, s_rec_accessor);
}
SCM *loc_makrtd;
static char s_makrectyp[] = "make-record-type";
SCM makrectyp(name, fields)
     SCM name, fields;
{
  SCM n, argv[2];
#ifndef RECKLESS
  if (ilength(fields) < 0)
  errout: wta(fields, (char *)ARG2, s_makrectyp);
  for (n = fields; NIMP(n); n = CDR(n))
    if (!SYMBOLP(CAR(n))) goto errout;
#endif
  argv[0] = name;
  argv[1] = fields;
  return scm_cvapply(*loc_makrtd, 2L, argv);
}

static char s_rec_prinset[] = "record-printer-set!";
SCM rec_prinset(rtd, printer)
     SCM rtd, printer;
{
  ASRTER(NIMP(rtd) && RTDP(rtd), rtd, ARG1, s_rec_prinset);
  ASRTER(BOOL_F==printer || scm_arity_check(printer, 3L, (char *)0),
	 printer, ARG2, s_rec_prinset);
  RTD_PRINTER(rtd) = printer;
  return UNSPECIFIED;
}

static SCM markrec(ptr)
     SCM ptr;
{
  sizet i;
  for (i = NUMDIGS(ptr); --i;)
    if (NIMP(VELTS(ptr)[i])) gc_mark(VELTS(ptr)[i]);
  return REC_RTD(ptr);
}
static sizet freerec(ptr)
     CELLPTR ptr;
{
  must_free(CHARS((SCM)ptr), sizeof(SCM)*NUMDIGS((SCM)ptr));
  return 0;
}
static int recprin1(exp, port, writing)
     SCM exp, port;
     int writing;
{
  SCM names, printer = RTD_PRINTER(REC_RTD(exp));
  SCM argv[3];
  if (NIMP(printer)) {
    argv[0] = exp;
    argv[1] = port;
    argv[2] = writing ? BOOL_T : BOOL_F;
    /* A writing value of 2 means we are printing an error message.
       An error in a record printer at this time will result in a
       fatal recursive error. */
    if (2 != writing) {
      if (NFALSEP(scm_cvapply(printer, 3L, argv)))
	return 1;
    }
    else {
      lputs("\n; Ignoring record-printer: ", cur_errp);
    }
  }
  names = RTD_FIELDS(REC_RTD(exp));
  lputs("#s(", port);
  scm_iprin1(RTD_NAME(REC_RTD(exp)), port, 0);
  if (writing) {
    lputc(':', port);
    scm_intprint(((long)(exp))>>1, 16, port);
  }
#ifdef SCM_SHOW_RECORD_FIELDS
  {
    sizet i;
    for (i = 1; i < NUMDIGS(exp); i++) {
      lputc(' ', port);
      scm_iprin1(CAR(names), port, 0);
      names = CDR(names);
      lputc(' ', port);
      scm_iprin1(VELTS(exp)[i], port, writing);
    }}
#endif
  lputc(')', port);
  return 1;
}

static SCM f_rtdprin1;
SCM rec_rtdprin1(rtd, port, writing_p)
     SCM rtd, port, writing_p;
{
  lputs("#s(record-type ", port);
  scm_iprin1(RTD_NAME(rtd), port, 0);
  lputc(':', port);
  scm_intprint(((long)rtd)>>1, 16, port);
  lputs(" fields ", port);
  scm_iprin1(RTD_FIELDS(rtd), port, 0);
  if (NIMP(RTD_PRINTER(rtd)))
    lputs(" P)", port);
  else
    lputc(')', port);
  return BOOL_T;
}

SCM recequal(rec0, rec1)
     SCM rec0, rec1;
{
  sizet i = NUMDIGS(rec0);
  if (i != NUMDIGS(rec1)) return BOOL_F;
  if (REC_RTD(rec0) != REC_RTD(rec1)) return BOOL_F;
  while(--i)
    if (FALSEP(equal(VELTS(rec0)[i], VELTS(rec1)[i])))
      return BOOL_F;
  return BOOL_T;
}
static smobfuns recsmob = {markrec, freerec, recprin1, recequal};
static iproc subr1s[] = {
  {s_recordp, recordp},
  {s_rec_pred, rec_pred},
  {s_rec_rtd, rec_rtd},
  {0, 0}};
static iproc subr2s[] = {
  {s_rec_accessor, rec_accessor},
  {s_rec_modifier, rec_modifier},
  {s_makrectyp, makrectyp},
  {s_rec_prinset, rec_prinset},
  {0, 0}};

SCM_DLL_EXPORT void init_record P((void));

void init_record()
{
  SCM i_name = CAR(sysintern("name", UNDEFINED));
  SCM i_fields = CAR(sysintern("fields", UNDEFINED));
  SCM i_printer = CAR(sysintern("printer", UNDEFINED));
  SCM the_rtd, rtd_name = makfrom0str("record-type");
  SCM rtd_fields = cons2(i_name, i_fields, cons(i_printer, EOL));
  tc16_record = newsmob(&recsmob);
  f_rtdprin1 = make_subr(" rtdprin1", tc7_subr_3, rec_rtdprin1);
  DEFER_INTS;
  the_rtd = must_malloc_cell(4L * sizeof(SCM),
		 MAKE_NUMDIGS(4L, tc16_record), s_record);
  REC_RTD(the_rtd) = the_rtd;
  RTD_NAME(the_rtd) = rtd_name;
  RTD_FIELDS(the_rtd) = rtd_fields;
  RTD_PRINTER(the_rtd) = f_rtdprin1;
  ALLOW_INTS;
  the_rtd_rtd = the_rtd;	/* Protected by make-record-type */
  f_rec_pred1 = make_subr(" record-predicate-procedure", tc7_subr_2, rec_pred1);
  f_rec_constr1 = make_subr(s_rec_constr1, tc7_lsubr, rec_constr1);
  f_rec_accessor1 = make_subr(s_rec_accessor1, tc7_subr_2, rec_accessor1);
  f_rec_modifier1 = make_subr(s_rec_modifier1, tc7_subr_3, rec_modifier1);
  make_subr(s_rec_constr, tc7_subr_2o, rec_constr);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);
  sysintern("record-type-descriptor?", rec_pred(the_rtd_rtd));
  sysintern("record-type-name", rec_accessor(the_rtd_rtd, i_name));
  sysintern("record-type-field-names", rec_accessor(the_rtd_rtd, i_fields));
  loc_makrtd = &CDR(sysintern("RTD:make",
			      rec_constr(the_rtd_rtd, cons2(i_name, i_fields, EOL))));
  add_feature(s_record);
}
