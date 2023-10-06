/* "scmhob.h" is a header file for scheme source compiled with hobbit5x
 * Copyright (C) 1992, 1993, 1994, 1995, 1996, 1997 Free Software Foundation
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

/* Author: Tanel Tammet */

#include "scm.h"

#define STBL_VECTOR_SET(v,k,o) (v[((long)INUM(k))] = o)
#define STBL_VECTOR_REF(v,k) (v[((long)INUM(k))])
#define CHAR_LESSP(x,y) ((ICHR(x) < ICHR(y)) ? BOOL_T : BOOL_F)
#define CHAR_LEQP(x,y) ((ICHR(x) <= ICHR(y)) ? BOOL_T : BOOL_F)
#define CHCI_EQ(x,y) ((upcase[ICHR(x)]==upcase[ICHR(y)]) ? BOOL_T : BOOL_F)
#define CHCI_LESSP(x,y) ((upcase[ICHR(x)] < upcase[ICHR(y)]) ? BOOL_T : BOOL_F)
#define CHCI_LEQP(x,y) ((upcase[ICHR(x)] <= upcase[ICHR(y)]) ? BOOL_T : BOOL_F)
#define CHAR_ALPHAP(chr) ((isascii(ICHR(chr)) && isalpha(ICHR(chr))) ? BOOL_T : BOOL_F)
#define CHAR_NUMP(chr) ((isascii(ICHR(chr)) && isdigit(ICHR(chr))) ? BOOL_T : BOOL_F)
#define CHAR_WHITEP(chr) ((isascii(ICHR(chr)) && isspace(ICHR(chr))) ? BOOL_T : BOOL_F)
#define CHAR_UPPERP(chr) ((isascii(ICHR(chr)) && isupper(ICHR(chr))) ? BOOL_T : BOOL_F)
#define CHAR_LOWERP(chr) ((isascii(ICHR(chr)) && islower(ICHR(chr))) ? BOOL_T : BOOL_F)
#define CHAR2INT(chr) MAKINUM(ICHR(chr))
#define INT2CHAR(n) MAKICHR(INUM(n))
#define CHAR_UPCASE(chr) MAKICHR(upcase[ICHR(chr)])
#define CHAR_DOWNCASE(chr) MAKICHR(downcase[ICHR(chr)])
#define ST_LENGTH(str) MAKINUM(LENGTH(str))
#define ST_REF(str,k) MAKICHR(CHARS(str)[INUM(k)])
#define VECTOR_LENGTH(v)  MAKINUM(LENGTH(v))

#ifdef FLOATS
# include <math.h>
#else
# define scm_abs scm_iabs
#endif
#ifdef BIGDIG
# define PRE_TRANSC_FUN(x) (INUMP(x) ? (double) INUM(x) : (REALP(x) ? (double) REALPART(x) : (double) int2dbl(x)))
#else
# define PRE_TRANSC_FUN(x) (INUMP(x) ?  (double) INUM(x) : (double) REALPART(x))
#endif

#define SIN_FUN(x) (makdbl( sin( PRE_TRANSC_FUN(x)), 0.0))
#define COS_FUN(x) (makdbl( cos( PRE_TRANSC_FUN(x)), 0.0))
#define TAN_FUN(x) (makdbl( tan( PRE_TRANSC_FUN(x)), 0.0))
#define ASIN_FUN(x) (makdbl( asin( PRE_TRANSC_FUN(x)), 0.0))
#define ACOS_FUN(x) (makdbl( acos( PRE_TRANSC_FUN(x)), 0.0))
#define ATAN_FUN(x) (makdbl( atan( PRE_TRANSC_FUN(x)), 0.0))
#define SINH_FUN(x) (makdbl( sinh( PRE_TRANSC_FUN(x)), 0.0))
#define COSH_FUN(x) (makdbl( cosh( PRE_TRANSC_FUN(x)), 0.0))
#define TANH_FUN(x) (makdbl( tanh( PRE_TRANSC_FUN(x)), 0.0))
#define ASINH_FUN(x) (makdbl( asinh( PRE_TRANSC_FUN(x)), 0.0))
#define ACOSH_FUN(x) (makdbl( acosh( PRE_TRANSC_FUN(x)), 0.0))
#define ATANH_FUN(x) (makdbl( atanh( PRE_TRANSC_FUN(x)), 0.0))
#define SQRT_FUN(x) (makdbl( sqrt( PRE_TRANSC_FUN(x)), 0.0))
#define EXPT_FUN(x,y) (makdbl( pow(( PRE_TRANSC_FUN(x)), ( PRE_TRANSC_FUN(y))), 0.0))
#define EXP_FUN(x) (makdbl( exp( PRE_TRANSC_FUN(x)), 0.0))
#define LOG_FUN(x) (makdbl( log( PRE_TRANSC_FUN(x)), 0.0))
#define ABS_FUN(x) (makdbl( fabs( PRE_TRANSC_FUN(x)), 0.0))
#define EX2IN_FUN(x) (makdbl( PRE_TRANSC_FUN(x), 0.0))
#define FLOOR_FUN(x) (makdbl( floor( PRE_TRANSC_FUN(x)), 0.0))
#define CEILING_FUN(x) (makdbl( ceil( PRE_TRANSC_FUN(x)), 0.0))
#define TRUNCATE_FUN(x) (makdbl( ltrunc( PRE_TRANSC_FUN(x)), 0.0))
#define ROUND_FUN(x) (makdbl(round( PRE_TRANSC_FUN(x)), 0.0))

/* the following defs come from the #ifdef HOBBIT part of scm.h */

#define SBOOL(x) ((x) ? BOOL_T : BOOL_F)

#define BOOLEAN_P(x) ((x)==BOOL_T || (x)==BOOL_F)
#define CHAR_P ICHRP
#define SYMBOL_P(x) (ISYMP(x) || (!(IMP(x)) && SYMBOLP(x)))
#define VECTOR_P(x) (!(IMP(x)) && VECTORP(x))
#define PAIR_P(x) (!(IMP(x)) && CONSP(x))
#define NUMBER_P INUMP
#define INTEGER_P INUMP
#define STRING_P(x) (!(IMP(x)) && STRINGP(x))
#define NULL_P NULLP
#define ZERO_P(x) ((x)==INUM0)
#define POSITIVE_P(x) ((x) > INUM0)
#define NEGATIVE_P(x) ((x) < INUM0)

#define NOT(x) ((x)==BOOL_F ? BOOL_T : BOOL_F)
#define SET_CAR(x,y) (CAR(x) = (SCM)(y))
#define SET_CDR(x,y) (CDR(x) = (SCM)(y))
#define VECTOR_SET(v,k,o) (VELTS(v)[((long)INUM(k))] = o)
#define VECTOR_REF(v,k) (VELTS(v)[((long)INUM(k))])
#define GLOBAL(x) (*(x))

#define append2(lst1,lst2) (append(cons2(lst1,lst2,EOL)))
#define procedure_pred_(x) (BOOL_T==procedurep(x))

/* new for hobbit5 - scm5 */

/*
SCM intp(SCM);
SCM eqv(SCM,SCM);
*/
