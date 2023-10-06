/* "get-contoffset-ia64.c" Program writes "contoffset-ia64.S" with C offsets.
 * Copyright (C) 2006 Free Software Foundation, Inc.
 *
 * By including the following notice, I am agreeing to its terms,
 * including the special exception for SCM.
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

/* Author: Richard E. Harke
 *
 * The .o of this file does not get linked into SCM.
 * It is a utility used to create an include file
 * for continue-ia64.S to get offsets into struct jump_buf
 * and be sure they are correct for the current compiler.
 *
 * create get-contoffset-ia64 program by:
 *   gcc -o get-contoffset-ia64 get-contoffset-ia64.c
 * create ASM include file by:
 *   ./get-contoffset-ia64 contoffset-ia64.S
 */
#include <stddef.h>
#include <stdio.h>

#define IN_CONTINUE_C
#include "setjump.h"

int
main(int argc, char ** argv)
{
  struct Continuation taco;
  long jmpbuf_off = 10L;
  long thrwval_off;
  long length_off;
  long stkbse_off;
  long bspbse_off;
  long bsplength_off;
  long rnat_off;
  long other_off;
  long parent_off;
  long cont_size;
  FILE *prt;

  switch (argc) {
  case 1: prt = stdout; break;
  case 2: prt = fopen(argv[1], "w"); break;
  default: return !0;
  }
  jmpbuf_off = &((struct Continuation *)0)->jmpbuf;
  thrwval_off = &((struct Continuation *)0)->thrwval;
  length_off = &((struct Continuation *)0)->length;
  stkbse_off = &((struct Continuation *)0)->stkbse;
  bspbse_off = &((struct Continuation *)0)->bspbse;
  bsplength_off = &((struct Continuation *)0)->bsplength;
  rnat_off = &((struct Continuation *)0)->rnat;
  other_off = &((struct Continuation *)0)->other;
  parent_off = &((struct Continuation *)0)->parent;
  cont_size = sizeof(struct Continuation);

  fprintf(prt, "        jmpbuf_off = %ld\n", jmpbuf_off);
  fprintf(prt, "        thrwval_off = %ld\n", thrwval_off);
  fprintf(prt, "        length_off = %ld\n", length_off);
  fprintf(prt, "        stkbse_off = %ld\n", stkbse_off);
  fprintf(prt, "        bspbse_off = %ld\n", bspbse_off);
  fprintf(prt, "        bsplength_off = %ld\n", bsplength_off);
  fprintf(prt, "        rnat_off = %ld\n", rnat_off);
  fprintf(prt, "        other_off = %ld\n", other_off);
  fprintf(prt, "        parent_off = %ld\n", parent_off);
  fprintf(prt, "        cont_size = %ld\n", cont_size);
  fclose(prt);
  return 0;
}
