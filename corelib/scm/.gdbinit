# ".gdbinit" GDB 4.16 initialization for SCM Scheme Interpreter
# Copyright (C) 1990, 1991, 1992, 1993, 1994, 1995, 1996 Free Software Foundation, Inc.
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public
# License along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

# Author: Aubrey Jaffer

set output-radix 16.

define runscm
  break abrt
  echo \nto return to gdb, type: (abort)\n\n
  run
end

define verbose
  output (prolixity(((($arg0)<<1)<<1)+2L))>>2
  echo\n
end

define errobj
  call scm_iprin1(*loc_errobj, sys_protects[2], 1),(void)0
  call scm_newline(sys_protects[2]),(void)0
end

define scm
  call scm_iprin1($arg0, sys_protects[2], 1),(void)0
  call scm_newline(sys_protects[2]),(void)0
end

define code
  call scm_princode($arg0, scm_estk_ptr[2], sys_protects[2], 1),(void)0
  call scm_newline(sys_protects[2]),(void)0
end

define lload
  if (errjmp_bad)
    echo sorry, errjmp_bad\n
  else
    call scm_ldfile($arg0),(void)0
  end
end

define eval
  if (errjmp_bad)
    echo sorry, errjmp_bad\n
  else
    print scm_evstr($arg0)
    scm $
  end
end

define car
  print ((cell*)(~1L & $))->car
end
document car
CAR of $
end

define cdr
  print ((cell*)(~1L & $))->cdr
end
document cdr
CDR of $
end

define disp
  call scm_iprin1($arg0, sys_protects[2], 0)
  echo \n
end

define writ
  call scm_iprin1($arg0, sys_protects[2], 1)
  echo \n
end




