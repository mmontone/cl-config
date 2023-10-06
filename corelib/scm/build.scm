;; "build.scm" Build database and program	-*-scheme-*-
;; Copyright (C) 1994-2006 Free Software Foundation, Inc.
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

(require 'parameters)
(require 'databases)
(require 'database-commands)
(require 'alist)
(require 'common-list-functions)
(require 'object->string)
(require 'filename)
(require 'batch)
(require-if 'compiling 'alist-table)
(require-if 'compiling 'posix-time)
;@
(define OPEN_WRITE "w")			; Because MS-DOS scripts need ^M
;@
(define build (add-command-tables (create-database #f 'alist-table)))

(batch:initialize! build)
(((open-table! build 'batch-dialect) 'row:insert)
 '(default-for-platform 0))

;;;; This first part is about SCM files and features.

(define-tables build

  '(file-formats
    ((format symbol))
    ()
    ((plaintext)
     (c-source)
     (c-header)
     (scheme)
     (vax-asm)
     (gnu-as)
     (gdb-init)
     (cray-asm)
     (makefile)
     (MS-DOS-batch)
     (nroff)
     (texinfo)))

  '(file-categories
    ((category symbol))
    ((documentation string))
    ((documentation "documentation")
     (platform-specific "required for certain platforms")
     (core "core for building executable SCM")
     (optional "required for some feature")
     (linkable "can be statically or dynamically linked for some feature")
     (test "test SCM")
     (none "no files")))

  '(manifest
    ((file string)
     (format file-formats)
     (category file-categories))
    ((documentation string))
    (("README"	plaintext	documentation	"contains a MANIFEST, INSTALLATION INSTRUCTIONS, hints for EDITING SCHEME CODE, and a TROUBLE SHOOTING GUIDE.")
     ("COPYING"	plaintext	documentation	"GNU GENERAL PUBLIC LICENSE")
     ("COPYING.LESSER"	plaintext	documentation	"GNU LESSER GENERAL PUBLIC LICENSE")
     ("scm.1"	nroff	documentation	"unix style man page.")
     ("scm.doc"	plaintext	documentation	"man page generated from scm.1.")
     ("QUICKREF"	plaintext	documentation	"Quick Reference card for R4RS and IEEE Scheme.")
     ("scm.texi"	Texinfo	documentation	"SCM installation and use.")
     ("fdl.texi"	Texinfo documentation	"GNU Free Documentation License.")
     ("ChangeLog"	plaintext	documentation	"changes to SCM.")
     ("r4rstest.scm"	Scheme	test	"tests conformance with Scheme specifications.")
     ("example.scm"	Scheme	test	"example from R4RS which uses inexact numbers.")
     ("pi.scm"	Scheme	test	"computes digits of pi [type (pi 100 5)].  Test performance against pi.c.")
     ("pi.c"	c-source	test	"computes digits of pi [cc -o pi pi.c;time pi 100 5].")
     ("bench.scm"	Scheme	test	"computes and records performance statistics of pi.scm.")
     ("Makefile"	Makefile	core	"builds SCMLIT using the `make' program.")
     ("build.scm"	Scheme	core	"database for compiling and linking new SCM programs.")
     ("build.bat"	MS-DOS-batch	platform-specific	"invokes build.scm for MS-DOS")
     ("mkimpcat.scm"	Scheme	core	"build SCM-specific catalog for SLIB.")
     (".gdbinit"	gdb-init	optional "provides commands for debugging SCM with GDB")
     ("setjump.mar"	Vax-asm	platform-specific	"provides setjump and longjump which do not use $unwind utility on VMS.")
     ("ugsetjump.s"	gnu-as	platform-specific	"provides setjump and longjump which work on Ultrix VAX.")
     ("setjump.s"	Cray-asm	platform-specific	"provides setjump and longjump for the Cray YMP.")
     ("continue-ia64.S"	gnu-as	platform-specific "replaces make_root_continuation(), make_continuation(), and dynthrow() in continue.c")
     ("get-contoffset-ia64.c"	c-source	platform-specific	"makes contoffset-ia64.S for inclusion by continue-ia64.S")
     ("Init.scm"	Scheme	core	"Scheme initialization.")
     ("Transcen.scm"	Scheme	core	"inexact builtin procedures.")
     ("Link.scm"	Scheme	core	"Dynamic link/loading.")
     ("compile.scm"	Scheme	core	"Hobbit compilation to C.")
     ("Macro.scm"	Scheme	core	"Supports Syntax-Rules Macros.")
     ("scmfig.h"	c-header	core	"contains system dependent definitions.")
     ("patchlvl.h"	c-header	core	"patchlevel of this release.")
     ("setjump.h"	c-header	core	"continuations, stacks, and memory allocation.")
     ("continue.h"	c-header	core	"continuations.")
     ("continue.c"	c-source	core	"continuations.")
     ("scm.h"	c-header	core	"data type and external definitions of SCM.")
     ("scm.c"	c-source	core	"initialization, interrupts, and non-IEEE utility functions.")
     ("scmmain.c"	c-source	core	"initialization, interrupts, and non-IEEE utility functions.")
     ("findexec.c"	c-source	core	"find the executable file function.")
     ("script.c"	c-source	core	"utilities for running as `#!' script.")
     ("time.c"	c-source	core	"functions dealing with time.")
     ("repl.c"	c-source	core	"error, read-eval-print loop, read, write and load.")
     ("scl.c"	c-source	core	"inexact arithmetic")
     ("eval.c"	c-source	core	"evaluator, apply, map, and foreach.")
     ("sys.c"	c-source	core	"call-with-current-continuation, opening and closing files, storage allocation and garbage collection.")
     ("subr.c"	c-source	core	"the rest of IEEE functions.")
     ("debug.c" c-source	core	"debugging, printing code.")
     ("unif.c"	c-source	core	"uniform vectors.")
     ("rope.c"	c-source	core	"C interface functions.")
     ("ramap.c"	c-source	optional	"array mapping")
     ("dynl.c"	c-source	optional	"dynamically load object files.")
     ("sc2.c"	c-source	linkable	"procedures from R2RS and R3RS not in R4RS.")
     ("byte.c"	c-source	linkable	"strings as bytes.")
     ("rgx.c"	c-source	linkable	"string regular expression match.")
     ("crs.c"	c-source	linkable	"interactive terminal control.")
     ("split.scm"	Scheme	test	"example use of crs.c.  Input, output, and diagnostic output directed to separate windows.")
     ("edline.c"	c-source	linkable	"Gnu readline input editing (get ftp.sys.toronto.edu:/pub/rc/editline.shar).")
     ("Iedline.scm"	Scheme	optional	"Gnu readline input editing.")
     ("bytenumb.c"	c-source	linkable	"Byte-number conversions.")
     ("differ.c"	c-source	linkable	"Linear-space O(PN) sequence comparison.")
     ("Idiffer.scm"	Scheme	optional	"Linear-space O(PN) sequence comparison.")
     ("record.c"	c-source	linkable	"proposed `Record' user definable datatypes.")
     ("gsubr.c"	c-source	linkable	"make_gsubr for arbitrary (< 11) arguments to C functions.")
     ("ioext.c"	c-source	linkable	"system calls in common between PC compilers and unix.")
     ("posix.c"	c-source	linkable	"posix library interface.")
     ("unix.c"	c-source	linkable	"non-posix system calls on unix systems.")
     ("socket.c"	c-source	linkable	"BSD socket interface.")
     ("pre-crt0.c"	c-source	platform-specific	"loaded before crt0.o on machines which do not remap part of the data space into text space in unexec.")
     ("ecrt0.c"	c-source	platform-specific	"discover the start of initialized data space dynamically at runtime.")
     ("gmalloc.c"	c-source	platform-specific	"Gnu malloc(); used for unexec.")
     ("unexec.c"	c-source	platform-specific	"Convert a running program into an executable file.")
     ("unexhp9k800.c"	c-source	platform-specific	"Convert a running HP-UX program into an executable file.")
     ("unexelf.c"	c-source	platform-specific	"Convert a running ELF program into an executable file.")
     ("unexalpha.c"	c-source	platform-specific	"Convert a running program into an Alpha executable file.")
     ("unexsgi.c"	c-source	platform-specific	"Convert a running program into an IRIX executable file.")
     ("unexsunos4.c"	c-source	platform-specific	"Convert a running program into an executable file.")
     ("macosx-config.h"	c-header	platform-specific	"Included by unexmacosx.c and lastfile.c.")
     ("unexmacosx.c"	c-source	platform-specific	"Convert a running program into an executable file under MacOS X.")
     ("lastfile.c"	c-source	platform-specific	"find the point in data space between data and libraries.")
     ))

  '(build-whats
    ((name symbol))
    ((class file-categories)
     (c-proc symbol)
     (o-proc symbol)
     (spec expression)
     (documentation string))
    ((exe core compile-c-files link-c-program #f
	  "executable program")
     (lib core compile-c-files make-archive ((c-lib lib))
	  "library module")
     (dlls linkable compile-dll-c-files make-dll-archive ((define "DLL"))
	   "archived dynamically linked library object files")
     (dll none compile-dll-c-files update-catalog ((define "DLL"))
	  "dynamically linked library object file")))

  '(features
    ((name symbol))
    ((spec expression)
     (documentation string))
    ((none () "No features"))))

(define-domains build
  '(optstring #f (lambda (x) (or (not x) (string? x))) string #f)
  '(filename #f #f string #f)
  '(features features #f symbol #f)
  '(build-whats build-whats #f symbol #f))

(define define-build-feature
  (let ((defeature ((open-table! build 'features) 'row:insert)))
    (lambda args
      (defeature (append args (list (comment)))))))

#;Lightweight -- no features
(define-build-feature
  'lit
  '())

#;Normally, the number of arguments arguments to interpreted closures
#;(from LAMBDA) are checked if the function part of a form is not a
#;symbol or only the first time the form is executed if the function
#;part is a symbol.  defining @samp{reckless} disables any checking.
#;If you want to have SCM always check the number of arguments to
#;interpreted closures define feature @samp{cautious}.
(define-build-feature
 'cautious
 '((define "CAUTIOUS")))

#;Define this for extra checking of interrupt masking and some simple
#;checks for proper use of malloc and free.  This is for debugging C
#;code in @file{sys.c}, @file{eval.c}, @file{repl.c} and makes the
#;interpreter several times slower than usual.
(define-build-feature
 'careful-interrupt-masking
 '((define "CAREFUL_INTS")))

#;Turns on the features @samp{cautious} and
#;@samp{careful-interrupt-masking}; uses
#;@code{-g} flags for debugging SCM source code.
(define-build-feature
 'debug
 '((c-lib debug) (features cautious careful-interrupt-masking)))

#;If your scheme code runs without any errors you can disable almost
#;all error checking by compiling all files with @samp{reckless}.
(define-build-feature
 'reckless
 '((define "RECKLESS")))

#;C level support for hygienic and referentially transparent macros
#;(syntax-rules macros).
(define-build-feature
 'macro
 '((define "MACRO") (features rev2-procedures record)))

#;Large precision integers.
(define-build-feature
 'bignums
 '((define "BIGNUMS")))

#;Use if you want arrays, uniform-arrays and uniform-vectors.
(define-build-feature
 'arrays
 '((define "ARRAYS")))

#;Alias for ARRAYS
(define-build-feature
 'array
 '((features arrays)))

#;array-map! and array-for-each (arrays must also be featured).
(define-build-feature
 'array-for-each
 '((c-file "ramap.c") (compiled-init "init_ramap")))

#;Use if you want floating point numbers.
(define-build-feature
 'inexact
 '((features bignums) (define "FLOATS") (c-lib m)))

#;Use if you want floats to display in engineering notation (exponents
#;always multiples of 3) instead of scientific notation.
(define-build-feature
 'engineering-notation
 '((define "ENGNOT")))

#;Use if you want all inexact real numbers to be single precision.  This
#;only has an effect if SINGLES is also defined (which is the default).
#;This does not affect complex numbers.
(define-build-feature
 'single-precision-only
 '((define "SINGLESONLY")))

#;Use if you want to run code from:
#;
#;@cindex SICP
#;Harold Abelson and Gerald Jay Sussman with Julie Sussman.
#;@cite{Structure and Interpretation of Computer Programs.}
#;The MIT Press, Cambridge, Massachusetts, USA, 1985.
#;
#;Differences from R5RS are:
#;@itemize @bullet
#;@item
#;(eq? '() '#f)
#;@item
#;(define a 25) returns the symbol a.
#;@item
#;(set! a 36) returns 36.
#;@end itemize
(define-build-feature
 'sicp
 '((define "SICP")))

#;These procedures were specified in the @cite{Revised^2 Report on Scheme}
#;but not in @cite{R4RS}.
(define-build-feature
 'rev2-procedures
 '((c-file "sc2.c") (init "init_sc2")))

#;Treating strings as byte-vectors.
(define-build-feature
 'byte
 '((c-file "byte.c") (init "init_byte")))

#;The Record package provides a facility for user to define their own
#;record data types.  See SLIB for documentation.
(define-build-feature
 'record
 '((define "CCLO") (c-file "record.c") (compiled-init "init_record")))

#;Use if you want to use compiled closures.
(define-build-feature
 'compiled-closure
 '((define "CCLO")))

#;@code{make_gsubr} for arbitrary (< 11) arguments to C functions.
(define-build-feature
 'generalized-c-arguments
 '((c-file "gsubr.c") (compiled-init "init_gsubr")))

#;Use if you want the ticks and ticks-interrupt functions.
(define-build-feature
 'tick-interrupts
 '((define "TICKS")))

#;Commonly available I/O extensions: @dfn{exec}, line I/O, file
#;positioning, file delete and rename, and directory functions.
(define-build-feature
 'i/o-extensions
 '((c-file "ioext.c") (init "init_ioext")))

#;@dfn{Turtle} graphics calls for both Borland-C and X11 from
#;sjm@@ee.tut.fi.
(define-build-feature
 'turtlegr
 '((c-file "turtlegr.c") (c-lib graphics) (features inexact)
   (compiled-init "init_turtlegr")))

#;Interface to Xlib graphics routines.
(define-build-feature
 'Xlib
 '((c-file "x.c") (c-lib graphics) (compiled-init "init_x") (features arrays)))

#;Alias for Xlib feature.
(define-build-feature
 'X
 '((features Xlib)))

#;For the @dfn{curses} screen management package.
(define-build-feature
 'curses
 '((c-file "crs.c") (c-lib curses) (compiled-init "init_crs")))

#;interface to the editline or GNU readline library.
(define-build-feature
 'edit-line
 '((c-file "edline.c") (c-lib termcap editline) (compiled-init "init_edline")))

#;Client connections to the mysql databases.
(define-build-feature
 'mysql
 '((c-file "database.c") (c-lib mysql) (compiled-init "init_database")))

#;String regular expression matching.
(define-build-feature
 'regex
 '((c-file "rgx.c") (c-lib regex) (compiled-init "init_rgx")))

#;BSD @dfn{socket} interface.  Socket addr functions require
#;inexacts or bignums for 32-bit precision.
(define-build-feature
 'socket
 '((c-lib socket) (c-file "socket.c") (compiled-init "init_socket")))

#;Posix functions available on all @dfn{Unix-like} systems.  fork and
#;process functions, user and group IDs, file permissions, and
#;@dfn{link}.
(define-build-feature
 'posix
 '((c-file "posix.c") (compiled-init "init_posix")))

#;Those unix features which have not made it into the Posix specs:
#;nice, acct, lstat, readlink, symlink, mknod and sync.
(define-build-feature
 'unix
 '((c-file "unix.c") (compiled-init "init_unix")))

#;Sequence comparison
(define-build-feature
 'differ
 '((c-file "differ.c") (compiled-init "init_differ")))

#;Byte/number conversions
(define-build-feature
 'byte-number
 '((c-file "bytenumb.c") (compiled-init "init_bytenumb")))

#;Microsoft Windows executable.
(define-build-feature
 'windows
 '((c-lib windows)))			; (define "NON_PREEMPTIVE")

#;Be able to load compiled files while running.
(define-build-feature
 'dynamic-linking
 '((c-file "dynl.c") (c-lib dlll)))

#;Convert a running scheme program into an executable file.
(define-build-feature
 'dump
 '((define "CAN_DUMP") (c-lib dump) (c-lib nostart)))

;;; Descriptions of these parameters is in "setjump.h".
;;	(initial-heap-size ((define "INIT_HEAP_SIZE" (* 25000 sizeof-cell))))
;;	(heap-segment-size ((define "HEAP_SEG_SIZE" (* 8100 sizeof-cell))))
;;	(short-aligned-stack ((define "SHORT_ALIGN")))
;;	(initial-malloc-limit ((define "INIT_MALLOC_LIMIT" 100000)))
;;	(number-of-hash-buckets ((define "NUM_HASH_BUCKETS" 137)))
;;	(minimum-gc-yield ((define "MIN_GC_YIELD" "(heap_cells/4)")))

#;Use if you want segments of unused heap to not be freed up after
#;garbage collection.  This may increase time in GC for *very* large
#;working sets.
(define-build-feature
 'no-heap-shrink
 '((define "DONT_GC_FREE_SEGMENTS")))

#;SCM normally converts references to local variables to ILOCs, which
#;make programs run faster.  If SCM is badly broken, try using this
#;option to disable the MEMOIZE_LOCALS feature.
(define-build-feature
 'dont-memoize-locals
 '((define "DONT_MEMOIZE_LOCALS")))

#;If you only need straight stack continuations, executables compile with
#;this feature will run faster and use less storage than not having it.
#;Machines with unusual stacks @emph{need} this.  Also, if you incorporate
#;new C code into scm which uses VMS system services or library routines
#;(which need to unwind the stack in an ordrly manner) you may need to
#;use this feature.
(define-build-feature
 'cheap-continuations
 '((define "CHEAP_CONTINUATIONS")))

#;WB database with relational wrapper.
(define-build-feature
  'wb
  '((c-file
     "../wb/c/blink.c" "../wb/c/blkio.c" "../wb/c/del.c" "../wb/c/ents.c"
     "../wb/c/handle.c" "../wb/c/prev.c" "../wb/c/scan.c" "../wb/c/segs.c"
     "../wb/c/stats.c" "../wb/c/wbsys.c" "../wb/c/wbscm.c")
    (c-lib pthread)
    (scm-srcdir "../scm")
    (compiled-init "init_db")))
(define-build-feature
  'wb-no-threads
  '((c-file
     "../wb/c/blink.c" "../wb/c/blkio.c" "../wb/c/del.c" "../wb/c/ents.c"
     "../wb/c/handle.c" "../wb/c/prev.c" "../wb/c/scan.c" "../wb/c/segs.c"
     "../wb/c/stats.c" "../wb/c/wbsys.c" "../wb/c/wbscm.c")
    (scm-srcdir "../scm")
    (compiled-init "init_db")))

;;;; The rest is about building on specific platforms.

(define-tables build

  '(processor-family
    ((family symbol))
    ((also-runs processor-family))
    ((*unknown* #f)
     (i8086 #f)
     (ia64 #f)
     (acorn #f)
     (alpha #f)
     (cray #f)
     (hp-risc #f)
     (i386 i8086)
     (m68000 #f)
     (m68030 m68000)
     (mips #f)
     (nos/ve #f)
     (pdp-10 #f)
     (pdp-11 #f)
     (pdp-8 #f)
     (powerpc #f)
     (pyramid #f)
     (sequent #f)
     (sparc #f)
     (tahoe #f)
     (vax pdp-11)
     ))

  '(platform
    ((name symbol))
    ((processor processor-family)
     (operating-system operating-system)
     (compiler symbol)
     ;;(linker symbol)
     )
    ((*unknown*		 *unknown* unix	     cc	       ) ;ld
     (acorn-unixlib	 acorn	   *unknown* cc	       ) ;link
     (aix		 powerpc   aix	     cc	       ) ;cc
     (osf1		 alpha	   unix	     cc	       ) ;cc
     (alpha-elf		 alpha	   unix	     cc	       ) ;cc
     (alpha-linux	 alpha	   linux     gcc       ) ;gcc
     (amiga-aztec	 m68000	   amiga     cc	       ) ;cc
     (amiga-dice-c	 m68000	   amiga     dcc       ) ;dcc
     (amiga-gcc		 m68000	   amiga     gcc       ) ;gcc
     (amiga-sas		 m68000	   amiga     lc	       ) ;link
     (atari-st-gcc	 m68000	   atari-st  gcc       ) ;gcc
     (atari-st-turbo-c	 m68000	   atari-st  tcc       ) ;tlink
     (borland-c		 i8086	   ms-dos    bcc       ) ;bcc
     (gnu-win32		 i386	   unix	     gcc       ) ;gcc
     (djgpp		 i386	   ms-dos    gcc       ) ;gcc
     (freebsd		 *unknown*	   unix	     cc	       ) ;cc
     (gcc		 *unknown* unix	     gcc       ) ;gcc
     (highc		 i386	   ms-dos    hc386     ) ;bind386
     (hp-ux		 hp-risc   hp-ux     cc	       ) ;cc
     (irix		 mips	   irix	     gcc       ) ;gcc
     (linux		 *unknown*	   linux     gcc       ) ;gcc
     (linux-aout	 i386	   linux     gcc       ) ;gcc
     (linux-ia64	 ia64	   linux     gcc       ) ;gcc
     (darwin		 powerpc   unix      cc	       ) ;gcc
     (microsoft-c	 i8086	   ms-dos    cl	       ) ;link
     (microsoft-c-nt	 i386	   ms-dos    cl	       ) ;link
     (microsoft-quick-c	 i8086	   ms-dos    qcl       ) ;qlink
     (ms-dos		 i8086	   ms-dos    cc	       ) ;link
     (netbsd		 *unknown* unix	     gcc       ) ;gcc
     (openbsd		 *unknown* unix	     gcc       ) ;gcc
     (os/2-cset		 i386	   os/2	     icc       ) ;link386
     (os/2-emx		 i386	   os/2	     gcc       ) ;gcc
     (plan9-8		 i386	   plan9     8c        ) ;8l
     (svr4-gcc-sun-ld	 sparc	   sunos     gcc       ) ;ld
     (sunos		 sparc	   sunos     cc	       ) ;ld
     (svr4		 *unknown* unix	     cc	       ) ;ld
     (turbo-c		 i8086	   ms-dos    tcc       ) ;tcc
     (unicos		 cray	   unicos    cc	       ) ;cc
     (unix		 *unknown* unix	     cc	       ) ;cc
     (vms		 vax	   vms	     cc	       ) ;link
     (vms-gcc		 vax	   vms	     gcc       ) ;link
     (watcom-9.0	 i386	   ms-dos    wcc386p   ) ;wlinkp
     ))

  '(C-libraries
    ((library symbol)
     (platform platform))
    ((compiler-flags string)
     (link-lib-flag string)
     (lib-path optstring)
     (lib-support expression)
     (suppress-files expression))

    ((m *unknown* "" "-lm" "/usr/lib/libm.a" () ())
     (c *unknown* "" "-lc" "/usr/lib/libc.a" () ())
     (regex *unknown* "" "-lregex" "/usr/lib/libregex.a" () ())
     (curses *unknown* "" "-lcurses" "/usr/lib/libcurses.a" () ())
     (graphics *unknown* "-I/usr/X11/include -DX11" "-lX11"
	       "/usr/X11/lib/libX11.sa" () ())
     (editline *unknown* "" "-lreadline" "/usr/lib/libreadline.a" () ())
     (termcap *unknown* "" "-ltermcap" "/usr/lib/libtermcap.a" () ())
     (debug *unknown* "-g" "-g" #f () ())
     (socket *unknown* "" "" #f () ())
     (lib *unknown* "" "" #f () ("scmmain.c"))
     (mysql *unknown* "-I/usr/include/mysql" "-L/usr/lib/mysql -lmysqlclient"
	    "/usr/lib/mysql/libmysqlclient.a" () ())
     (pthread *unknown* "" "-lpthread" #f () ())

     (m gnu-win32 "" "" #f () ())
     (c gnu-win32 "" "" #f () ())
     (dlll gnu-win32 "-DSCM_WIN_DLL" "" #f () ("posix.c" "unix.c" "socket.c"))

     (m linux-aout "" "-lm" "/usr/lib/libm.sa" () ())
     (c linux-aout "" "-lc" "/usr/lib/libc.sa" () ())
     (dlll linux-aout "-DDLD -DDLD_DYNCM" "-ldld" #f () ("findexec.c"))
     (curses linux-aout "-I/usr/include/ncurses" "-lncurses"
	     "/usr/lib/libncurses.a" () ())
     (nostart linux-aout "" "-nostartfiles" #f ("pre-crt0.c") ())
     (dump linux-aout "" "/usr/lib/crt0.o" #f ("unexec.c" "gmalloc.c") ())

     (m linux "" "-lm" "/lib/libm.so" () ())
     (c linux "" "-lc" "/lib/libc.so" () ())
     (dlll linux "-fPIC -DSUN_DL" "-ldl" #f () ())
     (regex linux "" "" #f () ())
     (graphics linux "-I/usr/include/X11 -DX11" "-L/usr/X11R6/lib -lX11"
	       "/usr/X11R6/lib/libX11.so" () ())
     (curses linux "" "-lcurses" "/lib/libncurses.so" () ())
     (nostart linux "" "" #f () ())
     (dump linux "" "" #f ("unexelf.c" "gmalloc.c") ())

     (dump irix "" "-G 0" #f () ())

     (m acorn-unixlib "" "" #f () ())

     (nostart osf1 "" "" #f ("pre-crt0.c") ())
     (dlll osf1 "-DSUN_DL" "" #f () ())
     (dump osf1 "" "" #f ("unexalpha.c" "gmalloc.c") ())
     (regex osf1 "" "" #f () ())
     (graphics osf1 "-I/usr/include/X11 -DX11" "-lX11"
	       #f () ())

     (m amiga-dice-c "" "-lm" #f () ())
     (m amiga-sas "" "lcmieee.lib" #f () ())
     (c amiga-sas "" "lc.lib" #f () ())

     (m vms-gcc "" "" #f () ())
     (m vms "" "" #f () ())

     (m atari-st-gcc "" "-lpml" #f () ())
     (m atari-st-turbo-c "" "" #f () ())

     (c plan9-8 "" "" #f () ())
     (m plan9-8 "" "" #f () ())

     (m sunos "" "-lm" #f () ())
     (dlll sunos "-DSUN_DL" "-ldl" #f () ())
     (nostart sunos "" "-e __start -nostartfiles -static" #f ("ecrt0.c") ())
     (dump sunos "" "" #f ("unexelf.c" "gmalloc.c") ())

     (m svr4-gcc-sun-ld "" "-lm" #f () ())
     (dlll svr4-gcc-sun-ld "-DSUN_DL" "-Wl,-ldl -export-dynamic" #f () ())
     (nostart svr4-gcc-sun-ld "" "-e __start -nostartfiles" #f ("ecrt0.c") ())
     (dump svr4-gcc-sun-ld "" "" #f ("unexelf.c" "gmalloc.c") ())
     (socket svr4-gcc-sun-ld "" "-lsocket -lnsl" #f () ())
     (regex svr4-gcc-sun-ld "" "" #f () ())

     (nostart gcc "" "-e __start -nostartfiles" #f ("ecrt0.c") ())
     (dump gcc "" "" #f ("unexelf.c" "gmalloc.c") ())

     (m hp-ux "" "-lm" #f () ())
     (dlll hp-ux "-DHAVE_DYNL" "-Wl,-E -ldld" #f () ())
     (graphics hp-ux "-DX11" "-lX" "/usr/lib/X11R5/libX11.sl" () ())
     (nostart hp-ux "" "" #f ("ecrt0.c") ())
     (dump hp-ux "" "" #f ("unexhp9k800.c" "gmalloc.c") ())

     (c djgpp "" "-lc" #f () ("findexec.c"))
     (curses djgpp "-I/djgpp/contrib/pdcurses/include/"
	     "-L/djgpp/contrib/pdcurses/lib/ -lcurses"
	     "\\djgpp\\contrib\\pdcurses\\lib\\libcurse.a" () ())
     (nostart djgpp "" "-nostartfiles" #f ("pre-crt0.c") ())
     (dump djgpp "" "c:/djgpp/lib/crt0.o" #f ("unexec.c" "gmalloc.c") ())
;;;     (nostart djgpp "" "" #f ("ecrt0.c") ())
;;;     (dump djgpp "" "" #f ("unexelf.c" "gmalloc.c") ())
;;;     (nostart djgpp "" "-e __start -nostartfiles -static" #f ("ecrt0.c") ())
;;;     (dump djgpp "" "" #f ("unexelf.c" "gmalloc.c") ())

     (c microsoft-c "" "" #f () ("findexec.c"))
     (m microsoft-c "" "" #f () ())
     (c microsoft-c-nt "" "" #f () ("findexec.c"))
     (m microsoft-c-nt "" "" #f () ())
     (dlll microsoft-c-nt "-DSCM_WIN_DLL -MD" "" #f () ("posix.c" "unix.c" "socket.c"))
     (debug microsoft-c-nt "-Zi" "/debug" #f () ())
     (c microsoft-quick-c "" "" #f () ("findexec.c"))
     (m microsoft-quick-c "" "" #f () ())

     (c turbo-c "" "" #f () ("findexec.c"))
     (m turbo-c "" "" #f () ())
     (graphics turbo-c "" "graphics.lib" #f () ())

     (c borland-c "" "" #f () ("findexec.c"))
     (m borland-c "" "" #f () ())
     (graphics borland-c "" "graphics.lib" #f () ())
     (windows borland-c "-N -W" "-W" #f () ())

     (c highc "" "" #f () ("findexec.c"))
     (m highc "" "" #f () ())
     (windows highc "-Hwin" "-Hwin" #f () ())

     (m darwin "" "" #f () ())
     (c darwin "" "" #f () ())
     (curses darwin "" "" #f () ())
     (regex darwin "" "" #f () ())
     (dump darwin "" "" #f ("unexmacosx.c" "lastfile.c") ())
     (dlll darwin "-DSUN_DL" "-ldl" "" () ())

     (c freebsd "" "-export-dynamic" #f () ())
     (m freebsd "" "-lm" #f () ())
     (curses freebsd "" "-lncurses" "/usr/lib/libncurses.a" () ())
     (regex freebsd "-I/usr/include/gnu" "-lgnuregex" "" () ())
     (editline freebsd "" "-lreadline" "" () ())
     (dlll freebsd "-DSUN_DL" "-export-dynamic" "" () ())
     (nostart freebsd "" "-e start -dc -dp -Bstatic -lgnumalloc" #f ("pre-crt0.c") ())
     (dump freebsd "" "/usr/lib/crt0.o" "" ("unexsunos4.c") ())
     (curses netbsd "-I/usr/pkg/include" "-lncurses" "-Wl,-rpath -Wl,/usr/pkg/lib -L/usr/pkg/lib" () ())
     (editline netbsd "-I/usr/pkg/include" "-lreadline" "-Wl,-rpath -Wl,/usr/pkg/lib -L/usr/pkg/lib" () ())
     (graphics netbsd "-I/usr/X11R6/include -DX11" "-lX11" "-Wl,-rpath -Wl,/usr/X11R6/lib -L/usr/X11R6/lib" () ())
     (m netbsd "" "-lm" #f () ())
     (m openbsd "" "-lm" #f () ())
     (dlll openbsd "-DSUN_DL" "" "" () ())
     (curses openbsd "" "-lcurses" "/usr/lib/libcurses.a" () ())
     (regex openbsd "" "" #f () ())
     ))

  '(compile-commands
    ((name symbol)
     (platform platform))
    ((procedure expression))
    ((update-catalog *unknown*
		     (lambda (oname objects libs parms)
		       (batch:rebuild-catalog parms)
		       (if (= 1 (length objects)) (car objects)
			   objects))))))

(define define-compile-commands
  (let ((defcomms ((open-table! build 'compile-commands) 'row:insert)))
    (lambda args
      (defcomms args))))		;(append args (list (comment)))
(defmacro defcommand (name platform procedure)
  `(define-compile-commands ',name ',platform ',procedure))

(defcommand compile-c-files borland-c
  (lambda (files parms)
    (define rsp-name "temp.rsp")
    (apply batch:lines->file parms rsp-name files)
    (and (batch:try-command
	  parms
	  "bcc" "-d" "-Z" "-G" "-w-pro" "-ml" "-c"
	  (if (member '(define "FLOATS" #t)
		      (c-defines parms))
	      "" "-f-")
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  (string-append "@" rsp-name))
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand link-c-program borland-c
  (lambda (oname objects libs parms)
    (define lnk-name (string-append oname ".lnk"))
    (apply batch:lines->file parms
	   lnk-name
	   (append libs objects))
    (and (batch:try-command
	  parms "bcc" (string-append "-e" oname)
	  "-ml" (string-append "@" lnk-name))
	 (string-append oname ".exe"))))

(defcommand compile-c-files turbo-c
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "tcc" "-c" "-d" "-Z" "-G" "-ml" "-c"
	  "-Ic:\\turboc\\include"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand link-c-program turbo-c
  (lambda (oname objects libs parms)
    (let ((exe (truncate-up-to (obj->exe (car objects)) #\\))
	  (oexe (string-append oname ".exe")))
      (and (or (string-ci=? exe oexe)
	       (batch:delete-file parms oexe))
	   (batch:try-command
	    parms "tcc" "-Lc:\\turboc\\lib" libs objects)
	   (or (string-ci=? exe oexe)
	       (batch:rename-file parms exe oexe))
	   oexe))))

(defcommand compile-c-files microsoft-c
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms "cl" "-c" "Oxp" "-AH"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand link-c-program microsoft-c
  (lambda (oname objects libs parms)
    (let ((exe (truncate-up-to (obj->exe (car objects)) #\\))
	  (oexe (string-append oname ".exe")))
      (and (or (string-ci=? exe oexe)
	       (batch:delete-file parms oexe))
	   (batch:try-command
	    parms "link" "/noe" "/ST:40000"
	    (apply string-join "+" (map obj-> objects))
	    libs)
	   (or (string-ci=? exe oexe)
	       (batch:rename-file parms exe oexe))
	   oexe))))

(defcommand compile-c-files microsoft-c-nt
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cl" "-c" "-nologo"
	  (if (memq 'stack-limit (parameter-list-ref parms 'features))
	      "-Oityb1" "-Ox")
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand compile-dll-c-files microsoft-c-nt
  (lambda (files parms)
    (define platform (car (parameter-list-ref parms 'platform)))
    (let ((suppressors (build:c-suppress 'dlll platform)))
      (define c-files (remove-if (lambda (file) (member file suppressors))
				 files))
      (and (batch:try-chopped-command
	    parms
	    "cl" "-c" "-nologo"
	    (if (memq 'stack-limit (parameter-list-ref parms 'features))
		"-Oityb1" "-Ox")
	    (include-spec "-I" parms)
	    (c-includes parms)
	    (c-flags parms)
	    c-files)
	   (let ((fnames (map c-> c-files)))
	     (and (batch:try-command
		   parms "link" "/dll" "/nologo"
		   (string-append "/out:" (car fnames) ".dll")
		   (string-append "/implib:" (car fnames) ".lib")
		   fnames
		   (map (lambda (l) (build:lib-ld-flag l platform))
			(parameter-list-ref parms 'c-lib))
		   "scm.lib")
		  (list (string-append (car fnames) ".dll"))))))))
(defcommand make-dll-archive microsoft-c-nt
  (lambda (oname objects libs parms) objects))
(defcommand make-archive microsoft-c-nt
  (lambda (oname objects libs parms)
    (let ((aname (string-append oname ".dll")))
      (and (batch:try-command parms
			      "link" "/dll" "/nologo"
			      (string-append "/out:" aname)
			      (string-append "/implib:" oname ".lib")
			      libs (map obj-> objects))
	   aname))))
(defcommand link-c-program microsoft-c-nt
  (lambda (oname objects libs parms)
    (let ((exe (truncate-up-to (obj->exe (car objects)) #\\))
	  (oexe (string-append oname ".exe")))
      (and (batch:try-command
	    parms "link" "/nologo"
	    (string-append "/out:" oexe)
	    (apply string-join " " (map obj-> objects))
	    libs)
	   oexe))))

(defcommand compile-c-files microsoft-quick-c
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "qcl" "/AH" "/W1" "/Ze" "/O" "/Ot" "/DNDEBUG"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand link-c-program microsoft-quick-c
  (lambda (oname objects libs parms)
    (define crf-name (string-append oname ".crf"))
    (apply batch:lines->file parms
	   crf-name
	   `(,@(map (lambda (f) (string-append f " +"))
		    objects)
	     ""
	     ,(string-append oname ".exe")
	     ,(apply string-join " " libs)
	     ";"))
    (and (batch:try-command
	  parms "qlink"
	  "/CP:0xffff" "/NOI" "/SE:0x80" "/ST:0x9c40"
	  crf-name)
	 (string-append oname ".exe"))))

(defcommand compile-c-files watcom-9.0
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "wcc386p" "/mf" "/d2" "/ze" "/oxt" "/3s"
	  "/zq" "/w3"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand link-c-program watcom-9.0
  (lambda (oname objects libs parms)
    (let ((exe (truncate-up-to (obj->exe (car objects)) #\\))
	  (oexe (string-append oname ".exe")))
      (and (or (string-ci=? exe oexe)
	       (batch:delete-file parms oexe))
	   (batch:try-command
	    parms
	    "wlinkp" "option" "quiet" "option"
	    "stack=40000" "FILE"
	    (apply string-join "," (map obj-> objects))
	    libs)
	   (if (not (string-ci=? exe oexe))
	       (batch:rename-file parms exe oexe))
	   oexe))))
(defcommand compile-c-files highc
  (lambda (files parms)
    (define hcc-name "temp.hcc")
    (apply batch:lines->file parms hcc-name files)
    (and (batch:try-command
	  parms
	  "d:\\hi_c\\hc386.31\\bin\\hc386"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  "-c" (string-append "@" hcc-name))
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand link-c-program highc
  (lambda (oname objects libs parms)
    (let ((oexe (string-append oname ".exe")))
      (define lnk-name (string-append oname ".lnk"))
      (apply batch:lines->file parms
	     lnk-name (append libs objects))
      (and (batch:try-command
	    parms
	    "d:\\hi_c\\hc386.31\\bin\\hc386" "-o" oname
	    "-stack 65000"
	    (string-append "@" lnk-name))
	   (batch:try-command
	    parms
	    "bind386" "d:/hi_c/pharlap.51/run386b.exe" oname
	    "-exe" oexe)
	   oexe))))

(defcommand compile-c-files djgpp
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "gcc" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->o files) "\\/"))))
(defcommand link-c-program djgpp
  (lambda (oname objects libs parms)
    (let ((exe (string-append oname ".exe")))
      (and (or (batch:try-command parms
				  "gcc" "-o" oname
				  (must-be-first
				   '("-nostartfiles"
				     "pre-crt0.o" "ecrt0.o"
				     "c:/djgpp/lib/crt0.o")
				   (append objects libs)))
	       (let ((arname (string-append oname ".a")))
		 (batch:delete-file parms arname)
		 (and (batch:try-chopped-command
		       parms
		       "ar" "r" arname objects)
		      (batch:try-command
		       parms "gcc" "-o" oname
		       (must-be-first
			'("-nostartfiles"
			  "pre-crt0.o" "ecrt0.o"
			  "c:/djgpp/lib/crt0.o")
			(cons arname libs)))
		      (batch:delete-file parms arname)))
	       ;;(build:error 'build "couldn't build archive")
	       )
	   (batch:try-command parms "strip" exe)
	   (batch:delete-file parms oname)
	   ;;(batch:delete-file parms exe)
	   ;;(batch:try-command parms "coff2exe" "-s" "c:\\djgpp\\bin\\go32.exe" oname)
	   exe))))

(defcommand compile-c-files os/2-emx
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-m386" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\\))))
(defcommand link-c-program os/2-emx
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "gcc" "-o" (string-append oname ".exe")
	  objects libs)
	 (string-append oname ".exe"))))

(defcommand compile-c-files os/2-cset
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms "icc" "/Gd-" "/Ge+" "/Gm+" "/Q" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->obj files) #\\))))
(defcommand link-c-program os/2-cset
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "link386" objects libs
	  (string-append "," oname ".exe,,,;"))
	 (string-append oname ".exe"))))

(defcommand compile-c-files HP-UX
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "cc" "+O1" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand compile-dll-c-files HP-UX
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms "cc" "+O1" "-Wl,-E" "+z" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (let ((fnames (truncate-up-to (map c-> files) #\/)))
	   (define fname.sl (string-append (car fnames) ".sl"))
	   (batch:rename-file parms fname.sl (string-append fname.sl "~"))
	   (and (batch:try-command
		 parms "ld" "-b" "-o"
		 fname.sl
		 (map (lambda (fname) (string-append fname ".o")) fnames))
		(list fname.sl))))))
;    (make-dll-archive HP-UX
;		       (lambda (oname objects libs parms)
;			 (and (batch:try-command
;			       parms "ld" "-b" "-o" (string-append oname ".sl")
;			       objects)
;			      (batch:rebuild-catalog parms)
;			      (string-append oname ".sl"))))

(defcommand compile-dll-c-files linux-aout
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "gcc" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->o files) #\/))))
;;;     (make-dll-archive linux-aout
;;;		       (lambda (oname objects libs parms) #t
;;;			       (batch:rebuild-catalog parms)
;;;			       oname))

(defcommand compile-c-files linux
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand compile-dll-c-files linux
  (lambda (files parms)
    (and
     (batch:try-chopped-command parms "gcc" "-fpic" "-c"
				(include-spec "-I" parms)
				(c-includes parms)
				(c-flags parms)
				files)
     (let* ((platform (car (parameter-list-ref parms 'platform)))
	    (fnames (truncate-up-to (map c-> files) #\/))
	    (fname.so (string-append (car fnames) ".so"))
	    (result
	     (and (batch:try-command
		   parms
		   "gcc" "-shared" "-o" fname.so
		   (map (lambda (fname) (string-append fname ".o")) fnames)
		   (map (lambda (l) (build:lib-ld-flag l platform))
			(parameter-list-ref parms 'c-lib)))
		  (list fname.so))))
       (for-each (lambda (fname)
		   (batch:delete-file
		    parms (string-append fname ".o")))
		 fnames)
       result))))
(defcommand make-dll-archive linux
  (lambda (oname objects libs parms)
    (let ((platform (car (parameter-list-ref parms 'platform))))
      (and (batch:try-command
	    parms
	    "gcc" "-shared" "-o"
	    (string-append
	     (car (parameter-list-ref parms 'implvic))
	     oname ".so")
	    objects
	    (map (lambda (l) (build:lib-ld-flag l platform))
		 (parameter-list-ref parms 'c-lib)))
	   (batch:rebuild-catalog parms)
	   (string-append
	    (car (parameter-list-ref parms 'implvic))
	    oname ".so")))))
(defcommand link-c-program linux
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "gcc" "-rdynamic" "-o" oname
	  (must-be-first
	   '("pre-crt0.o" "ecrt0.o" "/usr/lib/crt0.o")
	   (append objects libs)))
	 oname)))

(define (build-continue-ia64 parms)
  (and (batch:try-command
	parms "gcc -o get-contoffset-ia64 get-contoffset-ia64.c")
       (batch:try-command
	parms "./get-contoffset-ia64 contoffset-ia64.S")
       (batch:try-command
	parms "gcc -c continue-ia64.S")))

(defcommand link-c-program linux-ia64
  (lambda (oname objects libs parms)
    (and (build-continue-ia64 parms)
	 (batch:try-command
	  parms "gcc" "-rdynamic" "-o" oname "continue-ia64.o"
	  (must-be-first
	   '("pre-crt0.o" "ecrt0.o" "/usr/lib/crt0.o")
	   (append objects libs)))
	 oname)))

(defcommand compile-c-files unicos
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cc" "-hvector2" "-hscalar2" "-c"
	  (include-spec "-i" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program unicos
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "cc" "setjump.o" "-o" oname objects libs)
	 oname)))

;; George Bronnikov <goga@rubinstein.mccme.ru> describes options for the
;; PLAN9 native C compiler `8c':
;;
;; -F Enable type-checking of calls to print(2) and other
;;    formatted print routines.
;; -V By default, the compilers are non-standardly lax about
;;    type equality between void* values and other pointers.
;;    This flag requires ANSI C conformance.
;; -w Print warning messages about unused variables etc. (It
;;    does print a lot of them, indeed.)
;; -p Invoke a standard ANSI C preprocessor before compiling
;;    (instead of a rudimentary builtin one used by default).
(defcommand compile-c-files plan9-8
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "8c" "-Fwp" "-DPLAN9"		;"-V"
	  ;;(include-spec "-i" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->8 files) #\/))))
(defcommand link-c-program plan9-8
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "8l" "-o" oname objects libs)
	 oname)))

(defcommand compile-c-files gcc
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program gcc
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "gcc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "ecrt0.o"
			       "/usr/lib/crt0.o")
			     (append objects libs)))
	 oname)))
(defcommand compile-dll-c-files gcc
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) "\\/]"))))
(defcommand make-dll-archive gcc
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms
	  "ld" "-assert" "pure-text" "-o"
	  (string-append
	   (car (parameter-list-ref parms 'implvic))
	   oname ".so.1.0")
	  objects)
	 (batch:rebuild-catalog parms)
	 (string-append
	  (car (parameter-list-ref parms 'implvic))
	  oname ".so.1.0"))))

(defcommand compile-dll-c-files gnu-win32
  (lambda (files parms)
    (define platform (car (parameter-list-ref parms 'platform)))
    (let ((suppressors (build:c-suppress 'dlll platform)))
      (define c-files (remove-if (lambda (file) (member file suppressors))
				 files))
      (and (batch:try-chopped-command
	    parms "gcc" "-c"
	    (include-spec "-I" parms)
	    (c-includes parms)
	    (c-flags parms)
	    c-files)
	   (let ((fnames (map c-> c-files)))
	     (and (batch:try-command
		   parms "dllwrap"
		   "--output-lib" (string-append (car fnames) ".lib")
		   "-dllname" (string-append (car fnames) ".dll")
		   "--output-def" (string-append (car fnames) ".def")
		   (map (lambda (fname) (string-append fname ".o"))
			fnames)
		   (map (lambda (l) (build:lib-ld-flag l platform))
			(parameter-list-ref parms 'c-lib))
		   "scm.lib")
		  (list (string-append (car fnames) ".dll"))))))))
(defcommand make-dll-archive gnu-win32
  (lambda (oname objects libs parms) objects))
(defcommand make-archive gnu-win32
  (lambda (oname objects libs parms)
    (let ((aname (string-append oname ".dll")))
      (and (batch:try-command parms
			      "dllwrap"
			      "--output-lib" (string-append oname ".lib")
			      "-dllname" aname
			      "--output-def" (string-append oname ".def")
			      libs objects)
	   aname))))
(defcommand compile-c-files gnu-win32
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program gnu-win32
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       (string-append oname ".exe")
		       (string-append oname "~"))
    (and (batch:try-command parms
			    "gcc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "ecrt0.o"
			       "/usr/lib/crt0.o")
			     (append objects libs)))
	 oname)))

(defcommand compile-c-files osf1
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cc" "-std1" "-c"
	  ;;(if (member "-g" (c-includes parms)) "" "-O")
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand compile-dll-c-files osf1
  (lambda (files parms)
    (and
     (batch:try-chopped-command
      parms "cc" "-std1" "-c"
      (include-spec "-I" parms)
      (c-includes parms)
      (c-flags parms)
      files)
     (let* ((platform (car (parameter-list-ref parms 'platform)))
	    (fnames (truncate-up-to (map c-> files) #\/)))
       (and (batch:try-command
	     parms "cc" "-shared" "-o" (string-append (car fnames) ".so")
	     (map (lambda (fname) (string-append fname ".o")) fnames)
	     (map (lambda (l) (build:lib-ld-flag l platform))
		  (parameter-list-ref parms 'c-lib)))
	    (for-each (lambda (fname)
			(batch:delete-file parms (string-append fname ".o")))
		      fnames)
	    (list (string-append (car fnames) ".so")))))))
(defcommand make-dll-archive osf1
  (lambda (oname objects libs parms)
    (let ((platform (car (parameter-list-ref parms 'platform))))
      (and (batch:try-command
	    parms
	    "cc" "-shared" "-o"
	    (string-append
	     (car (parameter-list-ref parms 'implvic))
	     oname ".so")
	    objects
	    (map (lambda (l) (build:lib-ld-flag l platform))
		 (parameter-list-ref parms 'c-lib)))
	   (batch:rebuild-catalog parms)
	   (string-append
	    (car (parameter-list-ref parms 'implvic))
	    oname ".so")))))

(defcommand compile-c-files svr4-gcc-sun-ld
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program svr4-gcc-sun-ld
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "gcc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "ecrt0.o"
			       "/usr/lib/crt0.o")
			     (append objects libs)))
	 oname)))
(defcommand compile-dll-c-files svr4-gcc-sun-ld
  (lambda (files parms)
    (and
     (batch:try-chopped-command parms "gcc" "-fpic" "-c"
				(include-spec "-I" parms)
				(c-includes parms)
				(c-flags parms)
				files)
     (let* ((platform (car (parameter-list-ref parms 'platform)))
	    (fnames (truncate-up-to (map c-> files) #\/)))
       (and (batch:try-command
	     parms "ld" "-G" "-o" (string-append (car fnames) ".so")
	     (map (lambda (fname) (string-append fname ".o")) fnames)
	     (map (lambda (l) (build:lib-ld-flag l platform))
		  (parameter-list-ref parms 'c-lib)))
	    (for-each (lambda (fname)
			(batch:delete-file parms (string-append fname ".o")))
		      fnames)
	    (list (string-append (car fnames) ".so")))))))

(defcommand compile-c-files svr4
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "cc" "-DSVR4" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))

(defcommand compile-c-files aix
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "cc" "-Dunix" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program aix
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "cc" "-lansi" "-o" oname objects libs)
	 oname)))

(defcommand compile-c-files amiga-aztec
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "cc" "-dAMIGA"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program amiga-aztec
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "cc" "-o" oname objects libs "-lma")
	 oname)))

(defcommand compile-c-files amiga-sas
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "lc" "-d3" "-M" "-fi"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (batch:try-command
	  parms "blink with link.amiga NODEBUG")
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program amiga-sas
  (lambda (oname objects libs parms)
    (define lnk-name "link.amiga")
    (apply batch:lines->file parms
	   lnk-name
	   (apply string-join "+" ">FROM LIB:c.o"
		  (map object->string objects))
	   (string-append
	    "TO " (object->string (string-append "/" oname)))
	   (append
	    (cond
	     ((pair? libs)
	      (cons (string-append "LIB LIB:" (car libs))
		    (map (lambda (s)
			   (string-append "    LIB:" s))
			 (cdr libs))))
	     (else '()))
	    '("VERBOSE" "SC" "SD")))
    oname))

(defcommand compile-c-files amiga-dice-c
  (lambda (files parms)
    (and (batch:try-command
	  parms
	  "dcc" "-r" "-gs" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files "-o" (truncate-up-to (map c->o files) #\/))
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program amiga-dice-c
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "dcc" "-r" "-gs" "-o" oname objects libs)
	 oname)))

(defcommand compile-c-files amiga-gcc
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program amiga-gcc
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "gcc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "ecrt0.o"
			       "/usr/lib/crt0.o")
			     (append objects libs)))
	 oname)))

(defcommand compile-c-files atari-st-gcc
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "gcc" "-v" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program atari-st-gcc
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "gcc" "-v" "-o" (string-append oname ".ttp")
	  objects libs)
	 (string-append oname ".ttp"))))

(defcommand compile-c-files atari-st-turbo-c
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "tcc" "-P" "-W-" "-Datarist"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program atari-st-turbo-c
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "tlink" "-o" (string-append oname ".ttp")
	  objects libs "mintlib.lib" "osbind.lib"
	  "pcstdlib.lib" "pcfltlib.lib")
	 (string-append oname ".ttp"))))

(defcommand compile-c-files acorn-unixlib
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cc" "-c" "-depend" "!Depend" "-IUnixLib:"
	  "-pcc" "-Dunix" "-DSVR3" "-DARM_ULIB"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (truncate-up-to (map c->o files) #\/))))
(defcommand link-c-program acorn-unixlib
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms "link" "-o" oname objects libs
	  ":5.$.dev.gcc.unixlib36d.clib.o.unixlib")
	 (batch:try-command parms "squeeze" oname)
	 oname)))

(defcommand compile-c-files vms
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cc"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  (map c-> files))
	 (truncate-up-to (map c->obj files) "/]"))))
(defcommand link-c-program vms
  (lambda (oname objects libs parms)
    (let ((exe (truncate-up-to (obj->exe (car objects)) "/]"))
	  (oexe (string-append oname ".exe")))
      (and (batch:try-command parms "macro" "setjump")
	   (batch:try-command
	    parms
	    "link"
	    (apply string-join ","
		   (append (map obj-> objects)
			   '("setjump" "sys$input/opt\n   ")))
	    (apply string-join
		   "," (append (remove "" libs)
			       '("sys$share:vaxcrtl/share"))))
	   (or (string-ci=? exe oexe)
	       (batch:rename-file parms exe oexe))
	   oexe))))

(defcommand compile-c-files vms-gcc
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "gcc"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  (map c-> files))
	 (truncate-up-to (map c->obj files) "/]"))))
(defcommand link-c-program vms-gcc
  (lambda (oname objects libs parms)
    (let ((exe (truncate-up-to (obj->exe (car objects)) "/]"))
	  (oexe (string-append oname ".exe")))
      (and (batch:try-command parms "macro" "setjump")
	   (batch:try-command
	    parms
	    "link"
	    (apply string-join ","
		   (append objects
			   '("setjump.obj"
			     "sys$input/opt\n   ")))
	    (apply string-join
		   "," (append (remove "" libs)
			       '("gnu_cc:[000000]gcclib/lib"
				 "sys$share:vaxcrtl/share"))))
	   (or (string-ci=? exe oexe)
	       (batch:rename-file parms exe oexe))
	   oexe))))

(defcommand compile-c-files *unknown*
  (lambda (files parms)
    (batch:try-chopped-command
     parms
     "cc" "-c"
     (include-spec "-I" parms)
     (c-includes parms)
     (c-flags parms)
     files)
    (truncate-up-to (map c->o files) "\\/]")))
(defcommand link-c-program *unknown*
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "cc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "ecrt0.o"
			       "/usr/lib/crt0.o")
			     (append objects libs)))
	 oname)))
(defcommand make-archive *unknown*
  (lambda (oname objects libs parms)
    (let ((aname (string-append "lib" oname ".a")))
      (and (batch:try-command parms "ar rc" aname objects)
	   (batch:try-command parms "ranlib" aname)
	   aname))))

(defcommand make-archive linux-ia64
  (lambda (oname objects libs parms)
    (let ((aname (string-append "lib" oname ".a")))
      (and (build-continue-ia64 parms)
	   (batch:try-command parms "ar rc" aname objects "continue-ia64.o")
	   (batch:try-command parms "ranlib" aname)
	   aname))))

(defcommand compile-dll-c-files *unknown*
  (lambda (files parms)
    (and (batch:try-chopped-command parms
				    "cc" "-c"
				    (include-spec "-I" parms)
				    (c-includes parms)
				    (c-flags parms)
				    files)
	 (truncate-up-to (map c->o files) "\\/]"))))
(defcommand make-dll-archive *unknown*
  (lambda (oname objects libs parms)
    (let ((aname
	   (string-append
	    (car (parameter-list-ref parms 'implvic))
	    oname ".a")))
      (and (batch:try-command parms "ar rc" aname objects)
	   (batch:try-command parms "ranlib" aname)
	   (batch:rebuild-catalog parms)
	   aname))))

(defcommand compile-c-files freebsd
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
;;; gcc 3.4.2 for FreeBSD does not allow options other than default i.e. -O0 if NO -DGCC_SPARC_BUG - dai 2004-10-30
	  ;;"cc" "-O3 -pipe -DGCC_SPARC_BUG " "-c"
	  "cc" "-O3 -pipe " "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (map c->o files))))
(defcommand link-c-program freebsd
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "cc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "crt0.o"
			       "/usr/lib/crt0.o")
			     (append objects libs)))
	 oname)))
(defcommand compile-dll-c-files freebsd
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms "cc" "-O3 -pipe " "-fPIC" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (let ((fnames (truncate-up-to (map c-> files) #\/)))
	   (and (batch:try-command
		 parms "cc" "-shared"
		 (cond
		  ((equal? (car fnames) "edline") "-lreadline")
		  ((equal? (car fnames) "x") "-L/usr/X11R6/lib -lSM -lICE -lXext -lX11 -lxpg4")
		  (else ""))
		 "-o" (string-append (car fnames) ".so")
		 (map (lambda (fname) (string-append fname ".o")) fnames))
		(for-each (lambda (fname)
			    (batch:delete-file
			     parms (string-append fname ".o")))
			  fnames)
		(list (string-append (car fnames) ".so")))))))
(defcommand make-dll-archive freebsd
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms
	  "cc" "-shared" "-o"
	  (string-append
	   (car (parameter-list-ref parms 'implvic))
	   oname ".so")
	  objects)
	 (batch:rebuild-catalog parms)
	 (string-append
	  (car (parameter-list-ref parms 'implvic))
	  oname ".so"))))

(defcommand compile-c-files darwin
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cc" "-O3" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (map c->o files))))
(defcommand link-c-program darwin
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "cc" "-o" oname
			    (append objects libs))
	 oname)))
(defcommand compile-dll-c-files darwin
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "env MACOSX_DEPLOYMENT_TARGET=10.3"
	  "gcc" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (let ((fnames (truncate-up-to (map c-> files) #\/)))
	   (and (batch:try-command
		 parms
		 "env MACOSX_DEPLOYMENT_TARGET=10.3"
		 "gcc" "-dynamiclib" "-single_module" "-L." "-undefined" "dynamic_lookup"
		 "-o" (string-append (car fnames) ".so")
		 (map (lambda (fname) (string-append fname ".o")) fnames))
		(for-each (lambda (fname)
			    (batch:delete-file
			     parms (string-append fname ".o")))
			  fnames)
		(list (string-append (car fnames) ".so")))))))
(defcommand make-dll-archive darwin
  (lambda (oname objects libs parms)
    (let ((platform (car (parameter-list-ref parms 'platform))))
      (and (batch:try-command
	    parms
	    "env MACOSX_DEPLOYMENT_TARGET=10.3"
	    "gcc" "-dynamiclib" "-L." "-undefined" "dynamic_lookup" "-o"
	    (string-append
	     (car (parameter-list-ref parms 'implvic))
	     oname ".so")
	    objects
	    (map (lambda (l) (build:lib-ld-flag l platform))
		 (parameter-list-ref parms 'c-lib)))
	   (batch:rebuild-catalog parms)
	   (string-append
	    (car (parameter-list-ref parms 'implvic))
	    oname ".so")))))

(defcommand compile-c-files netbsd
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cc" "-c" (include-spec "-I" parms)
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (map c->o files))))
(defcommand link-c-program netbsd
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "cc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "crt0.o"
			       "/usr/lib/crt0.o")
			     (append libs objects)))
	 oname)))
(defcommand compile-dll-c-files netbsd
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms "cc" "-fPIC" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (let ((objs (map c->o files)))
	   (and (batch:try-command parms "gcc" "-shared" "-fPIC" objs)
		(batch:try-command parms "mv" "a.out" (car objs))
		(list (car objs)))))))
(defcommand make-dll-archive netbsd
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms
	  "gcc" "-shared" "-fPIC" "-o"
	  (string-append
	   (car (parameter-list-ref parms 'implvic))
	   oname ".so")
	  objects)
	 (batch:rebuild-catalog parms)
	 (string-append
	  (car (parameter-list-ref parms 'implvic))
	  oname ".so"))))

(defcommand compile-c-files openbsd
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms
	  "cc" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (map c->o files))))
(defcommand link-c-program openbsd
  (lambda (oname objects libs parms)
    (batch:rename-file parms
		       oname (string-append oname "~"))
    (and (batch:try-command parms
			    "cc" "-o" oname
			    (must-be-first
			     '("-nostartfiles"
			       "pre-crt0.o" "crt0.o"
			       "/usr/lib/crt0.o")
			     (append objects libs)))
	 oname)))
(defcommand compile-dll-c-files openbsd
  (lambda (files parms)
    (and (batch:try-chopped-command
	  parms "cc" "-fPIC" "-c"
	  (include-spec "-I" parms)
	  (c-includes parms)
	  (c-flags parms)
	  files)
	 (let ((objs (map c->o files)))
	   (and (batch:try-command parms "gcc" "-shared" "-fPIC" objs)
		(batch:try-command parms "mv" "a.out" (car objs))
		(list (car objs)))))))

(defcommand make-dll-archive openbsd
  (lambda (oname objects libs parms)
    (and (batch:try-command
	  parms
	  "gcc" "-shared" "-fPIC" "-o"
	  (string-append
	   (car (parameter-list-ref parms 'implvic))
	   oname ".so")
	  objects)
	 (batch:rebuild-catalog parms)
	 (string-append
	  (car (parameter-list-ref parms 'implvic))
	  oname ".so"))))

(define-domains build
  '(C-libraries C-libraries #f symbol #f))

(define-tables build

  '(build-params
    *parameter-columns*
    *parameter-columns*
    ((1 platform single platform
	(lambda (pl) (list *operating-system*))
	#f
	"what to build it for")
     (2 target-name single string (lambda (pl) '("scm")) #f
	"base name of target")
     (3 c-lib nary C-libraries (lambda (pl) '(c)) #f
	"C library (and include files)")
     (4 define nary string #f #f "#define FLAG")
     (5 implvic single string (lambda (pl) (list ""))
	#f "implementation vicinity")
     (6 c-file nary filename #f #f "C source files")
     (7 o-file nary filename #f #f "other object files")
     (8 init nary string #f #f "initialization calls")
     (9 compiled-init nary string #f #f "later initialization calls")
     (10 features nary features
	 (lambda (pl) '(arrays inexact bignums))
	 (lambda (rdb) ((open-table rdb 'features) 'get 'spec))
	 "features to include")
     (11 what single build-whats
	 (lambda (pl) '(exe))
	 (lambda (rdb)
	   (let* ((bwt (open-table rdb 'build-whats))
		  (getclass (bwt 'get 'class))
		  (getspec (bwt 'get 'spec))
		  (getfile ((open-table rdb 'manifest) 'get* 'file)))
	     (lambda (what)
	       `((c-file ,@(getfile #f 'c-source (getclass what)))
		 ,@(or (getspec what) '())))))
	 "what to build")
     (12 batch-dialect single batch-dialect
	 (lambda (pl) '(default-for-platform)) ;;guess-how
	 #f
	 "scripting language")
     (13 who optional expression #f #f "name of buildfile")
     (14 compiler-options nary string #f #f "command-line compiler options")
     (15 linker-options nary string #f #f "command-line linker options")

     (16 scm-srcdir single filename
	 (lambda (pl) (list (user-vicinity))) #f
	 "directory path for files in the manifest")
     (17 c-defines nary expression #f #f "#defines for C")
     (18 c-includes nary expression #f #f "library induced defines for C")
     (19 batch-port nary expression #f #f
	 "port batch file will be written to.")
     ;; The options file is read by a fluid-let getopt-- in "build".
     ;; This is here so the usage message will include -f <filename>.
     (20 options-file nary filename #f #f
	 "file containing more build options.")
     ))
  '(build-pnames
    ((name string))
    ((parameter-index uint))		;should be build-params
    (
     ("p" 1) ("platform" 1)
     ("o" 2) ("outname" 2)
     ("l" 3) ("libraries" 3)
     ("D" 4) ("defines" 4)
     ("s" 5) ("scheme initialization file" 5)
     ("c" 6) ("c source files" 6)
     ("j" 7) ("object files" 7)
     ("i" 9) ("initialization calls" 9)
     ("F" 10) ("features" 10)
     ("t" 11) ("type" 11)
     ("h" 12) ("batch dialect" 12)
     ("w" 13) ("script name" 13)
     ("compiler options" 14)
     ("linker options" 15)
     ("scm srcdir" 16)
     ("f" 20)
     ))

  '(*commands*
    ((name symbol))			;or just desc:*commands*
    ((parameters parameter-list)
     (parameter-names parameter-name-translation)
     (procedure expression)
     (documentation string))
    ((build
      build-params
      build-pnames
      build:command
      "compile and link SCM programs.")
     (*initialize*
      no-parameters
      no-parameters
      #f
      "SCM Build Database"))))

(define build:error slib:error)
(define build:c-libraries #f)
(define build:lib-cc-flag #f)
(define build:lib-ld-flag #f)
(define build:c-lib-support #f)
(define build:c-suppress #f)
(define plan-command #f)
(define platform->os #f)

;;; Look up command on a platform, but default to '*unknown* if not
;;; initially found.

(define (make-defaulting-platform-lookup getter)
  (lambda (thing plat)
    (define (look platform)
      (let ((ans (getter thing platform)))
	(cond (ans ans)
	      (else (let ((os (platform->os platform)))
		      (cond ((eq? os platform) (look '*unknown*))
			    ((eq? platform '*unknown*) '())
			    (else (look os))))))))
    (look plat)))

(define (build:command rdb)
  (lambda (parms)
    (let ((expanders
	   (map (lambda (e) (and e (lambda (s) (e s))))
		(map (lambda (f) (if f ((slib:eval f) rdb) f))
		     (((open-table rdb 'build-params)
		       'get* 'expander))))))
      (parameter-list-expand expanders parms)
      (set! parms
	    (fill-empty-parameters
	     (map slib:eval
		  (((open-table rdb 'build-params)
		    'get* 'defaulter)))
	     parms))
      (parameter-list-expand expanders parms))
    (let* ((platform (car (parameter-list-ref parms 'platform)))
	   (init= (apply string-append
			 (map (lambda (c)
				(string-append c "();"))
			      (parameter-list-ref parms 'init))))
	   (compiled-init=
	    (apply string-append
		   (map (lambda (c)
			  (string-append c "();"))
			(parameter-list-ref parms 'compiled-init))))
	   (implvic (let ((impl (car (parameter-list-ref parms 'implvic))))
		      (if (equal? "" impl)
			  (car (parameter-list-ref parms 'scm-srcdir))
			  impl)))
	   (c-defines
	    `((define "IMPLINIT"
		,(object->string
		  (string-append
		   implvic "Init"
		   (read-version
		    (in-vicinity (car (parameter-list-ref parms 'scm-srcdir))
				 "patchlvl.h"))
		   ".scm")))
	      ,@(if (string=? "" init=) '()
		    `((define "INITS" ,init=)))
	      ,@(if (string=? "" compiled-init=) '()
		    `((define "COMPILED_INITS" ,compiled-init=)))
	      ,@(map (lambda (d) (if (pair? d)
				     `(define ,@d)
				     `(define ,d #t)))
		     (parameter-list-ref parms 'define))))
	   (c-includes
	    (map (lambda (l) (build:lib-cc-flag l platform))
		 (parameter-list-ref parms 'c-lib)))
	   (what (car (parameter-list-ref parms 'what)))
	   (c-proc (plan-command (((open-table rdb 'build-whats)
				   'get 'c-proc)
				  what)
				 platform)))

      (case (car (parameter-list-ref parms 'batch-dialect))
	((default-for-platform)
	 (let ((os (((open-table build 'platform)
		     'get 'operating-system) platform)))
	   (if (not os)
	       (build:error "OS corresponding to " platform " unknown"))
	   (adjoin-parameters!
	    parms (cons 'batch-dialect (list (os->batch-dialect os)))))))

      (adjoin-parameters!
       parms (cons 'c-defines c-defines) (cons 'c-includes c-includes))
      (set! parms
	    (cons
	     (cons 'operating-system
		   (map platform->os (parameter-list-ref parms 'platform)))
	     parms))

      (let ((name (parameter-list-ref parms 'who)))
	(set! name (if (null? name) (current-output-port) (car name)))
	(batch:call-with-output-script
	 parms
	 name
	 (lambda (batch-port)
	   (define o-files #f)
	   (adjoin-parameters! parms (list 'batch-port batch-port))
	   (batch:comment
		   parms
		   (string-append "[-p " (symbol->string platform) "]"))
	   (let ((options-file (parameter-list-ref parms 'options-file)))
	     (and (not (null? options-file))
		  (batch:comment
		   parms
		   (apply string-join " " "used options from:" options-file))))
	   (batch:comment parms "================ Write file with C defines")
	   (cond
	    ((not (apply batch:lines->file parms
			 "scmflags.h"
			 (defines->c-defines c-defines)))
	     (batch:comment parms "================ Write failed!") #f)
	    (else
	     (batch:comment parms "================ Compile C source files")
	     (set! o-files
		   (let ((suppressors
			  (apply append
				 (map (lambda (l) (build:c-suppress l platform))
				      (parameter-list-ref parms 'c-lib)))))
		     (c-proc
		      (apply
		       append
		       (remove-if (lambda (file) (member file suppressors))
				  (parameter-list-ref parms 'c-file))
		       (map (lambda (l) (build:c-lib-support l platform))
			    (parameter-list-ref parms 'c-lib)))
		      parms)))
	     (cond
	      ((not o-files)
	       (batch:comment parms "================ Compilation failed!") #f)
	      (else

	       (batch:comment parms "================ Link C object files")
	       (let ((ans
		      ((plan-command
			(((open-table rdb 'build-whats) 'get 'o-proc) what)
			platform)
		       (car (parameter-list-ref parms 'target-name))
		       (append o-files (parameter-list-ref parms 'o-file))
		       (append
			(parameter-list-ref parms 'linker-options)
			(map (lambda (l) (build:lib-ld-flag l platform))
			     (parameter-list-ref parms 'c-lib)))
		       parms)))
		 (cond ((not ans)
			(batch:comment parms "================ Link failed!") #f)
		       (else ans)))))))))))))

(define (include-spec str parms)
  (let ((path (car (parameter-list-ref parms 'scm-srcdir))))
    (if (eqv? "" path) () (list str path))))
(define (c-defines parms)
  (parameter-list-ref parms 'c-defines))
(define (c-includes parms)
  (parameter-list-ref parms 'c-includes))
(define (c-flags parms)
  (parameter-list-ref parms 'compiler-options))

(define (defines->c-defines defines)
  (map
   (lambda (d)
     (case (caddr d)
       ((#t) (string-join " " "#define" (cadr d)))
       ((#f) (string-join " " "#undef" (cadr d)))
       (else (apply string-join " " "#define" (cdr d)))))
   defines))

(define (defines->flags defines)
  (map
   (lambda (d)
     (case (caddr d)
       ((#t) (string-append "-D" (cadr d)))
       ((#f) (string-append "-U" (cadr d)))
       (else (string-append "-D" (cadr d) "=" (object->string (caddr d))))))
   defines))

(define c-> (filename:substitute?? "*.c" "*"))
(define c->o (filename:substitute?? "*.c" "*.o"))
(define c->8 (filename:substitute?? "*.c" "*.8"))
(define c->obj (filename:substitute?? "*.c" "*.obj"))
(define obj-> (filename:substitute?? "*.obj" "*"))
(define obj->exe (filename:substitute?? "*.obj" "*.exe"))

(define (read-version revfile)
  (call-with-input-file
      (if (file-exists? revfile)
	  revfile
	  (in-vicinity (implementation-vicinity) "patchlvl.h"))
    (lambda (port)
      (do ((c (read-char port) (read-char port)))
	  ((or (eof-object? c) (eqv? #\= c))
	   (do ((c (read-char port) (read-char port))
		(lst '() (cons c lst)))
	       ((or (eof-object? c) (char-whitespace? c))
		(list->string (reverse lst)))))))))

(define (batch:rebuild-catalog parms)
  (batch:delete-file parms
		     (in-vicinity (car (parameter-list-ref parms 'implvic))
				  "slibcat"))
  #t)

(define (logger . args)
  (define cep (current-error-port))
  (for-each (lambda (x) (display #\space cep) (display x cep))
	    (cond ((provided? 'bignum)
		   (require 'posix-time)
		   (let ((ct (ctime (current-time))))
		     (string-set! ct (+ -1 (string-length ct)) #\:)
		     (cons ct args)))
		  (else args)))
  (newline cep))

(define build:qacs #f)
;@
(define (build:serve request-line query-string header)
  (define query-alist (and query-string (uri:decode-query query-string)))
  (if (not build:qacs)
      (set! build:qacs (make-query-alist-command-server build '*commands* #t)))
  (call-with-outputs
   (lambda () (build:qacs query-alist))
   (lambda (stdout stderr . status)
     (cond ((or (substring? ": ERROR: " stderr)
		(substring? ": WARN: " stderr))
	    => (lambda (idx)
		 (set! stderr (substring stderr (+ 2 idx)
					 (string-length stderr))))))
     (cond ((null? status)
	    (logger "Aborting query")
	    (pretty-print query-alist)
	    (display stderr)
	    (list "buildscm Abort" (html:pre stdout)
		  "<B>" (html:pre stderr) "</B>"))
	   (else
	    (display stderr)		;query is already logged
	    (if (car status)
		(http:content '(("Content-Type" . "text/plain")) ;application/x-sh
			      stdout)
		(list "buildscm Error" "<B>" (html:pre stderr) "</B>"
		      "<HR>"
		      (html:pre stdout))))))))
;;; (print 'request-line '= (cgi:request-line)) (print 'header '=) (for-each print (cgi:query-header))

(define build:initializer
  (lambda (rdb)
    (set! build:c-libraries
	  (open-table rdb 'c-libraries))
    (set! build:lib-cc-flag
	  (make-defaulting-platform-lookup
	   (build:c-libraries 'get 'compiler-flags)))
    (set! build:lib-ld-flag
	  (make-defaulting-platform-lookup
	   (build:c-libraries 'get 'link-lib-flag)))
    (set! build:c-lib-support
	  (make-defaulting-platform-lookup
	   (build:c-libraries 'get 'lib-support)))
    (set! build:c-suppress
	  (make-defaulting-platform-lookup
	   (build:c-libraries 'get 'suppress-files)))
    (set! platform->os
	  ((open-table rdb 'platform) 'get 'operating-system))
    (set! plan-command
	  (let ((lookup (make-defaulting-platform-lookup
			 ((open-table rdb 'compile-commands)
			  'get 'procedure))))
	    (lambda (thing plat)
	      ;;(print 'thing thing 'plat plat)
	      (slib:eval (lookup thing plat)))))))
(build:initializer build)
