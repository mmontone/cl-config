;;;; "hobbit.scm": an optimizing scheme -> C compiler for SCM
;; Copyright (C) 1992-2006 Free Software Foundation
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;====================================================================
;
;	    HOBBIT: an optimizing scheme -> C compiler for SCM
;
;                   scm5e1
;                   2002-04-11
;
;	    tammet@staff.ttu.ee, tammet@cs.chalmers.se
;
;		    Tanel Tammet
;                   Department of Computer Science
;                   Tallinn University of Technology
;                   Raja 15
;                   12618, Tallinn
;                   Estonia
;
;		    Department of Computing Science
;		    Chalmers University of Technology
;		    University of Go"teborg
;		    S-41296 Go"teborg
;		    Sweden
;
;
;		Documentation is in the file hobbit.texi
;
;                 NB! the terms for usage, copying
;   and redistribution of hobbit are given in the file COPYING
;====================================================================
;
; Last part of changelog:
;
; april 2-11, 2002, Tanel Tammet:
;   -  "system" and "verbose"  compilation corrected
;      (system was previously not compiled, verbose is compiled to prolixity)
;   -  "require" moved from top level to hobbit procedure (necessary
;       for self-compilation)
;   -  "copy-tree" and "acons" compilation introduced
; pre-april, 2002, Aubrey Jaffer:
;   -  numerous changes necessary for co-operation with SCM5d5

;;; Declare modules which might be needed:
(require-if 'compiling 'pretty-print)
(require-if 'compiling 'defmacroexpand)
(require-if 'compiling 'pprint-file)

;=================================================================
;
;		   default compiler options
;		      (may be changed)
;
;=================================================================

;;; The following variable controls whether hobbit will do any
;;; macroexpansion. In that case (require 'defmacroexpand) must
;;; be able to load the macroexpander from the scheme library.

(define *expand-macros-flag* #t)

;;; The following variable controls whether functions declared
;;; to be inlined are inlined in full or only once. If the set of
;;; nested inlinable function defs contains a circularity, the
;;; setting #t will cause Hobbit to go into an infinite loop.

(define *full-inlining-flag* #t)

;;; The following variable controls whether any intermediate files
;;; will be built. In that case (require 'pretty-print) and
;;; (require 'pprint-load) must be able to load the prettyprinter
;;; from the scheme library.

(define *build-intermediate-files* #f)

;;; The following variable controls whether any information about
;;; compilation (except warnings and error messages) are printed.

(define *infomessages-flag* #t)

;;; The following variables control whether all map-s and for-each-s
;;; are converted into inline-do-loops, or map-s and for-each-s
;;; taking only one list are compiled as any other higher-order call
;;; to functions map1 and for-each1 (inserted by the compiler in case
;;; of need).

(define *always-map->do-flag* #f)
(define *always-for-each->do-flag* #f)


;================================================================
;
;		 C-specific and system-specific options
;                       (change if needed)
;
;===============================================================

;;; If your C compiler does not assume that integers without a cast
;;; are long ints, you may need to set the following flag to #t.
;;; In that case all integers in the output C text, which should
;;; be long ints, will have a trailing L cast.

(define *long-cast-flag* #f)

;;; If your C compiler may compile the C operator <test> ? <r1> : <r2>
;;; to the code which may evaluate BOTH <r1> and <r2> in one
;;; evaluation of the whole operator, you MUST define *lift-ifs-flag*
;;; as #t.

(define *lift-ifs-flag* #f)

;;; If you C compiler may compile the C operator <a1> || <a2>
;;; to the code which may evaluate <a2> even if <a1> evaluates to 1,
;;; or, analogically, <a1> && <a2> may evaluate <a2> even if
;;; <a1> evaluates to 0, you MUST define *lift-and-or-flag* as #t.

(define *lift-and-or-flag* #f)

;;; The following flag may be false only if the output C program
;;; is supposed to run only on systems where the following holds:
;;; ((-1%2 == -1) && (-1%-2 == -1) && (1%2 == 1) && (1%-2 == 1).
;;; Otherwise the following flag must be #t.

(define *badivsgns-flag* #f)

;;; *input-file-modifier* and *output-file-modifier*
;;; are strings which are given to the C file-opener to
;;; indicate the mode of the file to be opened.
;;; Select the MSDOS or ATARI version if appropriate, or define
;;; your own modifier-strings.

(define *input-file-modifier* "r")     ;;; for UNIX & others
(define *output-file-modifier* "w")    ;;; for UNIX & others

;;; (define *input-file-modifier* "rb")    ;;; for MSDOS & ATARI
;;; (define *output-file-modifier* "wb")   ;;; for MSDOS & ATARI

;;; The following variable controls the maximal length of auxiliary
;;; functions created by the compiler (longer functions are split
;;; into separate chunks).

(define *max-auxfun-size* 50)

;====================================================================
;
;  Scheme-implementation-specific definitions. Change if needed.
;
;====================================================================

(define (report-error . lst)
  (display #\newline)
  (display "COMPILATION ERROR: ")
  (display #\newline)
  (for-each display lst)
  (display #\newline)
  (abort))
;@
(define compile-allnumbers #t)

;=================================================================
;
;			renamable constants
;	       (you might need to change some of these to
;			avoid name clashes)
;
;=================================================================


;;;  If your scheme file contains symbols which start
;;;  with a number, then *c-num-symb-prefix* is prefixed to
;;;  such symbols in the C source.

(define *c-num-symb-prefix* "nonum_prefix_")

;;; NB! If your scheme file contains variables which are also
;;;     C keywords or C functions defined in scm,
;;;     the string *c-keyword-postfix* is added to such variable names.
;;;     The list of prohibited variables is *c-keywords*. Add new
;;;     variables there, if needed.

(define *c-keyword-postfix* "_nonkeyword")

(define *c-keywords*
  '(auto double int struct break else long switch
    case enum register typedef char extern return union
    const float short unsigned continue for signed void
    default goto sizeof volatile do if static while
    system random exit			; Added by M.Ward

;;; Some things are commented out to make hobbit compile itself correctly.

 sizet void cell subr iproc smobfuns dblproc flo dbl isymnames s-and
 s-begin s-case s-cond s-do s-if s-lambda s-let s-letstar s-letrec s-or
 s-quote s-set i-dot i-quote i-quasiquote i-unquote i-uq-splicing
 tcs-cons-imcar tcs-cons-nimcar tcs-cons-gloc tcs-closures tcs-subrs
 tc7-asubr tcs-symbols tc7-ssymbol tcs-bignums tc16-bigpos tc3-cons
 tc3-cons-gloc tc3-closure tc7-ssymbol tc7-msymbol tc7-string
 tc7-vector tc7-Vbool
 tc7-VfixZ32 tc7-VfixN32 tc7-VfixZ16 tc7-VfixN16 tc7-VfixZ8 tc7-VfixN8
 tc7-VfloR32 tc7-VfloC32 tc7-VfloR64 tc7-VfloC64
 tc7-contin tc7-cclo tc7-asubr
;;; tc7-subr-0 tc7-subr-1
 tc7-cxr
;;; tc7-subr-3 tc7-subr-2
 tc7-subr-2x tc7-subr-1o tc7-subr-2o tc7-lsubr-2
;;; tc7-lsubr
 tc7-smob tc-free-cell tc16-flo tc-flo tc-dblr tc-dblc
 tc16-bigpos tc16-bigneg tc16-port tc-inport tc-outport tc-ioport
 tc-inpipe tc-outpipe smobfuns numsmob sys-protects cur-inp cur-outp
 listofnull undefineds nullvect nullstr symhash progargs transcript
 def-inp def-outp rootcont sys-protects upcase downcase symhash-dim
 heap-size stack-start-ptr heap-org freelist gc-cells-collected
 gc-malloc-collected gc-ports-collected cells-allocated linum
 errjmp-ok ints-disabled sig-deferred alrm-deferred han-sig han-alrm
 must-malloc ilength s-read s-write s-newline s-make-string
 s-make-vector s-list s-string s-vector repl-driver newsmob lthrow repl
 gc-end gc-start growth-mon scm_iprin1 scm_intprint scm_iprlist lputc lputs
 lfwrite time-in-msec my-time init-tables init-storage init-subrs
 init-features init-iprocs init- init-scl init-io init-repl init-time
 init-signals ignore-signals unignore-signals init-eval init-sc2
 free-storage init-unif uvprin1 markcdr free0 warn wta everr sysintern
;;; intern
 sym2vcell makstr
;;; make-subr
;;; makfromstr
 closure makprom force
 makarb tryarb relarb ceval prolixity gc gc-for-newcell tryload cons2
;;; acons
 resizuve cons2r lnot booleanp eq equal consp cons nullp
 setcar setcdr listp list length append reverse list-ref memq memv
 member assq assv assoc symbolp symbol2string string2symbol numberp exactp
 inexactp eqp lessp zerop positivep negativep oddp evenp scm_max scm_min sum
 product difference lquotient scm_abs remainder lremainder modulo lgcd llcm
 number2string
;;; string2number
 makdbl istr2flo mkbig long2big dbl2big
 ilong2str iflo2str floprint bigprint int2dbl charp char-lessp chci-eq
 chci-lessp char-alphap char-nump char-whitep char-upperp char-lowerp
 char2int int2char char-upcase char-downcase stringp make-string
 string st-length st-ref st-set st-equal stci-equal st-lessp
 stci-lessp substring st-append vectorp make-vector
 vector
 vector-length vector-ref vector-set for-each procedurep apply map
 call-cc copytree
;;; eval
 throwval quit input-portp output-portp
 cur-input-port cur-output-port open-file open-pipe close-port
 close-pipe read-char peek-char eof-objectp scm_write scm_display
 scm_newline scm_write-char
 file-position file-set-position scm_file-position scm_getenv prog-args
 makacro makmacro makmmacro
 remove ash round array-ref array_ref
 sin cos tan asin acos atan sinh cosh tanh asinh acosh atanh sqrt expt
 log abs exp
 ;; verbose copy-tree @copy-tree
 last-pair subml submr subfl		;from sc2.c
))


;;; NB! Your scheme file must not contain symbols which end with
;;;     the third elements of the following defines appended
;;;     with an integer. The same holds for the case where "-" is written
;;;     instead of "_". In case your scheme file contains any offending
;;;     symbols, replace them either in your file or replace the offending
;;;     strings in the following defines.
;;;
;;;     E.g. it is not allowed to have symbols like: my__12, spec-tmp-var3,
;;;     foo-inst1, foo_inst5, bar-aux2.
;;;
;;;     E.g. it is allowed to have symbols like: my__x, spec_tmp_var,
;;;     foo-inst1x, foo_inst_5, bar-aux-spec.

(define *local-var-infix* "__")
(define *new-var-name* "new_var")
(define *tmp-var-name* "tmp_var")
(define *new-parameter-prefix* "npar__")
(define *new-fun-infix* "_aux")
(define *new-letfun-infix* "_fn")
(define *new-instfun-infix* "_inst")
(define *new-constant-prefix* "const_")
(define *closure-name-suffix* "_cl")
(define *closure-vector-name* "clargsv_")

;;; The following are names for the additional scheme functions
;;; nonkeyword-make-promise and nonkeyword-force.
;;; If your scheme file contains a function
;;; with these names already, you must change the following names.

(define *make-promise-function* 'nonkeyword_make-promise)
(define *force-function* 'nonkeyword_force)

;;; The following two will be names for the additional scheme functions
;;; map1 and for-each1. If your scheme file contains any functions with
;;; such names already, you must change the following names.

(define *map1-function* 'map1)
(define *for-each1-function* 'for-each1)

;;; The following name is not allowed to occur in your scheme file,
;;; neither is <name>_<nr> allowed.

(define *new-closure-var* "newclosure")

;;; The following is appended to symbols (not variables!) in your scheme
;;; file. Thus your scheme file should not contain variables or symbols
;;; ending with the value of *symbol-name-postfix*. If needed, change
;;; *symbol-name-postfix* from "_symb" to some other string.

(define *symbol-name-postfix* "_symb")

;;; The following is appended to higher-order function names in your scheme
;;; file which should be accessible from the interpreter. Thus your scheme
;;; file should not contain variables or symbols
;;; ending with the value of *export-hof-postfix*. If needed, change
;;; *export-hof-postfix* from "_exporthof" to some other string.

(define *export-hof-postfix* "_exporthof")

;;; The following is needed for exportable functions which do not
;;; have a type available in scm and need a special wrapper-function
;;; to pass variables supplied by the interpreter. The wrapper function
;;; name for some function foo is foo_wrapper, unless you change
;;; the following define.

(define *wrapper-postfix* "_wrapper")

;;; The following is appended to those function names in your scheme
;;; which are passed in the file to functions defined out of file
;;; or to append: in other words, passed to interpreter

(define *interpreter-suffix* "_interpreter")

;;; The following is appended to names of stable vectors, to
;;; denote the precalculated VELTS(x) part of a stable vector x.

(define *st-vector-postfix* "_velts0")

;;; The following is appended to names of closure procedures, giving
;;; the C-only static SCM variable name

(define *closure-proc-suffix* "_clproc0")

;;; The following is a string which is prepended to the name of your
;;; scheme file (without .scm) to form a name of a function generated
;;; to initialize non-function defined variables in your scheme file.

(define *init-globals-prefix* "init_globals_")

;;; The following is a string which is prepended to the name of your
;;; scheme file (without .scm) to form a name of a function generated
;;; to perform all top-level computations in your scheme file.

(define *top-actions-prefix* "top_actions_")

;;; The following is a string which is prepended to the name of your
;;; scheme file (without .scm) to form a name of a main initialization
;;; function for your file.

(define *init-fun-prefix* "init_")

;;; The following is a name of a variable which may be defined to
;;; the list of inlinable functions in your scheme file.

(define *inline-declare* 'compile-inline)

;;; The following is a name of a variable which may be defined to
;;; the list of inlinable variables in your scheme file.

(define *inline-vars-declare* 'compile-inline-vars)

;;; The following is a name of a variable which has to be defined to
;;; make hobbit compile numeric procedures for all numbers as default,
;;; not just integers:

(define *allnumbers-declare* 'compile-allnumbers)

;;; The following is a name of a variable which has to be defined to
;;; make hobbit assume all procedures may be redefined.

(define *all-funs-modified-declare* 'compile-all-proc-redefined)

;;; The following is a name of a variable which has to be defined to
;;; make hobbit assume all procedures may be redefined.

(define *new-funs-modified-declare* 'compile-new-proc-redefined)

;;; The following is a name of a variable which may be defined to
;;; the list of exportable functions in your scheme file.

(define *export-declare* 'compile-export)

;;; The following is a name of a variable which may be defined to
;;; the list of stable vector names (never-assigned except the first
;;; initialization, not even by let or as local variables) in your
;;;  scheme file.

(define *stable-vectors-declare* 'compile-stable-vectors)

;;; The following is a name of a variable which may be defined to
;;; the list of uninterned fast global vars (never holding nonimmediate values,
;;; ie not char, bool or short int). These vars are NOT accessible
;;; by the interpreter! They are used directly as C vars, without the GLOBAL
;;; (ie * op) prefix.

(define *fast-vars-declare* 'compile-uninterned-variables)

;;; The following two are default names for the single argument
;;; of the closure function and the variable which is assigned its
;;; first element.

(define *closurefun-arg* 'closurearg_0)
(define *closurefun-arg-car* 'closurearg_car_0)

;;; NB! The following determine the replacements for symbols
;;;     allowed in scheme variables but not in C variables.
;;;     Be careful with your scheme variables to avoid
;;;     name clashes! E.g. if you have scheme variables
;;;     bar--plus_, bar-+ and bar_+, they will all be converted to
;;;     the same C variable bar__plus_
;;;     In case of need feel free to change the replacement table.
;;;     You may also wish to change the scheme function
;;;     display-c-var, which performs the conversion.
;;;
;;;     *global-postfix* determines the string to be appended to
;;;     variable names surrounded by *-s. The surrounding *-s
;;;     are dropped. E.g. *special-flag* will be converted to
;;;     special_flag_global
;;;     *char-replacements* determine the replacement strings
;;;     for characters not allowed in C variables. E.g. foo!?
;;;     will be converted to foo_excl__pred_

(define *global-postfix* "_global")

(define *char-replacements*
  '((#\+ "_plus_")
    (#\- "_")
    (#\@ "_at_")
    (#\. "_dot_")
    (#\* "_star_")
    (#\/ "_slash_")
    (#\\ "_backsl_"); Added by M.Ward:
    (#\< "_less_")
    (#\= "_equal_")
    (#\> "_grtr_")
    (#\! "_excl_")
    (#\? "_pred_")
    (#\: "_colon_")
    (#\$ "_dollar_")
    (#\% "_percent_")
    (#\_ "_")
    (#\& "_and_")
    (#\~ "_tilde_")
    (#\^ "_exp_")
    (#\[ "_obrckt_")
    (#\] "_cbrckt_")
    (#\| "_vbar_")))

;;; *c-indent* is the one-level indentation for C statements.
;;; There is no indentation for C expressions.

(define *c-indent* "  ")

;;; *c-infix-surround* is put before and after each infix C operator.
;;; The sensible alternative to default "" is " " or #\space.

(define *c-infix-surround* "")

;;; The following are some obvious C constants. *c-null* is the
;;; C object corresponding to scheme '().

(define *c-true* 1)
(define *c-false* 0)
(define *c-null* "EOL")
(define *scm-type* "SCM")
(define *unspecified* '**unspecified**) ; you may change it

;;; NB! Your scheme file must not contain any third symbols
;;;     of the following defines. If it does, replace the
;;;     offending symbol either in your file or in the following
;;;     defines (the compiler must contain the replacement anywhere
;;;     else).

(define *function* '**function**)
(define *higher-order-call* '**higher-order-call**)
(define *higher-order-flag* #f)
(define *dummy* '**dummy**)
(define *not?* '**not?**)
(define *and?* '**and**)
(define *or?* '**or**)
(define *open-file-function* '**open-file-function**)
(define *set-current-input-port-function*
  '**set-current-input-port-function**)
(define *set-current-output-port-function*
  '**set-current-output-port-function**)
(define *num-s->c* '**num-s->c**)
(define *num-c->s* '**num-c->s**)
(define *bool-s->c* '**bool-s->c**)
(define *bool-c->s* '**bool-c->s**)
(define *char-c->s* '**char-c->s**)
(define *float-c->s* '**float-c->s**)
(define *tailrec* '**tailrec**)
(define *c-fetch* '**c-fetch**)
(define *c-adr* '**c-adr**)
(define *op-if* '**op-if**)
(define *op-begin* '**op-begin**)
(define *op-let* '**op-let**)
(define *do-not* '**do-not**)
(define *return* '**return**) ; NB! do not change this!!!
(define *goto-tailrec* '**goto-tailrec**)
(define *mark-tailrec* '**mark-tailrec**)
(define *define-constant* '**define-constant**)
(define *actual-c-string* '**actual-c-string**)
(define *actual-c-expr* '**actual-c-expr**)
(define *actual-c-int* '**actual-c-int**)
(define *actual-c-eval* '**actual-c-eval**)
(define *special-pseudoquote* '**special-pseudoquote**)
(define *global-access* '**global-access**)
(define *sysapply* '**sysapply**)
(define *listofnull* '**listofnull**)
(define *velts-function* '**velts-function**)
(define *st-vector-set* '**st-vector-set**)
(define *st-vector-ref* '**st-vector-ref**)
(define *make-cclo* '**make-cclo**)


(define *special-scm->c-functions*
 (list
*function*
*higher-order-call*
*dummy*
*not?*
*and?*
*or?*
*open-file-function*
*set-current-input-port-function*
*set-current-output-port-function*
*num-s->c*
*num-c->s*
*bool-s->c*
*bool-c->s*
*char-c->s*
*float-c->s*
*tailrec*
*c-fetch*
*c-adr*
*op-if*
*op-begin*
*op-let*
*do-not*
*return*
*goto-tailrec*
*mark-tailrec*
*define-constant*
*actual-c-string*
*actual-c-int*
*actual-c-eval*
*special-pseudoquote*
*global-access*
*listofnull*
*velts-function*
*st-vector-set*
*st-vector-ref*
*sysapply*
*make-cclo*
*unspecified*))


;;; *intern-function* must be a C function taking a C string
;;; and its length (C int) which builds a new scheme symbol
;;; and returns it.
;;; *makfromstr-function* must be a C function taking a C string
;;; and its length (C int) which builds a new scheme string
;;; and returns it.
;;; *string->number-function* must be a C function taking a scheme string
;;; and a radix (scheme int) which builds a new scheme number
;;; and returns it.
;;; Instead of using such special functions it is possible to
;;; change the compiler functions make-symbol-constant and
;;; make-string-constant instead.

(define *intern-function* 'intern)
(define *intern-symbol-function* 'intern)
(define *makfromstr-function* 'makfromstr)
(define *string->number-function* 'string2number)
(define *c-eval-fun* 'eval)

(define *internal-c-functions*
  (list *intern-function* *makfromstr-function*
	 *intern-symbol-function* *string->number-function* *c-eval-fun*))

(define *prohibited-funs* '())

;;; *type-converters* is a list of scheme<->C representation
;;; converters.

(define *type-converters*
  (list *num-s->c* *num-c->s* *bool-s->c* *bool-c->s*
	*char-c->s* *float-c->s*))

;;; The following four defines specify functions which will either
;;;	take or return (or both) C numbers or booleans. They
;;;     are actually set in set-primitive-tables.
;;;
;;; *num-arg-c-funs* is a set of scheme functions which will be
;;;	converted to analogous C functions (provided
;;;	*reckless-arithmetic* is #t) and which take C numbers
;;;	as arguments.
;;; *num-res-c-funs* is a set of scheme functions which will
;;;	converted to analogous C functions (provided
;;;	*reckless-arithmetic* is #t) and which give C numbers
;;;	as results.
;;; *bool-arg-c-funs* is a set of scheme functions which will always be
;;;	converted to analogous C functions
;;;	and which take C booleans (int 0 or non-0) as arguments.
;;; *bool-res-c-funs* is a set of scheme functions which will be
;;;	converted to analogous C functions (some only if
;;;	*reckless-arithmetic* is #t) and which give C booleans
;;;	as results.

(define *num-arg-c-funs* '())
(define *always-num-arg-c-funs* '())
(define *num-res-c-funs* '())
(define *bool-arg-c-funs* '())
(define *always-bool-res-c-funs* '())
(define *bool-res-c-funs* '())

;;; cxr-functions is a set of allowed cxr functions. You may
;;;  extend it if you wish.

(define *cxr-funs*
  '(car cdr
    caar cadr cdar cddr
    caaar caadr cadar caddr cdaar cdadr cddar cdddr
    caaaar caaadr caadar caaddr cadaar cadadr caddar cadddr
    cdaaar cdaadr cdadar cdaddr cddaar cddadr cdddar cddddr))

;=================================================================
;
;		      global variable defs
;
;=================================================================

;;; the following variable determines whether floats or ints are used

(define *floats-flag* #f)    ; must be #f in this version

;;; The following variables control error-checking performed by
;;; the resulting C program and numerical operations.

(define *reckless-arithmetic-flag* #t) ; MUST be #t in this version
(define *reckless-access-flag* #t)     ; MUST be #t in this version

;;; The following variable controls optimizations of integer
;;; arithmetic for scheme<->C type conversions.

(define *optimize-arithmetic* #f)      ; MUST be #f in this version

(define *splitted-init-function-names* '())
(define *splitted-topaction-function-names* '())
(define *map1-needed-flag* #f)
(define *for-each1-needed-flag* #f)
(define *inline-funs* '())
(define *inline-vars* '())
(define *top-actions-list* '())
(define *inline-funs-data* '())
(define *inline-vars-data* '())
(define *c-port* '())
(define *char-replacements-lists* '())
(define *tmp-var-max* 500)
(define *initial-defs* '())
(define *passed-defs* '())
(define *output-defs* '())
(define *new-funs-list* '())
(define *fun-arities-alist* '())
(define *to-do-fun-list* '())
(define *via-interpreter-defined* '())
(define *non-directcomp-list* '())
(define *current-fun-name* 'foo)
(define *current-formal-args* '())
(define *current-formal-argslist* '())
(define *tailrec-flag* #f)
(define *tmp-vars* '())
(define *new-fun-nr* 0)
(define *new-fun-names* '())
(define *higher-ordr-flag* #f)
(define *higher-order-args* '())
(define *higher-order-funs* '())
(define *higher-order-templates* '())
(define *new-parameter-nr* '0)
(define *make-new-ho-data* '())
(define *dot-arg-funs* '())
(define *dot-arg-templates* '())
(define *new-instnr* '0)
(define *new-primitive-instnr* '0)
(define *local-vars* '())
(define *new-constant-list* '())
(define *symbol-constant-table* '())
(define *interpreter-funname-table* '())
(define *new-constant-num* 0)
(define *passed-ho-dot-instfuns* '())
(define *passed-closure-funs* '())
(define *free-vars-list* '())
(define *global-vars-list* '())
(define *var-make-list* '())
(define *symbol-list* '())
(define *unknown-functions* '())
(define *unknown-vars* '())
(define *local-parameters* '())
(define *top-level-funs* '())
(define *export-functions* '())
(define *export-table* '())
(define *wrapper-table* '())
(define *stable-vector-names* '())
(define *fast-vars-list* '())
(define *closure-var-vectornames* '())
(define *lifted-closures-to-do* '())
(define *lifted-trivial-closure-names* '())
(define *lifted-closure-names* '())
(define *liftable-hof-names* '())
(define *non-liftable-hof-names* '())
(define *special-c-vars* '())
(define *closure-name-nr* 0)
(define *closure-vector-name-nr* 0)
(define *liftable-hof-database* '())
(define *letrec-closure-nr* 0)
(define *letrec-closures* '())
(define *letrec-closure-init* '())
(define *not-all-liftable-names* '())
(define *all-funs-modified-flag* #f)
(define *new-funs-modified-flag* #f)
(define *primitives* '())
(define *symbol-and-fun-list* '())
(define *hobbit-declaration-vars* '())

;;; the definition of force is used in case 'delay' is
;;; found inside the program

(define *force-definition*
  (list
   'define
   *force-function*
   '(lambda (object) (object))))


;;; the definition of make-promise is used in case 'delay' is
;;; found inside the program

(define *make-promise-definition*
  (list
   'define
   *make-promise-function*
   '(lambda (proc)
      (let ((result-ready? #f)
	    (result #f))
	(lambda ()
	  (if result-ready?
	      result
	      (let ((x (proc)))
		(if result-ready?
		    result
		    (begin (set! result-ready? #t)
			   (set! result x)
			   result)))))))))



;;; a word of warning: the following two defs must not contain any
;;; of the following: (cond, case, not, or, and, let, letrec, map, for-each)
;;; and must not contain lambda-terms or clashing variables in let*.
;;; There might be other analogous restrictions as well!

(define *map1-definition*
  (list 'define
	*map1-function*
	`(lambda (fn lst)
	   (let* ((res '()) (res-end res))
	     (do ()
		 ((,*not?* (pair? lst)) res)
		 (if (null? res)
		     (begin (set! res (cons (fn (car lst)) '()))
			    (set! res-end res))
		     (begin (set-cdr! res-end (cons (fn (car lst)) '()))
			    (set! res-end (cdr res-end))))
		 (set! lst (cdr lst)))))))


(define *for-each1-definition*
  (list 'define
	*for-each1-function*
	`(lambda (fn lst)
	   (do ()
	       ((,*not?* (pair? lst)) ,*unspecified*)
	       (fn (car lst))
	       (set! lst (cdr lst))))))

;=================================================================
;
;			     top level
;
;=================================================================

;@ exported symbol hobbit.
(define (hobbit file . files)
  (let* ((tmpname "hobbit.tmp"))
    (if *build-intermediate-files*
        (begin (require 'pretty-print)))
    (if *expand-macros-flag*
        (begin (require 'defmacroexpand)
	       (require 'pprint-file)))
    (provide 'hobbit)
    (if (or (member '"scmhob.scm" (cons file files))
	    (member '"scmhob" (cons file files)))
	(report-error "The file scmhob.scm is not allowed to be compiled!"))
    (init-global)
    ;; check for defmacros
    (if *expand-macros-flag*
        (if (not (find-if (lambda (x) (file-contains-defmacro? x))
			  (cons file files)))
            (set! *expand-macros-flag* #f)))
    (set! *initial-defs* '())
    (if *expand-macros-flag* (for-each defmacro:load (cons file files)))
    (for-each (lambda (x)
		(if *infomessages-flag*
                    (begin (newline)
		           (display "Starting to read ") (display x)))
		(read-compiled-file x tmpname))
	      (cons file files))
    (if *infomessages-flag* (newline))
    (compile-defs file (reverse *initial-defs*))))

(define (file-contains-defmacro? str)
  (let ((foundflag #f)
        (expr '())
        (port (if (file-exists? str)
	          (open-input-file str)
	          (if (file-exists? (string-append str ".scm"))
		      (open-input-file (string-append str ".scm"))
		      (report-error "Could not find file " str)))))
    (if port
	(do ()
	    ((or foundflag (eof-object? expr)) foundflag)
	  (set! foundflag (expr-contains-defmacro? expr))
	  (set! expr (read port)))
	#f)))

(define (expr-contains-defmacro? expr)
  (cond ((not (pair? expr)) #f)
        ((or (eq? 'quote (car expr)) (eq? 'quasiquote (car expr))) #f)
        ((eq? 'defmacro (car expr)) #t)
        (else (pair-find-if (lambda (x) (expr-contains-defmacro? x)) expr))))


(define (read-compiled-file file tmpname)
  (let* ((iport (if (file-exists? file)
		    (open-input-file file)
		    (if (file-exists? (string-append file ".scm"))
			(open-input-file (string-append file ".scm"))
			(report-error "Could not find file " file))))
	 (oport (if *expand-macros-flag* (open-output-file tmpname) '()))
	 (def #t))
    (if *infomessages-flag* (newline))
    (if *expand-macros-flag*
	(begin
	  (if *infomessages-flag*
	      (begin
		(display "Starting macroexpansion building the ")
		(display "temporary file ")
		(display tmpname) (display #\.) (newline)))
	  (pprint-filter-file iport defmacro:expand* oport)
	  (close-output-port oport)
	  (set! iport (open-input-file tmpname))))
    (do ()
	((eof-object? def)
	 (close-input-port iport))
      (set! def (read iport))
      (cond ((eof-object? def))
	    ((and (pair? def)
		  (or (eq? 'load (car def))
		      (eq? 'require (car def))))
	     (report-warning "ignoring a load on top level: " def))
	    (else
	     (set! *initial-defs* (cons def *initial-defs*)))))))

(define (compile-defs file deflst)
  (let ()
    (set! file (descmify file))
    ;; - - - - adding primitives delay and force, if neccessary - - - - -
    (if (find-if (lambda (x) (in-fun-position? 'delay x)) deflst)
	(set! deflst
	      (append deflst
		      (append
		       (list *force-definition*)
		       (list *make-promise-definition*)))))
    ;; - - - - - initial checks and flag-setting, sorting out the toplevel
    ;;  builds *top-level-names*, *modified-primitives* and
    ;;  *modified-top-level-names*:
    (make-top-level-namelist! deflst)
    ;; sorts out the toplevel:
    (sort-out-toplevel! deflst file)
    (if (not *floats-flag*)
        (compute-floats-flag! deflst #t))
    (if *infomessages-flag*
        (if *floats-flag*
	    (begin
              (display "Generic (slow) arithmetic assumed: ")
              (display *floats-flag*)
              (display " found.")
	      (newline))
	    (begin
	      (display "Bounded integer (fast) arithmetic assumed.")
	      (newline))))
    (set-primitive-tables)
    (set! *passed-defs* '())
    ;; - - - - - vars-simplification pass - -
    (set! *to-do-fun-list*
	  (map vars-simplify-wholedef *to-do-fun-list*))
    ;; - - - - - finding liftable hof-s - - -
    (set! *liftable-hof-names* '())
    (set! *non-liftable-hof-names* '())
    (for-each (lambda (x)
		(if (and (pair? (caddr x))
			 (eq? 'lambda (caaddr x))
			 (liftable-hof? (caddr x) (cadr x)))
		    (set! *liftable-hof-names*
			  (cons (cadr x) *liftable-hof-names*))))
	      *to-do-fun-list*)
    (for-each lift-analyse-def! *to-do-fun-list*)
    (if *infomessages-flag*
        (begin (newline) (display "** Pass 1 completed **")))
    (if *build-intermediate-files*
	(let ((fport (open-output-file (string-append file '".anl"))))
	  (for-each (lambda (x) (pretty-print x fport) (newline fport))
		    *to-do-fun-list*)
	  (close-output-port fport)
	  (newline)
	  (display "analyzed & marked definitions file ")
	  (display (string-append file '".anl"))
	  (display " is built.")))
    ;; initial analysis passes completed
    ;; - - - - - building closures - - -  - - - - -
    (do ((part *to-do-fun-list* part))
	((null? part))
      (set! *lifted-closures-to-do* '())
      (set! *passed-defs*
	    (cons (try-closure-making-def(car part)) *passed-defs*))
      (set! part (append *lifted-closures-to-do* (cdr part))))
    (set! *to-do-fun-list* (reverse *passed-defs*))
    (for-each lift-unmark-def! *to-do-fun-list*)
    (if *infomessages-flag*
        (begin (newline) (display "** Pass 2 completed **")))
    (if *build-intermediate-files*
	(let ((fport (open-output-file (string-append file '".cls"))))
	  (for-each (lambda (x) (pretty-print x fport) (newline fport))
		    *to-do-fun-list*)
	  (close-output-port fport)
	  (newline)
	  (display "closures-building file ")
	  (display (string-append file '".cls"))
	  (display " is built.")))
    ;; closurebuilding pass completed
    ;; - - - - - - - -   flattening starts  - - - - - - - - -
    (set! *passed-defs* '())
    (for-each (lambda (def)
		(set! *passed-defs*
		      (append (reverse (flatten-wholedef def))
			      *passed-defs*)))
	      *to-do-fun-list*)
    (if (not (or (pair? *export-functions*) (null? *export-functions*)))
	(set! *export-functions* *top-level-funs*)
	(set! *export-functions*
	      (intersection *export-functions* *top-level-funs*)))
    (if *map1-needed-flag*
	(set! *passed-defs* (cons *map1-definition* *passed-defs*)))
    (if *for-each1-needed-flag*
	(set! *passed-defs* (cons *for-each1-definition* *passed-defs*)))
    (set! *passed-defs* (reverse *passed-defs*))
    (if *infomessages-flag*
	(begin (newline) (display "** Pass 3 completed **")))
    (if *build-intermediate-files*
	(let ((fport (open-output-file (string-append file '".flt"))))
	  (for-each (lambda (x) (pretty-print x fport) (newline fport))
		    *passed-defs*)
	  (close-output-port fport)
	  (newline)
	  (display "lambda-lifted & normalized definitions file ")
	  (display (string-append file '".flt"))
	  (display " is built.")))
    (set! *to-do-fun-list* *passed-defs*)
    ;; lambda-lifting & normalization finished
    ;; - - - - - - - - - - lift statements - - - - - - - - - - -
    (set! *passed-defs* '())
    (do ((x 1 1))
	((null? *to-do-fun-list*))
      (let ((tmp (car *to-do-fun-list*)))
	(set! *to-do-fun-list* (cdr *to-do-fun-list*))
	(set! *passed-defs* (append (lift-statements-wholedef tmp)
				    *passed-defs*))))
    (set! *passed-defs* (reverse *passed-defs*))
    (if *infomessages-flag*
	(begin (newline) (display "** Pass 4 completed **")))
    (if *build-intermediate-files*
	(let ((fport (open-output-file (string-append file '".stt"))))
	  (for-each (lambda (x) (pretty-print x fport) (newline fport))
		    *passed-defs*)
	  (close-output-port fport)
	  (newline)
	  (display "statement-lifted definitions file ")
	  (display (string-append file '".stt"))
	  (display " is built.")))
    (set! *to-do-fun-list* *passed-defs*)
    ;; statement-lifting pass finished
    ;; - - - - - - - hof-dot-corrections starts - - - - - -
    (set! *passed-ho-dot-instfuns* '())
    (set! *passed-defs* '())
    (do ((x 1 1))
	((null? *to-do-fun-list*))
      (let ((tmp (car *to-do-fun-list*)))
	(set! *to-do-fun-list* (cdr *to-do-fun-list*))
	(if (not (memq (cadr tmp) *passed-ho-dot-instfuns*))
	    (set! *passed-defs* (cons (ho-dot-wholedef tmp)
				      *passed-defs*)))))
    (set! *passed-defs*
	  (reverse (append (build-wrappers *passed-defs*)
			   *passed-defs*)))
    (build-wrapped-interpreter-table)
    (if *infomessages-flag*
	(begin (newline) (display "** Pass 5 completed **")))
    (if *build-intermediate-files*
	(let ((fport (open-output-file (string-append file '".hod"))))
	  (for-each (lambda (x) (pretty-print x fport) (newline fport))
		    *passed-defs*)
	  (close-output-port fport)
	  (newline)
	  (display "higher-order-&-dot-arglist corrected definitions file ")
	  (display (string-append file '".hod"))
	  (display " is built.")))
    (set! *to-do-fun-list* *passed-defs*)
    ;; hof-dot correction finished
    ;; - - - - - - - typing & constant-correcting - - - - - - - - - -
    (set! *passed-defs* '())
    (do ((x 1 1))
	((null? *to-do-fun-list*))
      (let ((tmp (car *to-do-fun-list*)))
	(set! *to-do-fun-list* (cdr *to-do-fun-list*))
	(set! *passed-defs* (cons (type-const-wholedef tmp)
				  *passed-defs*))))

    (for-each (lambda (x)
		(let ((tmp (assq x *extra-hobbit-primitive-defs*)))
		  (if (and tmp (not (memq x *modified-primitives*)))
		      (begin
			(set! *passed-defs*
			      (cons (type-const-wholedef
				     (list 'define x (cadr tmp)))
				    *passed-defs*))
			(if (memq x *extra-hobbit-dot-primitives*)
			    (set! *dot-arg-templates*
				  (cons (list x 'x) *dot-arg-templates*)))))))
	      *unknown-vars*)
    (set! *passed-defs* (reverse *passed-defs*))
    (init-export-funs! file)
    (make-initialization-function! file)
    (if *infomessages-flag*
	(begin (newline) (display "** Pass 6 completed **")))
    (if *build-intermediate-files*
	(let ((fport (open-output-file (string-append file '".typ"))))
	  (for-each (lambda (x) (pretty-print x fport) (newline fport))
		    *passed-defs*)
	  (close-output-port fport)
	  (newline)
	  (display "typing & constants - corrected definitions file ")
	  (display (string-append file '".typ"))
	  (display " is built.")
	  (newline)))
    (set! *to-do-fun-list* *passed-defs*)
    ;; typing & constant-correcting pass finished
    ;; - - - - - - - - building .c and .h files  - - - - - - - - - -
    (set! *passed-defs* '())
    (let ((fport (open-output-file (string-append file '".c"))))
      (display "#include " fport)
      (display #\" fport)
      (display (string-append file '".h") fport)
      (display #\" fport)
      (newline fport)
      (newline fport)
      (for-each (lambda (x) (write-c-wholefun x fport))
		*to-do-fun-list*)
      (close-output-port fport)
      (if *infomessages-flag*
	  (begin (newline) (newline)
		 (display "C source file ")
		 (display (string-append file '".c"))
		 (display " is built.")
		 (newline))))
    (let ((fport (open-output-file (string-append file '".h"))))
      (display-header fport)
      (newline fport)
      (for-each (lambda (x)
		  (write-fun-declaration (cadr x) fport))
		*to-do-fun-list*)
      (for-each (lambda (x)
		  (if (not (memq x *fast-vars-list*))
		      (write-c-*declaration x fport)))
		*global-vars-list*)
      (for-each (lambda (x)
		  (write-c-*declaration (cdr x) fport))
		*interpreter-funname-table*)
      (for-each (lambda (x)
		  (write-c-*declaration (make-closure-scmobj-name x) fport))
		*symbol-and-fun-list*)
      (for-each (lambda (x)
		  (write-c-static-declaration
		   (make-closure-scmobj-name x) fport))
		*lifted-trivial-closure-names*)
      (for-each (lambda (x)
		  (write-c-static-declaration
		   (make-closure-scmobj-name x) fport))
		*lifted-closure-names*)
      (for-each (lambda (x) (write-c-static-declaration (cadr x) fport))
		(reverse *symbol-constant-table*))
      (for-each (lambda (x)
		  (if (not (pair? (cadr x)))
		      (write-c-static-declaration (cadr x) fport)))
		(reverse *new-constant-list*))
      (for-each (lambda (x)
		  (write-c-*declaration
		   (string->symbol
		    (string-append (symbol->string x) *st-vector-postfix*))
		   fport))
		(reverse *stable-vector-names*))
      (for-each (lambda (x) (write-c-static-declaration x fport))
		(reverse *fast-vars-list*))
      (newline fport)
      (close-output-port fport)
      (if *infomessages-flag*
	  (begin
	    (display "C header file ")
	    (display (string-append file '".h"))
	    (display " is built.")
	    (newline))))
    ;; .c and .h files built
    ;; - - - - - - - - - extra compilation info: - - - - - - - - -
    (set! *via-interpreter-defined*
	  (append *via-interpreter-defined*
		  (map car *switch-args-table*)
		  *cxr-funs*
		  *non-compiled-primitives*
		  *interpreter-defined-vars*
		  (map car *floats-s->c-fun-table*)))
    ;; - - - - - - - - - redefinability info: - - - - - - - - -
    (if *infomessages-flag* (newline))
    (cond
     ((not *infomessages-flag*))
     (*all-funs-modified-flag*
      (newline)
      (display "All procedure names are assumed to be redefinable (slow).")
      (newline))
     (*new-funs-modified-flag*
      (newline)
      (display
       "All new procedure names are assumed to be redefinable (slow).")
      (newline)
      (if (not (null? *modified-primitives*))
	  (begin
	    (display
	     "These primitive procedure names are assumed to be redefinable (slow):")
	    (newline)
	    (display *modified-primitives*)
	    (newline))))
     (else
      (if (not (null? *modified-primitives*))
	  (begin
	    (display
	     "These primitive procedure names are assumed to be redefinable (slow):")
	    (newline)
	    (display *modified-primitives*)
	    (newline)))
      (if (not (null? (set-difference *modified-top-level-names*
				      (union
				       *global-vars-list* *fast-vars-list*))))
	  (begin
	    (display
	     "These top level procedure names are assumed to be redefinable (slow):")
	    (newline)
	    (display (set-difference *modified-top-level-names*
				     (union
				      *global-vars-list* *fast-vars-list*)))
	    (newline)))))
    ;; - - - - - - - - - hof-info: - - - - - - - - - - - - - -
    (cond
     ((not *infomessages-flag*))
     ((not (null? *non-liftable-hof-names*))
      (display
       "These top level higher order procedures are not clonable (slow):")
      (newline)
      (display *non-liftable-hof-names*)
      (newline)))
    ;; - - - - - - - - - closures-info: - - - - - - - - - - - - -
    (cond
     ((not *infomessages-flag*))
     ((not (null? *not-all-liftable-names*))
      (display
       "These top level procedures create non-liftable closures (slow):")
      (newline)
      (display *not-all-liftable-names*)
      (newline)))
    ;; - - - - - - - - - undefined-info: - - - - - - - - - - -
    (if (and *infomessages-flag*
             (not (null?  (set-difference (set-difference *unknown-functions*
							  *modified-top-level-names*)
					  (union *global-vars-list*
						 (union *fast-vars-list*
							*via-interpreter-defined*))))))
	(begin (newline)
	       (display
		"These nonprimitive procedures are assumed to be defined externally:")
	       (newline)
	       (display
		(set-difference (set-difference *unknown-functions*
						*modified-top-level-names*)
				(union *global-vars-list*
				       (union *fast-vars-list*
					      *via-interpreter-defined*))))
	       (newline)))
    (if (and *infomessages-flag*
             (not (null? (set-difference (set-difference *unknown-vars*
							 *modified-top-level-names*)
					 *via-interpreter-defined*))))
	(begin (newline)
	       (display
		"These variables undefined (but used) in your file were defined:")
	       (newline)
	       (display (set-difference (set-difference *unknown-vars*
							*modified-top-level-names*)
					*via-interpreter-defined*))
	       (newline)))
    (if *infomessages-flag* (newline))))

(define (sort-out-toplevel! lst file)
  (set! *to-do-fun-list* '())
  (set! *inline-funs* '())
  (set! *inline-vars* '())
  (set! *global-vars-list* '())
  (set! *fast-vars-list* '())
  (set! *var-make-list* '())
  (set! *non-directcomp-list* '())
  (set! *top-actions-list* '())
  (do ((part lst (cdr part)))
      ((null? part))
    (let ((el (car part))
	  (tmp '()))
      (cond
       ((and (list? el)
	     (eq? 'begin (car el)))
	(set! part (append el (cdr part))))
       ((and (pair? el)
	     (or (eq? 'load (car el))
		 (eq? 'require (car el))))
	(report-warning "ignoring a load on top level: " el))
       ((or (not (pair? el))
	    (not (eq? 'define (car el)))
	    (null? (cdr el))
	    (not (list? el)))
	;; (report-error "the compiled file contains a non-definition: "
	;;		   el)
	;;  (if (pair? el)
	;;	 (set! *non-directcomp-list* (cons el *non-directcomp-list*)))

	(set! *top-actions-list* (cons el *top-actions-list*)))

       ;; from here everything starts with 'define'.

       ((or (pair? (cadr el))		; the standard direct function def
	    (and (not (null? (cddr el)))
		 (pair? (caddr el))
		 (eq? 'lambda (car (caddr el)))))
	;;(and (pair? (cddr el))
	;;	    (pair? (caddr el))
	;;	    (memq (car (caddr el)) '(let let* letrec))
	;;	    (pair? (cddr (caddr el)))
	;;	    (pair? (caddr (caddr el)))
	;;	    (eq? 'lambda (car (caddr (caddr el)))))
	;;	    ;(not (find-if (lambda (x) (not (eq? (car x) (cadr x))))
	;;	    ;	       (cadr (caddr el))))
	(let* ((def (normalize-top-define el))
	       (funname (cadr def))
	       (tmp '()))
	  (if (modified-fun? funname)
	      (set! *top-actions-list*
		    (cons (cons 'set! (cdr def)) *top-actions-list*))
	      (begin
		(set! tmp (list *special-pseudoquote* funname))
		(set! *top-actions-list* (cons tmp *top-actions-list*))
		(set! *to-do-fun-list* (cons def *to-do-fun-list*))))))
       ;; the following filters out macro defs:
       ((and (pair? el)
	     (pair? (cdr el))
	     (eq? 'define (car el))
	     (not (pair? (cadr el)))
	     (pair? (cddr el))
	     (pair? (caddr el))
	     (eq? 'let (caaddr el))
	     (pair? (car (my-last-pair (caddr el))))
	     (eq? 'defmacro:transformer (caar (my-last-pair (caddr el))))))

       ;; - - - from here everything will be a define-expression - - - -

       ;;((and (pair? (caddr el))
       ;;	(not (eq? 'quote (car (caddr el))))
       ;;	(not (eq? 'quasiquote (car (caddr el)))))

       ;;   (set! *top-actions-list*
       ;;	   (cons (cons 'set! (cdr el)) *top-actions-list*)))

       ;;(set! tmp (make-pair-constant (caddr el)))
       ;;(set! *top-actions-list*
       ;;	   (cons (list 'set! (cadr el) (list *actual-c-eval* tmp))
       ;;		 *top-actions-list*))
       ;; (set! *via-interpreter-defined*
       ;;	   (cons (cadr el) *via-interpreter-defined*))

       ;; - - - - - - - - - declarations-part starts - - - - - - - - -
       ((eq? (cadr el) *inline-declare*)
	(set! *inline-funs* (append (cadr (caddr el)) *inline-funs*)))
       ((eq? (cadr el) *inline-vars-declare*)
	(set! *inline-vars* (append (cadr (caddr el)) *inline-vars*)))
       ((eq? (cadr el) *allnumbers-declare*)
	(set! *floats-flag* el))
       ((eq? (cadr el) *all-funs-modified-declare*)
	(set! *all-funs-modified-flag* #t))
       ((eq? (cadr el) *new-funs-modified-declare*)
	(set! *new-funs-modified-flag* #t))
       ((eq? (cadr el) *stable-vectors-declare*)
	(set! *stable-vector-names*
	      (append (cadr (caddr el))
		      *stable-vector-names*)))
       ((eq? (cadr el) *fast-vars-declare*)
	(set! *fast-vars-list*
	      (append (cadr (caddr el)) *fast-vars-list*)))
       ((eq? (cadr el) *export-declare*)
	(set! *export-functions*
	      (append (cadr (caddr el))
		      (if (pair? *export-functions*)
			  *export-functions*
			  '()))))
       ;; - - - - - - - - -declarations-part ends - - - - - - - - - -
       ((null? (cddr el))		; form: (define foo)
	(set! *global-vars-list* (cons (cadr el) *global-vars-list*))
	(set! *top-actions-list*
	      (cons (list 'set! (cadr el) *unspecified*)
		    *top-actions-list*))
	(if (not (memq (cadr el) *fast-vars-list*))
	    (set! *var-make-list*
		  (cons `(set!
			  ,(cadr el)
			  (,*c-adr* (cdr (,*intern-function*
					  (,*actual-c-string*
					   ,(symbol->string (cadr el)))
					  ,(string-length
					    (symbol->string (cadr el)))))))
			*var-make-list*))))

       (else				; form: (define foo <term>)
	(set! *global-vars-list* (cons (cadr el) *global-vars-list*))
	(set! *top-actions-list*
	      (cons (cons 'set! (cdr el)) *top-actions-list*))

	;;(if (symbol? (caddr el))
	;;	 ; the last el of define is a symbol; call intern:
	;;	 (set! *top-actions-list*
	;;	       (cons `(set!
	;;                   ,(cadr el)
	;;		       ,(list *actual-c-eval*
	;;			      (make-pair-constant-aux (caddr el))))
	;;(,*c-adr* (cdr (,*intern-function*
	;;		       (,*actual-c-string*
	;;			,(symbol->string (caddr el)))
	;;		       (,*actual-c-int*
	;;			,(string-length
	;;			  (symbol->string
	;;			   (caddr el)))))))
	;;     *top-actions-list*))
	;;	 ; the last el of define is a non-list non-symbol:
	;;	 (set! *top-actions-list*
	;;	       (cons (cons 'set! (cdr el))
	;;		     *top-actions-list*)))
	;;(set! *via-interpreter-defined*
	;;	   (cons (cadr el) *via-interpreter-defined*))
	(if (not (memq (cadr el) *fast-vars-list*))
	    (set! *var-make-list*
		  (cons `(set!
			  ,(cadr el)
			  (,*c-adr* (cdr (,*intern-function*
					  (,*actual-c-string*
					   ,(symbol->string (cadr el)))
					  ,(string-length
					    (symbol->string (cadr el)))))))
			*var-make-list*)))))))
  ;;(if (not (null? *top-actions-list*))
  ;;    (set! *to-do-fun-list*
  ;;	    (cons (list 'define
  ;;			(make-globals-name file)
  ;;			(list* 'lambda
  ;;			       '()
  ;;			       (reverse (cons '() *top-actions-list*))))
  ;;		  *to-do-fun-list*)))

  ;; - - - - - - - - making the top-actions-fun - - - - - - -- - -

  (let* ((init-all-list (reverse *top-actions-list*))
	 (init-all-splitted-lists (list '()))
	 (fname '())
	 (init-all-splitted-processed '())
	 (top-actions-fun '())
	 (split-nr 0))

    ;; split up the big list
    (do ((n 1 (+ n 1)))
	((null? init-all-list))
      (if (> n *max-auxfun-size*)
	  (begin
	    (set! n 1)
	    (set! init-all-splitted-lists
		  (cons '() init-all-splitted-lists))))
      (set! init-all-splitted-lists
	    (cons (cons (car init-all-list) (car init-all-splitted-lists))
		  (cdr init-all-splitted-lists)))
      (set!  init-all-list (cdr init-all-list)))

    ;;(display "init-all-splitted-lists: ")
    ;;(newline)
    ;;(pretty-print init-all-splitted-lists)
    ;;(newline)
    (set! init-all-splitted-lists (reverse init-all-splitted-lists))
    ;; process each sublist
    (do ((lst init-all-splitted-lists (cdr lst)))
	((null? lst))
      (set! split-nr (+ 1 split-nr))
      (set! fname
	    (string->symbol
	     (string-append  *top-actions-prefix*
			     (string-append
			      (number->string split-nr)
			      "_"
			      file))))
      (set! *splitted-topaction-function-names*
	    (cons fname *splitted-topaction-function-names*))
      (set!  init-all-splitted-processed
	     (cons
	      (list 'define
		    fname
		    (list 'lambda
			  '()
			  (list* 'let* '() (reverse (car lst)))))
	      init-all-splitted-processed)))

    ;;(display "init-all-splitted-processed: ")
    ;;(newline)
    ;;(pretty-print init-all-splitted-processed)
    ;;(newline)

    (set! top-actions-fun
	  (list
	   'define (make-top-actions-funname file)
	   (list* 'lambda '()
		  (map list
		       (reverse *splitted-topaction-function-names*)))))

    (set! *to-do-fun-list*
	  (cons top-actions-fun
		(append init-all-splitted-processed
			*to-do-fun-list*))))

  ;; - - - - - - - - top-actions-fun is made and kept - - - - - - -
  (set! *to-do-fun-list* (reverse *to-do-fun-list*))
  (set! *non-directcomp-list* (reverse *non-directcomp-list*))
  (set! *inline-funs-data* '())
  (set! *inline-vars-data* '())
  (do ((part *inline-vars* (cdr part)))
      ((null? part))
    (let ((tmp (member-if (lambda (x)
			    (and (pair? x)
				 (eq? 'set! (car x))
				 (eq? (cadr x) (car part))))
			  *top-actions-list*)))
      (if tmp
	  (set! *inline-vars-data*
		(cons (cdar tmp) *inline-vars-data*))
	  (set! *inline-vars* (remove (car part) *inline-vars*)))))
  (do ((part *inline-funs* (cdr part)))
      ((null? part))
    (let ((tmp (member-if (lambda (x)
			    (or (eq? (cadr x) (car part))
				(and (pair? (cadr x))
				     (eq? (caadr x) (car part)))))
			  *to-do-fun-list*)))
      (if tmp
	  (set! *inline-funs-data*
		(cons (list (car part)
			    (make-inline-body (car tmp)))
		      *inline-funs-data*))
	  (set! *inline-funs* (remove (car part) *inline-funs*))))))



(define (normalize-top-define term)
  (if (or (not (pair? (cdr term)))
	  (not (pair? (cddr term))))
      (report-error "incorrect define: " term))
  (if (pair? (cadr term))
      `(define ,(caadr term) (lambda ,(cdadr term) ,@(cddr term)))
      term))

(define (make-inline-body def)
  (let* ((tmp (rename-vars
	       (lettify-lambdas
		(normalize-defines
		 (compile-quasiquote def))
		200
		#t)))
	 (term (caddr tmp))
	 (body (cddr term)))
    (cond ((not (list? (cadr term)))
	   (report-error "inline-function has a non-list arglist: "
			 def))
	  ((null? body)
	   (report-error "inline-function has no body: " def))
	  ((null? (cdr body))
	   term)
	  (else
	   (list (car term)
		 (cadr term)
		 (cons 'begin body))))))


(define (make-initialization-function! file)
  (let* ((nondefines
	  (map make-pair-constant *non-directcomp-list*))
	 (vector-elts
	  (map (lambda (x)
		 `(set! ,(string->symbol
			  (string-append
			   (symbol->string x)
			   *st-vector-postfix*))
			(,*velts-function*
			 (,*global-access* ,x))))
	       *stable-vector-names*))
	 (init-all-list
	  (append
	   (init-closure-funs file *passed-defs*)
	   (init-interpretable-funs)
	   *var-make-list*
	   (reverse *symbol-list*)
	   (reverse *new-constant-list*)
	   (if (null? *top-actions-list*)
	       '()
	       (list
		(list (make-top-actions-funname file))))
	   vector-elts
	   (map (lambda (x)
		  (list *c-eval-fun* x))
		nondefines)))
	 (init-all-splitted-lists (list '()))
	 (init-all-splitted-processed '())
	 (split-nr 0)
	 (main-fun '())
	 (fname '()))


    ;;(display "init-all-list: ")
    ;;(newline)
    ;;(pretty-print init-all-list)
    ;;(newline)

    ;; split up the big list
    (do ((n 1 (+ n 1)))
	((null? init-all-list))
      (if (> n *max-auxfun-size*)
	  (begin
	    (set! n 1)
	    (set! init-all-splitted-lists
		  (cons '() init-all-splitted-lists))))
      (set! init-all-splitted-lists
	    (cons (cons (car init-all-list) (car init-all-splitted-lists))
		  (cdr init-all-splitted-lists)))
      (set!  init-all-list (cdr init-all-list)))

    ;;(display "init-all-splitted-lists: ")
    ;;(newline)
    ;;(pretty-print init-all-splitted-lists)
    ;;(newline)
    (set! init-all-splitted-lists (reverse init-all-splitted-lists))
    ;; process each sublist
    (do ((lst init-all-splitted-lists (cdr lst)))
	((null? lst))
      (set! split-nr (+ 1 split-nr))
      (set! fname
	    (string->symbol
	     (string-append *init-fun-prefix*
			    (string-append
			     (number->string split-nr)
			     "_"
			     file))))
      (set! *splitted-init-function-names*
	    (cons fname *splitted-init-function-names*))
      (set!  init-all-splitted-processed
	     (cons
	      (list 'define
		    fname
		    (list 'lambda
			  '()
			  (list* 'let* '() (reverse (car lst)))))
	      init-all-splitted-processed)))

    ;;(display "init-all-splitted-processed: ")
    ;;(newline)
    ;;(pretty-print init-all-splitted-processed)
    ;;(newline)

    (set! main-fun
	  (list 'define
		(string->symbol (string-append *init-fun-prefix* file))
		(list 'lambda
		      '()
		      (list* 'let* '()
			     '(set! no-symhash-gc #t)
			     (map list
				  (reverse *splitted-init-function-names*))))))

    (set! *passed-defs*
	  (append *passed-defs*
		  (append (reverse init-all-splitted-processed)
			  (list main-fun))))))


(define (init-export-funs! file)
  (let* ((res '())
	 (topactions-funname (make-top-actions-funname file)))
    (set! *export-functions*
	  (remove (make-globals-name file) *export-functions*))
    (for-each
     (lambda (x)
       (set! res (init-export-fun-aux x))
       (if res
	   (for-each
	    (lambda (name)
	      (let ((fun (car (member-if
			       (lambda (x) (eq? (cadr x) name))
			       *passed-defs*))))
		(subst-term-equal!
		 res (list *special-pseudoquote* (cadr x)) fun)))
	    (cons topactions-funname
		  *splitted-topaction-function-names*))))
     *passed-defs*)))


(define (init-export-fun-aux def)
  (if (not (memq (cadr def) *export-functions*))
      #f
      (let* ((tmp1 (assq (cadr def) *export-table*))
	     (tmp (assq (cadr def) *wrapper-table*))
	     (arity '())
	     (flag '())
	     (res '()))
	(cond (tmp (set! arity 'x))
	      ((begin
		 (set! arity (assq (cadr def) *dot-arg-templates*))
		 (and arity
		      (symbol? (cadr arity))))
	       (set! arity 'x))
	      (else (set! arity (cadr (caddr def)))))
	(cond ((symbol? arity)
	       (set! flag 'tc7_lsubr))
	      (else
	       (set! flag
		     (cadr (assq (length arity)
				 '((0 tc7_subr_0)
				   (1 tc7_subr_1)
				   (2 tc7_subr_2)
				   (3 tc7_subr_3)))))))
	(set! res
	      (list 'make_subr
		    (list *actual-c-string*
			  (if (memq (cadr def) *symbol-and-fun-list*)
			      (symbol->string
			       (make-closure-scmobj-name (cadr def)))
			      (symbol->string (cadr def))))
		    flag
		    (if tmp
			(cadr tmp)
			(if tmp1
			    (cadr tmp1)
			    (cadr def)))))
	(if (memq (cadr def) *symbol-and-fun-list*)
	    (set! res `(set! (,*global-access*
			      ,(make-closure-scmobj-name (cadr def)))
			     ,res)))
	res)))


(define (init-closure-funs file defs)
  (append
   (map
    (lambda (funname)
      (let* ((procname (make-closure-scmobj-name funname))
	     (def-part '()))
	(set! def-part (member-if (lambda (x) (eq? funname (cadr x))) defs))
	`(set! ,procname ,(init-export-fun-aux (car def-part)))))
    *lifted-trivial-closure-names*)
   (map
    (lambda (funname)
      (let* ((procname (make-closure-scmobj-name funname)))
	`(set! ,procname (make_subr (,*actual-c-string*
				     ,(symbol->string procname))
				    tc7_lsubr
				    ,funname))))
    *lifted-closure-names*)))


(define (init-interpretable-funs)
  (map
   (lambda (x)
     (list 'set! (cdr x)
	   `(,*c-adr* (cdr (,*intern-function*
			    (,*actual-c-string*
			     ,(symbol->string (car x)))
			    ,(string-length
			      (symbol->string (car x))))))))
   *interpreter-funname-table*))


(define (make-globals-name file)
  (string->symbol (string-append *init-globals-prefix* file)))

(define (make-top-actions-funname file)
  (string->symbol (string-append *top-actions-prefix* file)))

(define (descmify str)
  (let ((len (string-length str)))
    (if (and (> len 4)
	     (string-ci=? ".scm" (substring str (- len 4) len)))
	(substring str 0 (- len 4))
	str)))




(define (display-header fport)
  (define *h-port* fport)
  (define (headerline s)
    (display s *h-port*)
    (newline *h-port*))
  (if *floats-flag* (headerline "#define FLOATS"))
  (headerline "#include \"scmhob.h\"")
  (headerline ""))

(define (init-global)
   (set! *floats-flag* #f)
   (set! *tmp-vars* '())
   (set! *new-fun-names* '())
   (set! *new-fun-nr* 0)
   (set! *higher-order-funs* '())
   (set! *higher-order-templates* '())
   (set! *new-parameter-nr* '0)
   (set! *dot-arg-funs* '())
   (set! *dot-arg-templates* '())
   (set! *new-instnr* '0)
   (set! *new-primitive-instnr* '0)
   (set! *new-constant-list* '())
   (set! *symbol-constant-table* '())
   (set! *interpreter-funname-table* '())
   (set! *new-constant-num* 0)
   (set! *char-replacements-lists* '())
   (set! *splitted-init-function-names* '())
   (set! *splitted-topaction-function-names* '())
   (set! *map1-needed-flag* #f)
   (set! *for-each1-needed-flag* #f)
   (set! *symbol-list* '())
   (set! *unknown-functions* '())
   (set! *unknown-vars* '())
   (set! *top-level-funs* '())
   (set! *inline-funs* '())
   (set! *inline-vars* '())
   (set! *export-functions* #f)
   (set! *export-table* '())
   (set! *wrapper-table* '())
   (set! *stable-vector-names* '())
   (set! *fast-vars-list* '())
   (set! *closure-var-vectornames* '())
   (set! *lifted-closures-to-do* '())
   (set! *lifted-trivial-closure-names* '())
   (set! *lifted-closure-names* '())
   (set! *via-interpreter-defined* '())
   (set! *special-c-vars* '())
   (set! *closure-name-nr* 0)
   (set! *closure-vector-name-nr* 0)
   (set! *liftable-hof-database* '())
   (set! *letrec-closure-nr* 0)
   (set! *not-all-liftable-names* '())
   (set! *all-funs-modified-flag* #f)
   (set! *new-funs-modified-flag* #f)
   (set! *symbol-and-fun-list* '())
   (set! *hobbit-declaration-vars*
	 (list *inline-declare* *inline-vars-declare* *allnumbers-declare*
	       *all-funs-modified-declare* *new-funs-modified-declare*
	       *export-declare* *stable-vectors-declare* *fast-vars-declare*))
   (set! *primitives*
	 (append (map car *switch-args-table*)
		 *cxr-funs*
		 *non-compiled-primitives*
		 (map car *floats-s->c-fun-table*)))
   (do ((nr 1 (+ 1 nr)))
       ((= nr *tmp-var-max*))
       (set! *tmp-vars*
	     (cons (string->symbol (string-append  *tmp-var-name*
						   (number->string nr)))
		   *tmp-vars*)))
   (set! *tmp-vars* (reverse *tmp-vars*)))


;; set-primitive-tables sets tables differently for the float and non-float case

(define (set-primitive-tables)
 (set! *num-arg-c-funs*
       (append
	(if *badivsgns-flag*
	    '()
	    '(quotient remainder))
	(if *floats-flag*
	    '()
	    '(/))
	'(logxor lognot logsleft logsright
		 = < > <= >= + - *
		 %= %< %> %<= %>= %+ %- %* %/)))
 (set! *always-num-arg-c-funs*
	;;if *badivsgns-flag*
	;;   '()
	;;   '(quotient remainder))
	'(logxor lognot logsleft logsright
		 %= %< %> %<= %>= %+ %- %* %/))
 (set! *num-res-c-funs*
       (append
	(if *badivsgns-flag*
	    '()
	    '(quotient remainder))
	(if *floats-flag*
	    '()
	    '(/))
       '(logxor lognot logsleft logsright
		+ - *
		%+ %- %* %/)))
 (set! *bool-arg-c-funs*
       (cons *and?* (cons *or?* (list *not?*))))
 (set! *always-bool-res-c-funs*
       (cons *and?*
	     (cons *or?*
		   (cons *not?*
			 '(boolean? symbol? char? vector? pair?
			    string? number? complex?
			    eq? char=? null?
			    %eqv? %zero? %negative? %positive? %number?
			    %= %< %> %<= %>= )))))
 (set! *bool-res-c-funs*
       (cons *and?*
	     (cons *or?*
		   (cons *not?*
		      '(boolean? symbol? char? vector? pair?
		       string? number? real? rational? complex?
		       integer?
		       eq? eqv? char=? null? zero? negative? positive?
		       = < > <= >=
		       %eqv? %zero? %negative? %positive? %number?
		       %= %< %> %<= %>= ))))))


(define (report-warning . lst)
  (display #\newline)
  (display "COMPILER WARNING: ")
  (display #\newline)
  (for-each display lst)
  (display #\newline))

;;=================================================================
;;
;;		  final conversion to C
;;
;;=================================================================

(define (write-c-*declaration var port)
   (set! *c-port* port)
   (display-c *scm-type*)
   (display-c #\space)
   (display-c #\*)
   (display-c-var var)
   (display-c #\;)
   (display-c-newline))

(define (write-fun-declaration var port)
   (set! *c-port* port)
   (display-c *scm-type*)
   (display-c #\space)
   (display-c-var var)
   (display-c "()")
   (display-c #\;)
   (display-c-newline))

(define (write-c-static-declaration var port)
   (set! *c-port* port)
   (display-c "static ")
   (display-c *scm-type*)
   (display-c #\space)
   (display-c-var var)
   (display-c #\;)
   (display-c-newline))


(define (write-c-wholefun def port)
  (let* ((fun (caddr def))
	 (top-let (caddr fun)))
    (set! *c-port* port)
    (set! *current-fun-name* (cadr def))
    (display-c *scm-type*)
    (display-c #\space)
    (display-c-var (cadr def))
    (display-c-lst (args->list (cadr fun)) #\( #f)
    (display-c-newline)
    (if (not (null? (cadr fun)))
	(begin
	  (let ((scm-args (filter (lambda (x) (symbol? x)) (cadr fun)))
		(fun-args (filter (lambda (x)
				    (and (pair? x) (eq? *function* (car x))))
				  (cadr fun)))
		(ptr-args (filter (lambda (x)
				    (and (pair? x) (eq? *c-adr* (car x))))
				  (cadr fun))))
	    (if (not (null? ptr-args))
		(begin
		  (display-c *scm-type*)
		  (display-c #\space)
		  (display-c-lst (map cadr ptr-args) #f #\*)
		  (display-c #\;)
		  (display-c-newline)))
	    (if (not (null? fun-args))
		(begin
		  (display-c *scm-type*)
		  (display-c #\space)
		  (display-c-lst (map cadr fun-args) #f 'function)
		  (display-c #\;)
		  (display-c-newline)))
	    (if (not (null? scm-args))
		(begin
		  (display-c *scm-type*)
		  (display-c #\space)
		  (display-c-lst scm-args #f #f)
		  (display-c #\;)
		  (display-c-newline))))))
    (display-c #\{)
    (display-c-newline)
    (if (and (not (null? (cadr top-let)))
	     (find-if (lambda (x) (symbol? (car x))) (cadr top-let)))
	(begin
	  (display-c-indent 1)
	  (display-c *scm-type*)
	  (display-c #\space)
	  (display-c-lst (filter (lambda (x) (symbol? x))
				 (map car (cadr top-let)))
			 #f #f)
	  (display-c #\;)
	  (display-c-newline)
	  (display-c-newline)))
    (for-each (lambda (x)
		(display-c-statement x 1))
	      (cddr top-let))
    (display-c #\})
    (display-c-newline)
    (display-c-newline)))

(define (display-c x)
   (display x *c-port*))


;;(define (write-c-string x)
;;   (write x *c-port*))

(define (write-c-string x)
  (display "\"" *c-port*)
  (for-each
   (lambda (c)
     (cond
      ((eq? c #\nl) (display "\\n" *c-port*))
      ((eq? c #\")  (display "\\\"" *c-port*))
      ((eq? c #\ht) (display "\\t" *c-port*))
      ((eq? c #\\)  (display "\\\\" *c-port*))
      (else (display c *c-port*))))
   (string->list x))
  (display "\"" *c-port*))


(define (display-c-newline)
   (newline *c-port*))

(define (display-c-indent n)
  (do ((m 0 (+ 1 m)))
      ((= n m))
      (display-c *c-indent*)))

(define (display-c-lst lst par prefix)
 (let ((separator #\,))
  (cond ((char=? par #\()
	    (set! separator #\,)
	    (display-c #\())
	((char=? par #\{)
	    (set! separator #\;)
	    (display-c #\{))
	(else
	    (set! separator #\,)))
  (if (not (null? lst))
      (begin
	(for-each (lambda (x)
		    (cond ((and (pair? x)
				(eq? 'set! (car x))
				(eq? 3 (length x))
				(eq? *dummy* (caddr x))))
		          ((or (char? prefix) (string? prefix))
			     (display-c prefix)
			     (display-c-expression x #t)
			     (display-c separator))
			  ((eq? 'function prefix)
			     (display-c "(*")
			     (display-c-expression x #t)
			     (display-c ") ()")
			     (display-c separator))
			  (else
			     (display-c-expression x #t)
			     (display-c separator))))
		  (butlast lst 1))
	(cond ((or (char? prefix) (string? prefix))
		 (display-c prefix)
		 (display-c-expression (car (my-last-pair lst)) #t))
	      ((eq? 'function prefix)
		 (display-c "(*")
		 (display-c-expression (car (my-last-pair lst)) #t)
		 (display-c ") ()"))
	      (else
		 (display-c-expression (car (my-last-pair lst)) #t)))))
  (cond ((char=? par #\()
	    (display-c #\)))
	((char=? par #\{)
	    (display-c #\;)
	    (display-c #\})))))




(define (display-var var port)
 (cond
  ((eq? *listofnull* var)
     (display "listofnull" port))
  ((eq? *unspecified* var)
     (display "UNSPECIFIED" port))
  (else
   (let* ((str (symbol->string var))
	  (char '())
	  (replacement '())
	  (len (string-length str))
	  (global-flag #f))
     (if (and (symbol? var)
	      (char-numeric? (string-ref str 0)))
	 (display *c-num-symb-prefix* port))
     (if (and (char=? #\* (string-ref str 0))
	      (char=? #\* (string-ref str (- len 1))))
	 (set! global-flag #t))
     (do ((n 0 (+ 1 n)))
	 ((= n len))
       (set! char (string-ref str n))
       (cond ((and global-flag
		   (or (= 0 n) (= n (- len 1))))
	      char) ; do nothing
	     ((char-alphabetic? char)
	      (display (char-downcase char) port))
	     ((char-numeric? char)
	      (display char port))
	     ((begin
		(set! replacement (assoc char *char-replacements*))
		replacement)
	      (display (cadr replacement) port))
	     (else
	      (display char port))))
     (cond ((memq var *c-keywords*)
	    (display *c-keyword-postfix* port))
	   (global-flag
	    (display *global-postfix* port)))))))


(define (display-c-var var)
  (display-var var *c-port*))


(define (display-c-statement term n)
  (let ()
    (cond ((not (pair? term)))
	  ;;   (display-c-indent n)
	  ;;   (display-c #\;) ; empty operator
	  ;;   (display-c-newline))
	  ((eq? 'if (car term))
	   (display-c-indent n)
	   (display-c "if (")
	   (display-c-expression (cadr term) #t)
	   (display-c #\))
	   (cond ((not (pair? (caddr term)))
		  (display-c #\space)
		  (display-c #\;)	; empty operator
		  (display-c-newline))
		 ((and (not (eq? 'begin (car (caddr term))))
		       (not (eq? 'if (car (caddr term)))))
		  (display-c-newline)
		  (display-c-statement (caddr term) (+ 1 n)))
		 ((eq? 'begin (car (caddr term)))
		  (display-c #\space)
		  (display-c #\{)
		  (display-c-newline)
		  (for-each (lambda (x) (display-c-statement x (+ 1 n)))
			    (cdar (cddr term)))
		  (display-c-indent n)
		  (display-c #\})
		  (display-c-newline))
		 ((eq? 'if (car (caddr term)))
		  (display-c #\space)
		  (display-c #\{)
		  (display-c-newline)
		  (display-c-statement (car (cddr term)) (+ 1 n))
		  (display-c-indent n)
		  (display-c #\})
		  (display-c-newline))
		 (else (report-error "wrong syntax: " term)))
	   (cond ((null? (cdddr term)))	; do nothing
		 ((not (pair? (car (cdddr term))))) ; do nothing
		 ((and (not (eq? 'begin (caar (cdddr term))))
		       (not (eq? 'if (caar (cdddr term)))))
		  (display-c-indent n)
		  (display-c "else")
		  (display-c-newline)
		  (display-c-statement (car (cdddr term)) (+ 1 n)))
		 ((eq? 'begin (caar (cdddr term)))
		  (display-c-indent n)
		  (display-c "else")
		  (display-c #\space)
		  (display-c #\{)
		  (display-c-newline)
		  (for-each (lambda (x) (display-c-statement x (+ 1 n)))
			    (cdar (cdddr term)))
		  (display-c-indent n)
		  (display-c #\})
		  (display-c-newline))
		 ((eq? 'if (caar (cdddr term)))
		  (display-c-indent n)
		  (display-c "else")
		  (display-c-newline)
		  (display-c-statement (car (cdddr term)) n))))
	  ((eq? (car term) *do-not*)
	   (display-c-indent n)
	   (display-c "for (")
	   (let ((lst1 (map (lambda (x) (list 'set! (car x) (cadr x)))
			    (cadr term)))
		 (lst2 (map (lambda (x) (list 'set! (car x) (caddr x)))
			    (filter (lambda (y) (not (null? (cddr y))))
				    (cadr term)))))
	     (if (not (null? lst1))
		 (display-c-lst lst1 #f #f))
	     (display-c #\;)
	     (if (> (length lst1) 1)
		 (begin
		   (display-c-newline) (display-c-indent n) (display-c "    ")))
	     (display-c-expression (caar (cddr term)) #t)
	     (display-c #\;)
	     (if (and (> (length lst1) 1) (not (null? lst2)))
		 (begin
		   (display-c-newline) (display-c-indent n) (display-c "    ")))
	     (if (not (null? lst2))
		 (display-c-lst lst2 #f #f))
	     (display-c #\))
	     (cond ((or (null? (cdddr term))
			(not (find-if (lambda (x) (pair? x)) (cdddr term))))
		    (display-c #\space)
		    (display-c #\;)	; empty operator
		    (display-c-newline))
		   ((null? (cdr (cdddr term)))
		    (if (or (eq? 'begin (caar (cdddr term)))
			    (eq? *op-begin* (caar (cdddr term))))
			(begin
			  (display-c #\space)
			  (display-c #\{)
			  (display-c-newline)
			  (for-each (lambda (x)
				      (display-c-statement x (+ 1 n)))
				    (cdar (cdddr term)))
			  (display-c-indent n)
			  (display-c #\})
			  (display-c-newline))
			(begin
			  (display-c-newline)
			  (display-c-statement (car (cdddr term))
					       (+ 1 n)))))
		   (else
		    (display-c #\space)
		    (display-c #\{)
		    (display-c-newline)
		    (for-each (lambda (x)
				(display-c-statement x (+ 1 n)))
			      (cdddr term))
		    (display-c-indent n)
		    (display-c #\})
		    (display-c-newline)))))
	  ((or (eq? (car term) 'begin) (eq? (car term) *op-begin*))
	   (display-c-indent n)
	   (display-c #\{)
	   (display-c-newline)
	   (for-each (lambda (x) (display-c-statement x (+ 1 n)))
		     (cdr term))
	   (display-c-indent n)
	   (display-c #\})
	   (display-c-newline))
	  ((eq? (car term) *return*)
	   (display-c-indent n)
	   (display-c "return ")
	   (display-c-expression (cadr term) #t)
	   (display-c #\;)
	   (display-c-newline))
	  ((or (eq? *tailrec* (car term)) (eq? *mark-tailrec* (car term)))
	   (display-c "tailrecursion:")
	   (display-c-newline))
	  ((eq? *goto-tailrec* (car term))
	   (display-c-indent n)
	   (display-c "goto tailrecursion;")
	   (display-c-newline))
	  ((and (eq? 'set! (car term))
		(eq? *dummy* (caddr term)))) ; do nothing
	  (else
	   (display-c-indent n)
	   (display-c-expression term)
	   (display-c #\;)
	   (display-c-newline)))))

(define (display-c-expression term . no-par-flag)
  (let ((fn (if (pair? term) (car term) '()))
	(args (if (pair? term) (cdr term) '()))
	(tmp #f))
   (cond
     ((symbol? term)
	(display-c-var term))
     ((number? term)
	(display-c term)
	(if *long-cast-flag* (display-c "L")))
     ((boolean? term)
	(if term (display-c *c-true*) (display-c *c-false*)))
     ((char? term)
	(if (printable-char? term)
	    (begin
	      (display-c #\')
	      (display-c term)
	      (display-c #\'))
	    (display-c (char->integer term))))
     ((null? term)
	(display-c *c-null*))
     ((not (pair? term))
	(report-error "wrong type of object for C: " term))
     ((and (eq? *bool-c->s* fn)
	   (boolean? (car args)))
	(if (car args)
	    (display-c "BOOL_T")
	    (display-c "BOOL_F")))
     ((eq? *c-adr* fn)
	(display-c #\&)
	(display-c-expression (car args)))
     ((eq? *c-fetch* fn)
	(display-c #\*)
	(display-c-expression (car args)))
     ((eq? fn *higher-order-call*)
	(display-c "(*")
	(display-c-var (car args))
	(display-c ")")
	(display-c-lst (cdr args) #\( #f))
     ((eq? *function* fn)
	(display-c-expression (car args)))
     ((or (eq? fn 'begin) (eq? fn *op-begin*))
	(display-c-lst args #\( #f))
     ((eq? fn *op-if*)
	(display-c #\()
	(display-c-expression (car args))
	(display-c " ? ")
	(display-c-expression (cadr args))
	(display-c " : ")
	(display-c-expression
	   (if (null? (cddr args))
	       *unspecified*
	       (caddr args)))
	(display-c #\)))
     ((eq? fn *actual-c-string*)
	(display-c "(char *)")
	(write-c-string (car args)))
     ((eq? fn *actual-c-expr*)
	(display-c (car args)))
     ((eq? fn *actual-c-int*)
	(display-c (car args)))
     ((eq? fn *actual-c-eval*)
        (display-c "eval(")
	(display-c-var (car args))
	(display-c ")"))
     ((eq? 'set! fn)
	(or (eq? *dummy* (cadr args))
	    (begin  (display-c-expression (car args))
		    (display-c *c-infix-surround*)
		    (display-c "=")
		    (display-c *c-infix-surround*)
		    (display-c-expression (cadr args)))))
     ((begin (set! tmp (assq fn *switch-args-table*))
	     tmp)
	(display-c-expression (cons (cadr tmp) (reverse args))))
     ((and (begin (set! tmp (assq fn *add-args-table*))
		  tmp)
	   (not (= (length args) (caddr tmp))))
	 (display-c-expression
	       (cons fn (append args (list (cadr tmp))))))

     ((begin (if (memq fn '(vector string))
		 (set! args (list (normalize-list-for-c args))))
	     #f)) ; never succeeds
     ((begin (set! tmp (if *floats-flag*
			   (assq fn *floats-s->c-fun-table*)
			   (assq fn *reckless-s->c-fun-table*)))
	     tmp)
	(cond ((and (not (null? (cdddr tmp)))
		    (car (cdddr tmp)))
		 (if (or (null? no-par-flag)
			 (not (car no-par-flag)))
		     (display-c #\())
		 (display-c-expression (car args))
		 (display-c *c-infix-surround*)
		 (display-c (cadr tmp))
		 (display-c *c-infix-surround*)
		 (display-c-expression (cadr args))
		 (if (or (null? no-par-flag)
			 (not (car no-par-flag)))
		     (display-c #\))))
	      (else
		 (display-c (cadr tmp))
		 (display-c-lst args #\( #f))))
     (else
	(display-c-expression fn)
	(display-c-lst args #\( #f)))))



(define (printable-char? chr)
  (or (char-alphabetic? chr)
      (char-numeric? chr)
      (memq chr '(#\! #\@ #\$ #\% #\^ #\& #\* #\( #\)
		  #\_ #\+ #\| #\- #\=
		  #\{ #\} #\[ #\]
		  #\; #\, #\. #\/
		  #\: #\" #\~ #\< #\> #\?
		  #\space))))


(define *non-compiled-primitives*
  '(apply call-with-current-continuation apply force delay load
	  map for-each list call-with-input-file call-with-output-file
	  open-input-file open-output-file with-input-from-file
	  with-output-to-file string-append
	  defmacro:expand*
	  sin cos tan asin acos atan sinh cosh tanh asinh acosh
	  sin cos tan asin acos atan sinh cosh tanh asinh acosh
	  atanh sqrt expt integer-expt))

(define *interpreter-defined-vars* '())
;; '(slib:features
;;    most-positive-fixnum  most-negative-fixnum))

;; defs in *extra-hobbit-primitive-defs* are used when the extra primitive
;; is passed as an argument.

(define *extra-hobbit-dot-primitives* '(%+ %- %* %/ %= %< %> %<= %>=))

(define *extra-hobbit-primitive-defs*
  '((logsleft  (lambda (x y) (**return** (ash x y))))
    (logsright (lambda (x y) (**return** (ash x (- y)))))
    (%+ (lambda (x)
          (let* ((r 0))
	    (do ((l x (cdr x))) ((null? l) (**return** r))
	      (set! r (%+ r (car l)))))))
    (%- (lambda (x)
          (let* ((r 0))
	    (do ((l x (cdr x))) ((null? l) (**return** r))
	      (set! r (%- r (car l)))))))
    (%* (lambda (x)
          (let* ((r 1))
	    (do ((l x (cdr x))) ((null? l) (**return** r))
	      (set! r (%* r (car l)))))))
    (%/ (lambda (x)
          (let* ((r 1))
	    (do ((l x (cdr x))) ((null? l) (**return** r))
	      (set! r (%/ r (car l)))))))
    (%= (lambda (x)
          (let* ((r #t))
	    (do ((l x (cdr x)))
		((or (not r) (null? l) (null? (cdr l))) (**return** r))
	      (if (not (%= (car l) (cadr l)))
		  (set! r #f))))))
    (%< (lambda (x)
          (let* ((r #t))
	    (do ((l x (cdr x)))
		((or (not r) (null? l) (null? (cdr l))) (**return** r))
	      (if (not (%< (car l) (cadr l)))
		  (set! r #f))))))
    (%> (lambda (x)
          (let* ((r #t))
	    (do ((l x (cdr x)))
		((or (not r) (null? l) (null? (cdr l))) (**return** r))
	      (if (not (%> (car l) (cadr l)))
		  (set! r #f))))))
    (%>= (lambda (x)
	   (let* ((r #t))
	     (do ((l x (cdr x)))
		 ((or (not r) (null? l) (null? (cdr l))) (**return** r))
	       (if (not (%>= (car l) (cadr l)))
		   (set! r #f))))))
    (%<= (lambda (x)
	   (let* ((r #t))
	     (do ((l x (cdr x)))
		 ((or (not r) (null? l) (null? (cdr l))) (**return** r))
	       (if (not (%<= (car l) (cadr l)))
		   (set! r #f))))))))

(define *switch-args-table*
  '((char>? char<?) (char-ci>? char-ci<?)
    (char>=? char<=?) (char-ci>=?  char-ci<=?)
    (string>?  string<?) (string-ci>? string-ci<?)
    (string-ci>=? string-ci<=?) (string>=? string<=?)))

(define *add-args-table*
  (append
   (list
    (list 'make-vector '() 2)
    (list 'number->string (list *num-c->s* 10) 2)
    (list 'string->number (list *num-c->s* 10) 2)
    (list 'make-string (list *actual-c-expr* "MAKICHR(' ')") 2))
   '((quit 1 1)
     (read (current-input-port) 1)
     (read-char (current-input-port) 1)
     (peek-char (current-input-port) 1)
     (write (current-output-port) 2)
     (display (current-output-port) 2)
     (newline (current-output-port) 1)
     (write-char (current-output-port) 2))))

(define *standard-s->c-fun-table*
  (append
   (list (list 'force (symbol->string *force-function*) 1))
   '((%eqv?     "=="       2 #t #t)
     (%zero?    "ZERO_P"    1 #f #t)
     (%positive? "POSITIVE_P" 1 #f #t)
     (%negative? "NEGATIVE_P" 1 #f #t)
     (%=	"=="       2 #t #t)
     (%<	"<"	2 #t #t)
     (%>	">"	2 #t #t)
     (%<=       "<="       2 #t #t)
     (%>=       ">="       2 #t #t)
     (%+	"+"	2 #t #t)
     (%-	"-"	2 #t #t)
     (%*	"*"	2 #t #t)
     (%/	"lquotient"  2 #f #f)
     (cons "cons" 2) (car "CAR" 1) (cdr "CDR" 1)
     (acons "acons" 3)
     (list? "listp" 1) (length "length" 1) (append "append2" 2)
     (reverse "reverse" 1) (list-tail "list_tail" 2) (list-ref "list_ref" 2)
     (memq "memq" 2) (member "member" 2) (memv "memv" 2)
     (assq "assq" 2) (assv "assv" 2) (assoc "assoc" 2)

     (symbol->string "symbol2string" 1) (string->symbol "string2symbol" 1)
     (system "lsystem" 1)
     (verbose "prolixity" 1)
     (copy-tree "copytree" 1)
     (@copy-tree "copytree" 1)

     (exact? "exactp" 1) (inexact? "inexactp" 1)
     (odd? "oddp" 1) (even? "evenp" 1) (max "scm_max" 2) (min "scm_min" 2) (abs "scm_abs" 1)
     (quotient "lquotient" 2) (remainder "lremainder" 2)
     (modulo "modulo" 2) (gcd "lgcd" 2) (lcm "llcm" 2)

     (exact->inexact "EX2IN_FUN" 1) (floor "FLOOR_FUN" 1)
     (ceiling "CEILING_FUN" 1)
     (truncate "TRUNCATE_FUN" 1) (round "ROUND_FUN" 1)
     ($sin "SIN_FUN" 1) ($cos "COS_FUN" 1) ($tan "TAN_FUN" 1)
     ($asin "ASIN_FUN" 1)
     ($acos "ACOS_FUN" 1) ($atan "ATAN_FUN" 1) ($sinh "SINH_FUN" 1)
     ($cosh "COSH_FUN" 1)
     ($tanh "TANH_FUN" 1) ($asinh "ASINH_FUN" 1) ($acosh "ACOSH_FUN" 1)
     ($atanh "ATANH_FUN" 1)
     ($sqrt "SQRT_FUN" 1) ($expt "EXPT_FUN" 2)
     ($log "LOG_FUN" 1) ($abs "ABS_FUN" 1) ($exp "EXP_FUN" 1)
     (real-sin "SIN_FUN" 1) (real-cos "COS_FUN" 1) (real-tan "TAN_FUN" 1)
     (real-asin "ASIN_FUN" 1) (real-acos "ACOS_FUN" 1) (real-atan "ATAN_FUN" 1)
     (real-sinh "SINH_FUN" 1) (real-cosh "COSH_FUN" 1)
     (real-tanh "TANH_FUN" 1) (real-asinh "ASINH_FUN" 1) (real-acosh "ACOSH_FUN" 1)
     (real-atanh "ATANH_FUN" 1)
     (real-sqrt "SQRT_FUN" 1) (real-expt "EXPT_FUN" 2)
     (real-ln "LOG_FUN" 1) (real-exp "EXP_FUN" 1)

     (inexact->exact "in2ex" 1)
     (make-rectangular "makrect" 2) (make-polar "makpolar" 2)
     (real-part "real_part" 1) (imag-part "imag_part" 1)
     (magnitude "scm_magnitude" 1) (angle "angle" 1)

     (number->string "number2string" 2) (string->number "string2number" 1)

     (char<? "CHAR_LESSP" 2) (char<=? "CHAR_LEQP" 2)
     (char-ci=? "CHCI_EQ" 2) (char-ci<? "CHCI_LESSP")
     (char-ci<=? "CHCI_LEQP" 2)

     (char-alphabetic? "CHAR_ALPHAP" 1) (char-numeric? "CHAR_NUMP" 1)
     (char-whitespace? "CHAR_WHITEP" 1) (char-upper-case? "CHAR_UPPERP" 1)
     (char-lower-case? "CHAR_LOWERP" 1)

     (char->integer "CHAR2INT" 1) (integer->char "INT2CHAR" 1)
     (char-upcase "CHAR_UPCASE" 1) (char-downcase "CHAR_DOWNCASE" 1)

     (make-string "make_string" 2)
     (string "string" 1)
     (string-length "ST_LENGTH" 1)
     (string-ref "ST_REF" 2)
     (string-set! "st_set" 3)
     (substring "substring" 3)
     (string-append "st_append" 1)
     (list->string "string" 1)
     (string->list "string2list" 1)
     (string-copy "string_copy" 1)
     (string-fill! "string_fill" 2)
     (string=? "st_equal" 2) (string<? "st_lessp" 2) (string<=? "st_leqp" 2)
     (string-ci=? "stci_equal" 2) (string-ci<? "stci_lessp")
     (string-ci<=? "stci_leqp" 2)

     (make-vector "make_vector" 2)
     (vector "vector" 1)
     (vector-length "VECTOR_LENGTH" 1)

     (vector-ref "vector_ref" 2)
     (vector-set! "vector_set" 3)
     (vector->list "vector2list" 1)
     (list->vector "vector" 1)

     (read "scm_read" 1)
     (read-char "scm_read_char" 1)
     (peek-char "scm_peek_char" 1)
     (eof-object? "eof_objectp" 1)
     (write "scm_write" 2)
     (display "scm_display" 2)
     (newline "scm_newline" 1)
     (write-char "scm_write_char" 2)

     (input-port? "input_portp" 1)
     (output-port? "output_portp" 1)
     (current-input-port "cur_input_port" 0)
     (current-output-port "cur_output_port" 0)
     (close-input-port "close_port" 1)
     (close-output-port "close_port" 1)

     (get-internal-run-time "my_time" 0)
     (quit "quit" 1)
     (abort "abrt" 0)
     (restart "restart" 0)
     (chdir "chdir" 1)
     (delete-file "del_fil" 1)
     (rename-file "ren_fil" 2))))


 ;;; (<s-fn> <c-fn> <nr-args> <infix-flag> <c-flag>)

(define *reckless-s->c-fun-table*
  (append

   (if *badivsgns-flag*
       '()
       '((quotient "/" 2 #t #t)
	 (remainder "%" 2 #t #t)
	 (/  "/" 2 #t #t)))

   (list
    (list *sysapply* "apply" 3 #f #f)
    (list *make-cclo* "makcclo" 2 #f #f)
    (list *global-access* "GLOBAL" 1 #f #f)
    (list *velts-function* "VELTS" 1 #f #f)
    (list *st-vector-ref* "STBL_VECTOR_REF" 2 #f #f)
    (list *st-vector-set* "STBL_VECTOR_SET" 3 #f #f)
    (list *not?*  "!" 1 #f #t)
    (list *and?* "&&"    2 #t #t)
    (list *or?*  "||"    2 #t #t)
    (list *open-file-function* "open_file" 2 #f #f)
    (list *set-current-input-port-function* "set_inp" 1 #f #f)
    (list *set-current-output-port-function* "set_outp" 1 #f #f)
    (list *num-s->c* "INUM" 1 #f #f)
    (list *num-c->s* "MAKINUM" 1 #f #f)
    (list *bool-s->c* "NFALSEP" 1 #f #f)
    (list *bool-c->s* "SBOOL" 1 #f #f)
    (list *char-c->s* "MAKICHR" 1 #f #f))

   '((boolean? "BOOLEAN_P" 1 #f #t)
     (symbol?  "SYMBOL_P"  1 #f #t)
     (char?    "CHAR_P"    1 #f #t)
     (vector?  "VECTOR_P"  1 #f #t)
     (pair?    "PAIR_P"    1 #f #t)
     (number?  "NUMBER_P"  1 #f #t)
     (complex? "NUMBER_P" 1 #f #t)
     (real?    "NUMBER_P"    1 #f #t)
     (rational? "NUMBER_P" 1 #f #t)
     (integer?  "INTEGER_P" 1 #f #t)
     (string?  "STRING_P"  1 #f #t)
     (procedure? "procedurep" 1 #f #t)

     (not      "NOT"     1 #f #f)
     (eq?      "=="       2 #t #t)
     (eqv?     "=="       2 #t #t)
     (char=?   "=="       2 #t #t)
     (null?    "NULL_P"    1 #f #t)
     (zero?    "ZERO_P"    1 #f #t)
     (positive? "POSITIVE_P" 1 #f #t)
     (negative? "NEGATIVE_P" 1 #f #t)

     (logand "&" 2 #t #t)
     (logior "|" 2 #t #t)
     (logxor "^" 2 #t #t)
     (lognot "~" 1 #f #t)
     (logsleft  "<<" 2 #t #t)
     (logsright ">>" 2 #t #t)

     (=	"=="       2 #t #t)
     (<	"<"	2 #t #t)
     (>	">"	2 #t #t)
     (<=       "<="       2 #t #t)
     (>=       ">="       2 #t #t)

     (+	"+"	2 #t #t)
     (-	"-"	2 #t #t)
     (*	"*"	2 #t #t)

     (/	"lquotient"  2 #f #f)
     (set-car! "SET_CAR"   2 #f #t)
     (set-cdr! "SET_CDR"   2 #f #t)
     (vector-set! "VECTOR_SET" 3 #f #t)
     (vector-ref  "VECTOR_REF" 2 #f #t)

     (equal? "equal" 2))

   *standard-s->c-fun-table*))

(define *floats-s->c-fun-table*
  (append

   (list
    (list *sysapply* "apply" 3 #f #f)
    (list *make-cclo* "makcclo" 2 #f #f)
    (list *global-access* "GLOBAL" 1 #f #f)
    (list *velts-function* "VELTS" 1 #f #f)
    (list *st-vector-ref* "STBL_VECTOR_REF" 2 #f #f)
    (list *st-vector-set* "STBL_VECTOR_SET" 3 #f #f)
    (list *not?*  "!" 1 #f #t)
    (list *and?* "&&"    2 #t #t)
    (list *or?*  "||"    2 #t #t)
    (list *open-file-function* "open_file" 2 #f #f)
    (list *set-current-input-port-function* "set_inp" 1 #f #f)
    (list *set-current-output-port-function* "set_outp" 1 #f #f)
    (list *num-s->c* "INUM" 1 #f #f)
    (list *num-c->s* "MAKINUM" 1 #f #f)
    (list *bool-s->c* "NFALSEP" 1 #f #f)
    (list *bool-c->s* "SBOOL" 1 #f #f)
    (list *char-c->s* "MAKICHR" 1 #f #f))

   '((boolean? "BOOLEAN_P" 1 #f #t)
     (symbol?  "SYMBOL_P"  1 #f #t)
     (char?    "CHAR_P"    1 #f #t)
     (vector?  "VECTOR_P"  1 #f #t)
     (pair?    "PAIR_P"    1 #f #t)
     (number?  "NUMBERP"  1 #f #t) ;;; diff from the int case; scm.h macro
     (complex? "NUMBERP" 1 #f #t) ;;; not in the int case; scm.h macro
     (real?    "realp"    1 #f #t) ;;; not in the int case;
     (rational? "realp" 1 #f #t) ;;; not for int; ONLY for FLOATS
     (integer?  "intp" 1 #f #t) ;;; not for int; ONLY for FLOATS
     (string?  "STRING_P"  1 #f #t)
     (procedure? "procedurep" 1 #f #t)

     (not      "NOT"     1 #f #f)
     (eq?      "=="       2 #t #t)
     (eqv?     "eqv"       2 #f #t);; diff for int
     (char=?   "=="       2 #t #t)
     (null?    "NULL_P"    1 #f #t)
     (zero?    "zerop"    1 #f #t);; diff for int
     (positive? "positivep" 1 #f #t);; diff for int
     (negative? "negativep" 1 #f #t);; diff for int

     (logand "&" 2 #t #t)
     (logior "|" 2 #t #t)
     (logxor "^" 2 #t #t)
     (lognot "~" 1 #f #t)
     (logsleft  "<<" 2 #t #t)
     (logsright ">>" 2 #t #t)

     (=	"eqp"       2 #f #t);; diff for int
     (<	"lessp"     2 #f #t);; diff for int
     (>	"greaterp"  2 #f #t);; diff for int
     (<=       "leqp"      2 #f #t);; diff for int
     (>=       "greqp"     2 #f #t);; diff for int

     (+	"sum"	2 #f #t);; diff for int
     (-	"difference" 2 #f #t);; diff for int
     (*	"product"    2 #f #t);; diff for int

     (/	"divide"    2 #f #f);; diff for int
     (quotient "lquotient" 2 #f #f)
     (remainder "lremainder" 2 #f #f)

     (set-car! "SET_CAR"   2 #f #t)
     (set-cdr! "SET_CDR"   2 #f #t)
     (vector-set! "VECTOR_SET" 3 #f #t)
     (vector-ref  "VECTOR_REF" 2 #f #t)

     (equal? "equal" 2))

   *standard-s->c-fun-table*))


(define (primitive? fn)
  (or (member fn *cxr-funs*)
      (if *floats-flag*
	  (assq fn *floats-s->c-fun-table*)
	  (assq fn *reckless-s->c-fun-table*))
      (assq fn *switch-args-table*)
      (assq fn *add-args-table*)
      (member fn '(list append cond case do let let* letrec define
			if and or map for-each))))

(define (fixed-arity-primitive? fn)
  (or (member fn *cxr-funs*)
      (and (if *floats-flag*
	       (assq fn *floats-s->c-fun-table*)
	       (assq fn *reckless-s->c-fun-table*))
	   (not (assq fn *associative-fun-table*))
	   (not (assq fn *comparison-fun-table*))
	   (not (assq fn *add-args-table*))
	   (not (member fn '(list append cond case do let let* letrec
				  define if and or map for-each
				  < > <= = >= + * - /
				  %< %> %<= %= %>= %+ %* %- %/ ))))
      (assq fn *switch-args-table*)))

(define (primitive-arity fn)
  (let ((tmp (if *floats-flag*
		 (assq fn *floats-s->c-fun-table*)
		 (assq fn *reckless-s->c-fun-table*))))
    (cond (tmp (caddr tmp))
	  ((memq fn *cxr-funs*) 1)
	  (else #f))))

;===================================================================
;
;		   introducing type conversion,
;		     collecting constants,
;		  moving variables to top-let.
;
;===================================================================


(define (type-const-wholedef term)
  (set! *local-vars* '())
  (set! *local-parameters*
	(map (lambda (x) (if (pair? x) (cadr x) x))
	     (cadr (caddr term))))
  (set! *current-fun-name* (cadr term))
  (let* ((tmp (map type-const-pass (cddr (caddr term))))
	 (tmp2 (list 'lambda
		     (cadr (caddr term))
		     (cons 'let*
			   (cons (map (lambda (x) (list x *dummy*))
				      *local-vars*)
				 (begins->list tmp))))))
    (list (car term) (cadr term) tmp2)))



(define (begins->list lst)
  (let ((res '()))
    (do ((part lst (cdr part)))
	((null? part))
      (if (and (pair? (car part))
	       (or (eq? 'begin (caar part))
		   (eq? *op-begin* (caar part))))
	  (set! res (append (reverse (begins->list (cdar part))) res))
	  (set! res (cons (car part) res))))
    (reverse res)))


(define (type-const-pass-res term)
  (cond
   ((string? term)
    (make-string-constant term))
   ((char? term)
    (list *char-c->s* term))
   ((vector? term)
    (make-vector-constant term))
   ((number? term)
    (if (and (integer? term)
	     (exact? term)
	     (<= term most-positive-fixnum)
	     (>= term most-negative-fixnum))
	(list *num-c->s* term)
	(begin
	  (if (not *floats-flag*)
	      (report-warning
	       "exact arithmetic assumed but a nonexact number encountered: " term))
	  (make-number-constant term))))
   ((symbol? term)
    (cond ((or (memq term *local-parameters*)
	       (memq term *local-vars*)
	       (memq term *special-c-vars*)
	       (memq term *special-scm->c-functions*))
	   term)
	  ((memq term *fast-vars-list*)
	   term)
	  ((memq term *interpreter-defined-vars*)
	   (list *global-access* term))
	  ((memq term *global-vars-list*)
	   (list *global-access* term))
	  ((or (member-if (lambda (x) (eq? term (cadr x)))
			  *new-constant-list*)
	       (member-if (lambda (x) (eq? term (cadr x)))
			  *symbol-constant-table*)
	       (in-file-defined? term))
	   term)
	  (else (or (memq term *unknown-vars*)
		    (set! *unknown-vars* (cons term *unknown-vars*)))
		(list *global-access* (make-unknown-constant term)))))
   ((boolean? term)
    (list *bool-c->s* term))
   ((null? term)
    '())
   ((not (pair? term))
    (report-error "disallowed object: " term))
   ((eq? *special-pseudoquote* (car term))
    term)
   ((eq? *actual-c-string* (car term))
    term)
   ((eq? *actual-c-int* (car term))
    term)
   ((eq? *actual-c-eval* (car term))
    term)
   ((eq? 'quote (car term))
    (cond ((or (string? (cadr term))
	       (vector? (cadr term))
	       (number? (cadr term))
	       (boolean? (cadr term))
	       (char? (cadr term))
	       (null? (cadr term)))
	   (type-const-pass (cadr term)))
	  ((symbol? (cadr term))
	   (make-symbol-constant (cadr term)))
	  ((pair? (cadr term))
	   (make-pair-constant (cadr term)))
	  (else
	   (report-error "disallowed object: " term))))
   ((and
     *reckless-arithmetic-flag*
     (not (modified-fun? (car term)))
     (or (memq (car term) *always-num-arg-c-funs*)
	 (and (not *floats-flag*)
	      (memq (car term) *num-arg-c-funs*))))
    (let* ((tmp (map type-const-pass (cdr term)))
	   (tmp2
	    (cons (car term)
		  (map (lambda (x)
			 (if (and (pair? x)
				  (eq? (car x) *num-c->s*))
			     (cadr x)
			     (list *num-s->c* x)))
		       tmp))))
      (cond ((memq (car term) *num-res-c-funs*)
	     (list *num-c->s* tmp2))
	    ((memq (car term) '(= < <= > >= %= %< %<= %> %>=))
	     (cond
	      ((and (pair? (cadr tmp2))
		    (pair? (caddr tmp2))
		    (eq? (car (cadr tmp2)) (car (caddr tmp2)))
		    (eq? *num-s->c* (car (cadr tmp2))))
	       (list *bool-c->s*
		     (cons (car term) (map cadr (cdr tmp2)))))
	      ((or (and (not (pair? (cadr tmp2)))
			(pair? (caddr tmp2)))
		   (and (not (pair? (caddr tmp2)))
			(pair? (cadr tmp2))))
	       (list *bool-c->s* (cons (car term) tmp)))
	      (else
	       (list *bool-c->s* tmp2))))
	    ((and (not *floats-flag*)
		  (memq (car term) *bool-res-c-funs*))
	     (list *bool-c->s* tmp2))
	    ((memq (car term) *always-bool-res-c-funs*)
	     (list *bool-c->s* tmp2))
	    (else
	     tmp2))))
   ((and (or (memq (car term) '(eq? char=? %eqv? %=))
	     (and (not *floats-flag*)
		  (or (eq? 'eqv? (car term))
		      (eq? '= (car term)))))
	 (not (modified-fun? (car term))))
    (let ((tmp (map type-const-pass (cdr term))))
      (if (and (pair? (car tmp))
	       (memq (caar tmp) *type-converters*)
	       (pair? (cadr tmp))
	       (memq (caadr tmp) *type-converters*))
	  (list *bool-c->s* (cons (car term) (map cadr tmp)))
	  (list *bool-c->s* (cons (car term) tmp)))))
   ((and (memq (car term) *bool-arg-c-funs*)
	 (not (modified-fun? (car term))))
    (let* ((tmp (map type-const-pass (cdr term)))
	   (tmp2 (cons (car term) (map c-boolify tmp))))
      (if (memq (car term) *bool-res-c-funs*)
	  (list *bool-c->s* tmp2)
	  tmp2)))
   ((and (not *floats-flag*)
	 (memq (car term) *bool-res-c-funs*)
	 (not (modified-fun? (car term))))
    (list *bool-c->s*
	  (cons (car term) (map type-const-pass (cdr term)))))
   ((and (memq (car term) *always-bool-res-c-funs*)
	 (not (modified-fun? (car term))))
    (list *bool-c->s*
	  (cons (car term) (map type-const-pass (cdr term)))))
   ((or (eq? 'if (car term)) (eq? *op-if* (car term)))
    (let ((tmp (map type-const-pass (cdr term))))
      (cons (car term)
	    (cons (c-boolify (car tmp)) (cdr tmp)))))
   ((eq? (car term) 'let*)
    (set! *local-vars* (union (map car (cadr term)) *local-vars*))
    (cons 'begin
	  (map type-const-pass
	       (begins->list
		(append (map (lambda (x) (cons 'set! x)) (cadr term))
			(cddr term))))))
   ((eq? (car term) *op-let*)
    (set! *local-vars* (union (map car (cadr term)) *local-vars*))
    (cons *op-begin*
	  (map type-const-pass
	       (begins->list
		(append (map (lambda (x) (cons 'set! x)) (cadr term))
			(cddr term))))))
   ((or (eq? 'begin (car term)) (eq? *op-begin* (car term)))
    (cons (car term)
	  (begins->list (map type-const-pass (cdr term)))))
   ((eq? (car term) 'do)
    (set! *local-vars* (union (map car (cadr term)) *local-vars*))
    (let ((tmp (list* 'do
		      (map (lambda (x) (map type-const-pass x))
			   (cadr term))
		      (map type-const-pass (caddr term))
		      (map type-const-pass (cdddr term)))))
      (if (null? (cdr (caddr tmp)))
	  (cons *do-not*
		(begins->list
		 (cons (cadr tmp)
		       (cons (cons (c-negate
				    (c-boolify
				     (car (caddr tmp))))
				   (cdr (caddr tmp)))
			     (cdddr tmp)))))
	  (cons
	   'begin
	   (begins->list
	    (cons
	     (cons *do-not*
		   (begins->list
		    (cons (cadr tmp)
			  (cons (list
				 (c-negate
				  (c-boolify
				   (car (caddr tmp)))))
				(cdddr tmp)))))
	     (begins->list (cdr (caddr tmp)))))))))
   ((eq? *function* (car term))
    (cond ((or (memq (cadr term) *local-vars*)
	       (memq (cadr term) *local-parameters*))
	   (list *function* (cadr term)))
	  ((memq (cadr term) *top-level-funs*)
					;  (report-error
					;    "In " *current-fun-name* " compiled function "
					;    (cadr term) " occurs as an argument. Use lambdaterm!")
	   (list *function* (cadr term)))
	  ((in-file-defined? (cadr term))
	   (list *function* (cadr term)))
	  (else
	   (report-error
	    "In " *current-fun-name* " interpreted function "
	    (cadr term) " occurs as an argument. Use lambdaterm!"))))
   ((and (memq (car term)  *cxr-funs*)
	 (not (modified-fun? (car term))))
    (cxr-open (car term) (type-const-pass (cadr term))))
					; the following always fails
   ((begin (set! term (fun-names-to-refs term)) #f))
   ((unknown-function? (car term) (cdr term))
    (make-unknown-call term))
   ((and (eq? (car term) 'vector-set!)
	 (memq (cadr term) *stable-vector-names*))
    (cons *st-vector-set*
	  (cons (string->symbol
		 (string-append
		  (symbol->string (cadr term))
		  *st-vector-postfix*))
		(map type-const-pass (cddr term)))))
   ((and (eq? (car term) 'vector-ref)
	 (memq (cadr term) *stable-vector-names*))
    (cons *st-vector-ref*
	  (cons (string->symbol
		 (string-append
		  (symbol->string (cadr term))
		  *st-vector-postfix*))
		(map type-const-pass (cddr term)))))
   (else
    (cons (car term)
	  (map type-const-pass (cdr term))))))

(define (type-const-pass term)
  (define res (type-const-pass-res term))
  (if (and (pair? res) (or (eq? 'begin (car res))
			   (eq? *op-begin* (car res))))
      (cons (car res) (begins->list (cdr res)))
      res))


(define (fun-names-to-refs term)
  (let ((hofdata (assq (car term) *liftable-hof-database*))
	(tmp '()))
    (if hofdata
	(cons (car term)
	      (map (lambda (flag arg)
		     (cond
		      ((not (symbol? arg)) arg)
		      (flag arg)
		      (else (fun-names-to-refs-aux arg))))
		   (cdr hofdata)
		   (cdr term)))
	(cons (car term)
	      (map (lambda (arg)
		     (if (symbol? arg)
			 (fun-names-to-refs-aux arg)
			 arg))
		   (cdr term))))))

(define (fun-names-to-refs-aux name)
  (if (and (not (memq name *local-parameters*))
	   (not	(memq name *local-vars*))
	   (in-file-defined? name))
      (let ((newname (make-closure-scmobj-name name)))
	(if (not (memq name *symbol-and-fun-list*))
	    (begin
	      (set! *var-make-list*
		    (cons `(set!
			    ,(make-closure-scmobj-name name)
			    (,*c-adr* (cdr (,*intern-function*
					    (,*actual-c-string*
					     ,(symbol->string name))
					    ,(string-length
					      (symbol->string name))))))
			  *var-make-list*))
	      (set! *symbol-and-fun-list* (cons name *symbol-and-fun-list*))))
	(list *global-access* newname))
      name))


(define (unknown-function? fn args)
  (let ((len (length args)))
    (or
     (pair? fn)
     (modified-fun? fn)
     (not
      (or (let ((tmp (memq fn *prohibited-funs*)))
	    (if tmp
		(report-error "In " *current-fun-name*
			      " a prohibited function "
			      fn " is called."))
	    #f)
	  (eq? fn *current-fun-name*)
	  (memq fn *special-scm->c-functions*)
	  (assq fn *switch-args-table*)
	  (assq fn *add-args-table*)
	  (memq fn '(vector string if begin let* lambda set!))
	  (memq fn *internal-c-functions*)
	  (let ((tmp (if *floats-flag*
			 (assq fn *floats-s->c-fun-table*)
			 (assq fn *reckless-s->c-fun-table*))))
	    (if (and tmp (not (eqv? len (caddr tmp))))
		(report-error "In " *current-fun-name* " function "
			      fn " is called with a wrong nr of args."))
	    tmp)
	  (let ((tmp (member-if (lambda (x) (eq? fn (cadr x)))
				*to-do-fun-list*)))
	    (if (and tmp (not (eqv? len (length (cadr (caddar tmp))))))
		(if (memq fn *top-level-funs*)
		    (report-error "In " *current-fun-name* " function "
				  fn " is called with a wrong nr of args.")
		    (report-error "In " *current-fun-name* " function "
				  fn
				  " is called with a wrong nr of args or builds closures.")))
	    tmp)
	  (let ((tmp (member-if (lambda (x) (eq? fn (cadr x))) *passed-defs*)))
	    (if (and tmp (not (eqv? len (length (cadr (caddar tmp))))))
		(if (memq fn *top-level-funs*)
		    (report-error "In " *current-fun-name* " function "
				  fn " is called with a wrong nr of args.")
		    (report-error "In " *current-fun-name* " function "
				  fn
				  " is called with a wrong nr of args or builds closures.")))
	    tmp)
	  (memq fn *top-level-funs*))))))


(define (in-file-defined? fn)
  (or (memq fn *top-level-funs*)
      (eq? fn *current-fun-name*)
      (member-if (lambda (x) (eq? fn (cadr x))) *to-do-fun-list*)
      (member-if (lambda (x) (eq? fn (cadr x))) *passed-defs*)))

(define (top-nonlist-in-file-defined? fn)
  (let ((x (or (member-if
		(lambda (x) (or (eq? fn (cadr x))
				(and (pair? (cadr x)) (eq? fn (caadr x)))))
		*to-do-fun-list*)
	       (member-if
		(lambda (x) (or (eq? fn (cadr x))
				(and (pair? (cadr x)) (eq? fn (caadr x)))))
		*passed-defs*))))
    (and x
	 (let ((y (car x)))
	   (if (pair? (cadr y))
	       (list? (cadr y))
	       (and (pair? (cddr y))
		    (pair? (caddr y))
		    (eq? 'lambda (car (caddr y)))
		    (pair? (cdr (caddr y)))
		    (list? (cadr (caddr y)))))))))

(define (make-unknown-call term)
  (let* ((fn (car term))
	 (args1 (map type-const-pass (cdr term)))
	 (args (map make-interpreter-usable args1))
	 (glob '()))
    (if (pair? fn)
	(set! glob (type-const-pass (car term)))
	(if (or (memq fn *special-c-vars*)
		(memq fn *local-parameters*)
		(memq fn *local-vars*))
	    (set! glob fn)
	    (set! glob (list *global-access* (make-unknown-constant fn)))))
    (or (pair? fn)
	(memq fn *special-c-vars*)
	(memq fn *local-parameters*)
	(memq fn *local-vars*)
	(memq fn *unknown-functions*)
	(set! *unknown-functions* (cons fn *unknown-functions*)))
    (list *sysapply*
	  glob
	  (if (null? args) '() (car args))
	  (if (null? args)
	      '()
	      (make-apply-second-arg (cdr args))))))

(define (make-unknown-call-aux term args)
  (let ((fn (caar term)))
    (if (or (in-file-defined? fn)
	    (memq fn *prohibited-funs*))
	(report-error "In " *current-fun-name* " function "
		      fn " is assumed to return a closure.")
	(make-unknown-call (car term)))))


(define (make-interpreter-usable term)
  (let ((fn (if (pair? term)
		(if (and (eq? *global-access* (car term))
			 (in-file-defined? (cadr term)))
		    (cadr term)
		    #f)
		(if (and (symbol? term)
			 (in-file-defined? term))
		    term
		    #f)))
	(tmp '()))
    (if (not fn)
	term
	(make-interpreter-funname fn))))


(define (make-interpreter-funname fn)
  (let ((tmp (assq fn *interpreter-funname-table*)))
    (if tmp
	(cdr tmp)
	(begin
	  (set! tmp
		(string->symbol
		 (string-append (symbol->string fn) *interpreter-suffix*)))
	  (set! *interpreter-funname-table*
		(cons (cons fn tmp) *interpreter-funname-table*))
	  (list *global-access* tmp)))))

(define (make-unknown-constant var)
  (if (memq var *global-vars-list*)
      var
      (begin
	(set! *global-vars-list* (cons var *global-vars-list*))
	(set! *var-make-list*
	      (cons `(set!
		      ,var
		      (,*c-adr* (cdr (,*intern-function*
				      (,*actual-c-string*
				       ,(symbol->string var))
				      ,(string-length
					(symbol->string var))))))
		    *var-make-list*))
	var)))

(define (make-apply-second-arg args)
  (if (null? args)
      *listofnull*
      (list 'cons
	    (car args)
	    (make-apply-second-arg (cdr args)))))



(define (make-string-constant str)
  (let ((name (make-constant-name)))
    (set! *new-constant-list*
	  (cons (list 'set!
		      name
		      (list 'scm-gc-protect
			    (list *makfromstr-function*
				  (list *actual-c-string* str)
				  (string-length str))))
		*new-constant-list*))
    name))

(define (make-number-constant num)
  (let ((name (make-constant-name))
	(str (number->string num)))
    (set! *new-constant-list*
	  (cons (list 'set!
		      name
		      (list 'scm-gc-protect
			    (list *string->number-function*
				  (list *makfromstr-function*
					(list *actual-c-string* str)
					(string-length str))
				  (list *num-c->s* 10))))
		*new-constant-list*))
    name))


(define (make-vector-constant vect)
  (let* ((name (make-constant-name))
	 (tmp (list 'set!
		    name
		    (list 'scm-gc-protect
			  (list 'list->vector
				(make-pair-constant-aux
				 (vector->list vect)))))))
    (set! *new-constant-list* (cons tmp *new-constant-list*))
    name))


(define (make-pair-constant pair)
  (let* ((name (make-constant-name))
	 (tmp (list 'set!
		    name
		    (list 'scm-gc-protect
			  (list 'cons
				(make-pair-constant-aux (car pair))
				(make-pair-constant-aux (cdr pair)))))))
    (set! *new-constant-list* (cons tmp *new-constant-list*))
    name))

(define (make-pair-constant-aux term)
  (if (pair? term)
      (list 'cons
	    (make-pair-constant-aux (car term))
	    (make-pair-constant-aux (cdr term)))
      (type-const-pass (list 'quote term))))

(define (make-symbol-constant symb)
  (let ((tmp (assq symb *symbol-constant-table*)))
    (if tmp
	(cadr tmp)
	(let ((name (make-symbol-name symb))
	      (str (symbol->string symb))
	      (clname '()))
	  ;; if the symb is also a top-level-fun, then avoid
	  ;; applying make_subr to the symbol name string:
	  ;; this would mess up symbol-names table for scm.
	  (if (and (memq symb *top-level-funs*)
		   (not (memq symb *symbol-and-fun-list*)))
	      (begin
		(set! *var-make-list*
		      (cons `(set!
			      ,(make-closure-scmobj-name symb)
			      (,*c-adr* (cdr (,*intern-function*
					      (,*actual-c-string*
					       ,(symbol->string symb))
					      ,(string-length
						(symbol->string symb))))))
			    *var-make-list*))

		(set! *symbol-and-fun-list*
		      (cons symb *symbol-and-fun-list*))))
	  (set! *symbol-constant-table*
		(cons (list symb name) *symbol-constant-table*))
	  (set! *symbol-list*
		(cons (list 'set!
			    name
			    `(scm-gc-protect
			      (car (,*intern-symbol-function*
				    (,*actual-c-string* ,str)
				    ,(string-length str)))))
			;;;    (list 'string->symbol
			;;;	  (list 'list->string
			;;;		(make-pair-constant-aux
			;;;		   (string->list
			;;;		      (symbol->string symb)))))
		      *symbol-list*))
	  name))))

(define (make-constant-name)
  (set! *new-constant-num* (+ 1 *new-constant-num*))
  (string->symbol (string-append *new-constant-prefix*
				 (number->string *new-constant-num*))))


(define (make-symbol-name symb)
  (string->symbol (string-append (symbol->string symb)
				 *symbol-name-postfix*)))

(define (c-negate term)
  (if (and (pair? term) (eq? *not?* (car term)))
      (cadr term)
      (list *not?* term)))

(define (cxr-open cxr arg)
  (let* ((str (symbol->string cxr))
	 (chr #\c)
	 (len (string-length str))
	 (res arg))
    (do ((n (- len 2) (- n 1)))
	((= 0 n))
      (set! chr (string-ref str n))
      (set! res
	    (list (if (eqv? #\a chr) 'car 'cdr) res)))
    res))


(define (c-boolify term)
  (if (and (pair? term) (eq? *bool-c->s* (car term)))
      (cadr term)
      (list *bool-s->c* term)))

;===================================================================
;
;			  a pass for
;	    correcting higher-order function calls and
;		 dotted-arglist function calls.
;
;===================================================================


(define (ho-dot-wholedef term)
  (set! *current-fun-name* (cadr term))
  (ho-dot-pass term))

(define (ho-dot-pass term)
  (cond ((or (not (pair? term)) (eq? 'quote (car term)))
	 term)
	((memq (car term) *dot-arg-funs*)
	 (let* ((template (assq (car term) *dot-arg-templates*))
		(new  (make-listarg-arglist (cadr template) (cdr term))))
	   (if (and (memq (car term) *higher-order-funs*)
		    (liftable-hofname? (car term)))
	       (correct-ho-call
		(cons (car term) (map ho-dot-pass new)))
	       (cons (car term) (map ho-dot-pass new)))))
	((and (memq (car term) *higher-order-funs*)
	      (liftable-hofname? (car term)))
	 (correct-ho-call (map ho-dot-pass term)))
	(else
	 (map ho-dot-pass term))))

(define (correct-ho-call term)
  (let* ((add-args '())
	 (stay-args '())
	 (name (car term))
	 (data (assq name *higher-order-templates*))
	 (new-template '()))

    (do ((args (cdr term) (cdr args))
	 (funtemplate (cadr data) (cdr funtemplate)))
	((null? args))
      (if (car funtemplate)
	  (begin
	    (if (and (not (pair? (car args)))
		     (and (primitive? (car args))
			  (if (fixed-arity-primitive? (car args))
			      #t
			      (report-error
			       "in function " *current-fun-name*
			       " a variable-arity primitive is passed to a higher-order fun: " term))))
		(let*
		    ((tmpargs (reverse
			       (list-tail '(w v u z y x)
					  (- 6 (primitive-arity (car args))))))
		     (newfun
		      (list 'lambda
			    tmpargs
			    (cons (car args) tmpargs)))
		     (newname (make-new-primitive-instname (car args))))
		  (set! *to-do-fun-list*
			(cons (list 'define newname newfun)
			      *to-do-fun-list*))
		  (set! args (cons newname (cdr args)))))
	    (if (pair? (car args))
		(begin
		  (set! add-args
			(append (reverse (cdar args)) add-args))
		  (set! stay-args
			(cons (caar args) stay-args))
		  (set! new-template
			(cons (list
			       (length
				(filter (lambda (x)
					  (or (not (pair? x))
					      (not (eq? *c-adr* (car x)))))
					(cdar args)))
			       (length
				(filter (lambda (x)
					  (and (pair? x)
					       (eq? *c-adr* (car x))))
					(cdar args))))
			      new-template)))
		(begin
		  (set! new-template (cons (list '0 '0) new-template))
		  (set! stay-args (cons (car args) stay-args)))))
	  (begin
	    (set! new-template (cons '0 new-template))
	    (set! stay-args (cons (car args) stay-args)))))

    (set! new-template (reverse new-template))
    (set! add-args (reverse add-args))
    (set! stay-args (reverse stay-args))
    (let ((attempt (assoc new-template (cddr data))))
      (if attempt
	  (begin
	    (cons (cadr attempt)
		  (append add-args
			  (map (lambda (x y)
				 (if x (list *function* y) y))
			       (cadr data)
			       stay-args))))
	  (begin
	    (make-new-ho-instance term new-template data add-args stay-args)
	    (let ((attempt2 (assoc new-template (cddr data))))
	      (cons (cadr attempt2)
		    (append add-args
			    (map (lambda (x y)
				   (if x (list *function* y) y))
				 (cadr data)
				 stay-args)))))))))


(define (make-new-ho-instance term new-template data add-args stay-args)
  (let* ((done-mainfun-flag #f)
	 (mainfun-place (member-if (lambda (x) (eq? (cadr x) (car term)))
				   *to-do-fun-list*))
	 (ho-fun  (if mainfun-place
		      (begin
			(set! done-mainfun-flag #f)
			(car mainfun-place))
		      (begin
			(set! mainfun-place
			      (member-if (lambda (x) (eq? (cadr x) (car term)))
					 *passed-defs*))
			(if (not mainfun-place)
			    (report-error "Higher-order function "
					  (car term)
					  " is not defined."))
			(set! done-mainfun-flag #t)
			(car mainfun-place))))
	 (dot-data (assq (cadr ho-fun) *dot-arg-templates*))
	 (data (assq (car term) *higher-order-templates*))
	 (ho-term (caddr ho-fun))
	 (new-args '())
	 (new-name (make-new-instname (cadr ho-fun) (length (cddr data)))))
    (set! *top-level-funs*
	  (cons new-name *top-level-funs*))
    (set! *make-new-ho-data* '())
    (for-each (lambda (x y)
		(if (pair? x)
		    (let ((new (make-new-parameters x)))
		      (set! *make-new-ho-data*
			    (cons (cons (if (pair? y) (cadr y) y)
					(args->list new))
				  *make-new-ho-data*))
		      (set! new-args
			    (append new new-args)))))
	      new-template
	      (args->list (cadr ho-term)))
    (if dot-data
	(begin (set! *dot-arg-funs* (cons new-name *dot-arg-funs*))
	       (set! *dot-arg-templates*
		     (cons (list new-name
				 (append new-args (cadr dot-data)))
			   *dot-arg-templates*))))
    (set! ho-term (make-new-inst-aux
		   ho-term (args->list new-args) (cadr ho-fun) new-name))
    (set! ho-term
	  (cons (car ho-term)
		(cons (append new-args (cadr ho-term))
		      (cddr ho-term))))
    (set! ho-fun
	  (list (car ho-fun) new-name ho-term))
    (set-cdr! (my-last-pair data)
	      (list (list new-template (cadr ho-fun))))
    (if done-mainfun-flag
	(begin
	  (set-cdr! mainfun-place
		    (cons (car mainfun-place) (cdr mainfun-place)))
	  (set-car! mainfun-place ho-fun))
	(begin
	  (set-cdr! mainfun-place
		    (cons ho-fun (cdr mainfun-place)))))))

(define (make-new-instname genname nr)
  (let ((name
	 (string->symbol
	  (string-append (symbol->string genname)
			 *new-instfun-infix*
			 (number->string nr)))))
    name))

(define (make-new-primitive-instname genname)
  (set! *new-primitive-instnr* (+ 1 *new-primitive-instnr*))
  (let ((name
	 (string->symbol
	  (string-append (symbol->string genname)
			 *new-instfun-infix*
			 (number->string *new-primitive-instnr*)))))
    name))

(define (make-new-inst-aux term n-args o-name n-name)
  (let ((tmp #f))
    (cond ((or (not (pair? term)) (eq? 'quote (car term))) term)
	  ((eq? *higher-order-call* (car term))
	   (set! tmp (assq (cadr term) *make-new-ho-data*))
	   (if tmp
	       (cons (car term)
		     (cons (cadr term)
			   (if (null? (cdr tmp))
			       (cddr term)
			       (append (cdr tmp) (cddr term)))))
	       (map (lambda (x) (make-new-inst-aux x n-args o-name n-name))
		    term)))
	  ((eq? (car term) 'lambda)
	   (cons (car term)
		 (cons (cadr term)
		       (map (lambda (x)
			      (make-new-inst-aux x n-args o-name n-name))
			    (cddr term)))))
	  ((eq? (car term) o-name)
	   (cons n-name
		 (append
		  n-args
		  (map (lambda (x)
			 (make-new-inst-aux x n-args o-name n-name))
		       (cdr term)))))
	  (else
	   (map (lambda (x) (make-new-inst-aux x n-args o-name n-name))
		term)))))


(define (make-new-parameters nums)
  (let* ((vars1 '())
	 (vars2 '()))
    (do ((n (car nums) (- n 1)))
	((zero? n))
      (set! vars1 (cons (make-new-parameter) vars1)))
    (do ((n (cadr nums) (- n 1)))
	((zero? n))
      (set! vars2 (cons (list *c-adr* (make-new-parameter)) vars2)))
    (set! vars1 (reverse vars1))
    (set! vars2 (reverse vars2))
    (append vars1 vars2)))

(define (make-new-parameter)
  (set! *new-parameter-nr* (+ 1 *new-parameter-nr*))
  (string->symbol (string-append *new-parameter-prefix*
				 (number->string *new-parameter-nr*))))



;===================================================================
;
;	      statement-lifting  & tail-recursion
;
;===================================================================


(define (lift-statements-wholedef defterm)
  (set! *current-fun-name* (cadr defterm))
  (set! *tailrec-flag* #f)
  (set! *higher-order-flag* #f)
  (let ((res '())
	(res2 '())
	(newname #f)
	(tmp '())
	(lambdaterm (caddr defterm)))
    (set! *higher-order-args* (args->list (cadr lambdaterm)))
    (set! *current-formal-args* (cadr lambdaterm))
    (set! *current-formal-argslist* (args->list (cadr lambdaterm)))
    (set! res (lift-statements lambdaterm '()))
    (if (not (list? (cadr lambdaterm)))
	(begin
	  (set! *dot-arg-funs*
		(cons (cadr defterm) *dot-arg-funs*))
	  (set! *dot-arg-templates*
		(cons (list (cadr defterm)
			    (cadr lambdaterm))
		      *dot-arg-templates*))))
    (if (and *higher-order-flag*
	     (liftable-hofname? (cadr defterm)))
	(begin
	  (set! *higher-order-args*
		(map (lambda (x) (if (eq? x '#t) '#t '#f))
		     *higher-order-args*))
	  (set! *higher-order-funs*
		(cons (cadr defterm) *higher-order-funs*))
	  (set! *higher-order-templates*
		(cons
		 (list (cadr defterm)
		       *higher-order-args*
		       (list (map (lambda (x)
				    (if x (list '0 '0) '0))
				  *higher-order-args*)
			     (cadr defterm)))
		 *higher-order-templates*))
	  (if (and (memq *current-fun-name* *top-level-funs*)
		   (not (null? *export-functions*))
		   (or (not (pair? *export-functions*))
		       (memq *current-fun-name* *export-functions*)))
	      (begin
		(set! newname
		      (string->symbol
		       (string-append (symbol->string *current-fun-name*)
				      *export-hof-postfix*)))
		(set! *top-level-funs*
		      (cons newname *top-level-funs*))
		(set! *export-table*
		      (cons (list *current-fun-name* newname)
			    *export-table*))
		(set! tmp (assq *current-fun-name* *dot-arg-templates*))
		(if tmp
		    (begin
		      (set! *dot-arg-templates*
			    (cons (list newname (cadr tmp))
				  *dot-arg-templates*))
		      (set! *dot-arg-funs*
			    (cons newname *dot-arg-funs*))))
		(set! res2
		      (make-export-hof res))))
	  (set! res (cons (car res)
			  (cons (map (lambda (x y)
				       (if x (list *function* y) y))
				     *higher-order-args*
				     (maklist (cadr res)))
				(cddr res))))))
    (if *tailrec-flag*
	(begin
	  (set! res (cons (car res)
			  (cons (cadr res)
				(cons (list *mark-tailrec*)
				      (cddr res)))))
	  (if (not (null? res2))
	      (set! res2 (cons (car res2)
			       (cons (cadr res2)
				     (cons (list *mark-tailrec*)
					   (cddr res2))))))))
    (set! res
	  (list 'define (cadr defterm)
		(if (list? (cadr res))
		    res
		    (cons (car res)
			  (cons (maklist (cadr res))
				(cddr res))))))
    (if (null? res2)
	(list res)
	(list res
	      (list 'define newname
		    (if (list? (cadr res2))
			res2
			(cons (car res2)
			      (cons (maklist (cadr res2))
				    (cddr res2)))))))))


(define (maklist args)
  (cond ((symbol? args)
	 (list args))
	((null? args)
	 '())
	(else (cons (car args) (maklist (cdr args))))))


(define (make-export-hof term)
  (cond ((or (not (pair? term))
	     (eq? 'quote (car term)))
	 term)
	((eq? 'lambda (car term))
	 (cons (car term)
	       (cons (cadr term)
		     (map make-export-hof (cddr term)))))
	((eq? (car term) *higher-order-call*)
	 (list *sysapply*
	       (cadr term)
	       (if (null? (cddr term))
		   '()
		   (make-export-hof (caddr term)))
	       (if (null? (cddr term))
		   '()
		   (make-apply-second-arg
		    (make-export-hof (cdddr term))))))
	((eq? (car term) *function*)
	 (cadr term))
	((eq? (car term) *current-fun-name*)
	 (cons (string->symbol
		(string-append (symbol->string *current-fun-name*)
			       *export-hof-postfix*))
	       (map make-export-hof (cdr term))))
	(else
	 (map make-export-hof term))))


(define (lift-statements term checkvars)
  (cond
   ((or (not (pair? term)) (eq? 'quote (car term)))
    term)
   ((eq? 'lambda (car term))
    (set! checkvars (args->list (cadr term)))
    (append
     (list 'lambda)
     (list (cadr term))
     (map (lambda (x) (lift-statements x checkvars))
	  (butlast (cddr term) 1))
     (list
      (lift-statements
       (push-result-var-in *return* (car (my-last-pair term)))
       checkvars))))
   ((and (eq? 'set! (car term))
	 (or (null? (cdr term)) (null? (cddr term))))
    (report-error
     " scheme syntax in fun " *current-fun-name* ": " term))
   ((and (eq? 'set! (car term))
	 (pair? (caddr term))
	 (memq (caaddr term) '(do if begin let*)))
    (lift-statements (push-result-var-in (cadr term) (caddr term))
		     checkvars))
   ((eq? 'do (car term))
    (set! checkvars (union (map car (cadr term)) checkvars))
    (list* 'do
	   (map
	    (lambda (x)
	      (map (lambda (y) (lift-stat-aux y checkvars)) x))
	    (cadr term))
	   (append (list
		    (lift-stat-aux (car (caddr term)) checkvars))
		   (map (lambda (x)
			  (lift-statements x checkvars))
			(cdr (caddr term))))
	   (map (lambda (x) (lift-statements x checkvars))
		(cdddr term))))
   ((eq? 'if (car term))
    (if (eq? 3 (length term))
	(list 'if
	      (lift-stat-aux (cadr term) checkvars)
	      (lift-statements (caddr term) checkvars))
	(list 'if
	      (lift-stat-aux (cadr term) checkvars)
	      (lift-statements (caddr term) checkvars)
	      (lift-statements (cadddr term) checkvars))))
   ((eq? 'begin (car term))
    (append (list 'begin)
	    (map (lambda (x)
		   (lift-statements
		    (if (and (pair? x) (eq? 'set! (car x)))
			(push-result-var-in (cadr x) (caddr x))
			x)
		    checkvars))
		 (cdr term))))
   ((or (eq? 'let* (car term)) (eq? 'let (car term)))
    (set! checkvars (union (map car (cadr term)) checkvars))
    (append (list 'let*)
	    (list (map (lambda (x) (list (car x) *dummy*)) (cadr term)))
	    (map (lambda (x)
		   (lift-statements
		    (push-result-var-in (car x) (cadr x))
		    checkvars))
		 (cadr term))
	    (map (lambda (x) (lift-statements x checkvars))
		 (cddr term))))
   ((and (eq? 'set! (car term))
	 (pair? (caddr term))
	 (memq (caaddr term) '(do if begin let*)))
    (lift-statements (push-result-var-in (cadr term) (caddr term))
		     checkvars))
   (else
    (lift-stat-aux term checkvars))))



(define (lift-stat-aux term checkvars)
  (cond
   ((or (not (pair? term)) (eq? 'quote (car term)))
    term)
   ((eq? (car term) 'if)
    (if (and *lift-ifs-flag*
	     (or (lift-if-arg? (caddr term))
		 (and (not (null? (cdddr term)))
		      (lift-if-arg? (cadddr term)))))
	(let ((argvars (free-vars term checkvars '()))
	      (newname (new-fun-name *current-fun-name*)))
	  (set! *to-do-fun-list*
		(cons
		 (list 'define
		       newname
		       (list 'lambda
			     (make-arglist argvars '())
			     (fetchify (cadr argvars) term)))
		 *to-do-fun-list*))
	  (cons newname (make-arglist argvars '())))
	(cons *op-if* (map (lambda (x) (lift-stat-aux x checkvars))
			   (cdr term)))))
   ((eq? (car term) 'begin)
    (cons *op-begin* (map (lambda (x) (lift-stat-aux x checkvars))
			  (cdr term))))
   ((or (eq? (car term) 'let*) (eq? (car term) 'let))
    (set! checkvars (union (map car (cadr term)) checkvars))
    (append (list *op-let*)
	    (list (map (lambda (x) (lift-stat-aux x checkvars))
		       (cadr term)))
	    (map (lambda (x) (lift-stat-aux x checkvars))
		 (cddr term))))
   ((eq? (car term) 'do)
    (let ((argvars (free-vars term checkvars '()))
	  (newname (new-fun-name *current-fun-name*)))
      (set! *to-do-fun-list*
	    (cons
	     (list 'define
		   newname
		   (list 'lambda
			 (make-arglist argvars '())
			 (fetchify (cadr argvars) term)))
	     *to-do-fun-list*))
      (cons newname (make-arglist argvars '()))))
   ((and (memq (car term) *current-formal-argslist*)
	 (liftable-hofname? *current-fun-name*))
    (set! *higher-order-flag* #t)
    (set! *higher-order-args*
	  (replaceq (car term) '#t *higher-order-args*))
    (cons *higher-order-call*
	  (map (lambda (x) (lift-stat-aux x checkvars)) term)))
   (else
    (map (lambda (x) (lift-stat-aux x checkvars)) term))))


;;; lift-if-arg? says whether it is needed/sensible to lift
;;; the if-statement with such a <term> as one of the resulting args

(define (lift-if-arg? term)
  (and (pair? term)
       (not (eq? 'quote (car term)))
       (not (and (memq
		  (car term)
		  (cons *not?*
			(cons *and?*
			      (cons *or?*
				    '(eq? = < > <= >=
					  number? boolean? null? pair? zero?
					  character? vector?
					  %= %< %> %<= %>=
					  %eqv? %number? %zero)))))
		 (not (member-if (lambda (x) (pair? x)) (cdr term)))))))


(define (push-result-var-in var term)
  (cond ((or (not (pair? term)) (eq? 'quote (car term)))
	 (if (eq? var *return*)
	     (list *return* term)
	     (list 'set! var term)))
	((eq? (car term) 'if)
	 (if (eq? 3 (length term))
	     (list 'if (cadr term)
		   (push-result-var-in var (caddr term)))
	     (list 'if (cadr term)
		   (push-result-var-in var (caddr term))
		   (push-result-var-in var (cadddr term)))))
	((eq? (car term) 'begin)
	 (append (list 'begin)
		 (butlast (cdr term) 1)
		 (list (push-result-var-in var
					   (car (my-last-pair term))))))
	((or (eq? (car term) 'let*) (eq? (car term) 'let))
	 (append (list 'let*)
		 (list (cadr term))
		 (butlast (cddr term) 1)
		 (list (push-result-var-in var
					   (car (my-last-pair term))))))
	((eq? (car term) 'do)
	 (append (list 'do)
		 (list (cadr term))
		 (list (append
			(list (car (caddr term)))
			(if (null? (cdr (caddr term)))
			    (list (push-result-var-in var *unspecified*))
			    (append
			     (butlast (cdr (caddr term)) 1)
			     (list
			      (push-result-var-in
			       var
			       (car (my-last-pair (caddr term)))))))))
		 (cdddr term)))
	;; ((eq? (car term) 'lambda)
	;;    (report-error
	;;	"Compiled function " *current-fun-name* " builds closures."))
	((eq? var *return*)
	 (if (eq? (car term) *current-fun-name*)
	     (begin
	       (set! *tailrec-flag* #t)
	       (make-tailrec-call (cdr term)))
	     (list *return* term)))
	(else
	 (list 'set! var term))))


(define (make-tailrec-call args)
  (define (first-n-reverse n lst)
    (if (zero? n) '() (cons (car lst) (first-n-reverse (- n 1) (cdr lst)))))
  (let ((tmp1 '())
	(tmp2 '())
	(tmp3 '()))
    (set! tmp3 (args->list *current-formal-args*))
    (set! args (make-listarg-arglist *current-formal-args* args))
    (do ((args-lst args (cdr args-lst))
	 (form-lst tmp3 (cdr form-lst)))
	((null? args-lst))
      (if (not (equal? (car args-lst) (car form-lst)))
	  (begin (set! tmp1 (cons (car args-lst) tmp1))
		 (set! tmp2 (cons (car form-lst) tmp2)))))
    (set! tmp1 (reverse tmp1))
    (set! tmp2 (reverse tmp2))
    (cond
     ((null? tmp1) (list *goto-tailrec*))
     ((null? (cdr tmp1))
      (list 'begin
	    (list 'set! (car tmp2) (car tmp1))
	    (list *goto-tailrec*)))
     (else
      (let ((tmplist
	     (first-n-reverse (length tmp1) *tmp-vars*)))
	(append
	 (list 'let*)
	 (list (map (lambda (x y) (list x y)) tmplist tmp1))
	 (map (lambda (x y) (list 'set! x y)) tmp2 tmplist)
	 (list (list *goto-tailrec*))))))))


(define (make-listarg-arglist formals args)
  (cond ((list? formals) args)
	((symbol? formals) (list (normalize-list-aux args)))
	((null? args)
	 (report-error
	  "In "  *current-fun-name*
	  " a list-taking function is called with too few args."))
	(else
	 (cons (car args)
	       (make-listarg-arglist (cdr formals) (cdr args))))))


(define (build-wrappers funs)
  (define (build-wrapper-aux arity arg)
    (cond ((null? arity)
	   '())
	  ((not (pair? arity))
	   (list arg))
	  (else
	   (cons (list 'car arg)
		 (build-wrapper-aux (cdr arity) (list 'cdr arg))))))
  (define (build-wrapper fun)
    (let* ((name (cadr fun))
	   (export (assq name *export-table*))
	   (arity (cadr (caddr fun)))
	   (arity2 (assq name *dot-arg-templates*)))
      (if arity2
	  (set! arity (cadr arity2)))
      (if (or (not (memq name *export-functions*))
	      (symbol? arity)
	      (and (list? arity)
		   (< (length arity) 4)))
	  #f
	  `(define ,(string->symbol
		     (string-append (symbol->string name)
				    *wrapper-postfix*))
	     (lambda (x)
	       (,*return*
		(,(if export (cadr export) name)
		 ,@(build-wrapper-aux arity 'x))))))))
  (let ((res '()))
    (for-each (lambda (x)
		(let ((new (build-wrapper x)))
		  (if new
		      (begin
			(set! res (cons new res))
			(set! *wrapper-table*
			      (cons (list (cadr x) (cadr new))
				    *wrapper-table*))))))
	      funs)
    res))

(define (build-wrapped-interpreter-table)
  (let ((new '())
	(tmp '()))
    (do ((part *interpreter-funname-table* (cdr part)))
	((null? part)
	 (set! *interpreter-funname-table* new))
      (set! tmp (assq (caar part) *wrapper-table*))
      (if tmp
	  (set! new (cons (cons (cadr tmp) (cdar part)) new))
	  (begin
	    (set! tmp (assq (caar part) *export-table*))
	    (if tmp
		(set! new (cons (cons (cadr tmp) (cdar part)) new))
		(set! new (cons (car part) new))))))))



;===================================================================
;
;		vars-simplifying and  lambda-lifting
;
;==================================================================

(define *new-vars-nr-for-topfun* 0)

(define (vars-simplify-wholedef def)
  (let ()
;;;(pretty-print def)
    (set! def (compile-quasiquote def))
;;;(pretty-print def)
    (set! def (normalize-defines def))
    (set! *current-fun-name* (cadr def))
    (set! *top-level-funs* (cons *current-fun-name* *top-level-funs*))
;;;(pretty-print def)
    (set! def (if *full-inlining-flag*
		  (subst-inline-full def)
		  (subst-inline def)))
    (set! def (normalize-delay def))
;;;(pretty-print def)
    (set! def (rename-vars def))
    (set! *new-vars-nr-for-topfun* 0)
    (set! def (normalize def #f 1))
;;;(pretty-print def)
    (set! def (normalize-def-letrecs def))
    (set! def (beautify-lets def))
;;;(pretty-print def)
    def))


;;; flatten-wholedef performs the first normalizing and lambda-lifting pass

(define (flatten-wholedef def)
  (let ()
;;;(newline)
;;;(display "starting to flatten def: ") (newline)
;;;(pretty-print def)
    (set! *current-fun-name* (cadr def))
    (set! def (lettify-lambdas def 100 #t))
;;;(pretty-print def)
    (set! def (remove-lambdasurrounding-let def))
;;;(pretty-print def)
    (set! *new-funs-list* '())
    (set! def (lambda-lift def '() '()))
;;;(pretty-print def)
    (set! *new-funs-list* (cons def *new-funs-list*))
    *new-funs-list*))


(define (lambda-lift term boundvars new-names-args)
  (let ((tmp '()))
    (cond
     ((symbol? term)
      (set! tmp (assq term new-names-args))
      (if tmp
	  (cons (cadr tmp) (make-arglist (caddr tmp) '()))
	  term))
     ((not (pair? term)) term)
     ((eq? (car term) 'quote) term)
     ((eq? (car term) 'lambda)
      (set! tmp (union (args->list (cadr term)) boundvars))
      (cons 'lambda
	    (cons (cadr term)
		  (map (lambda (x)
			 (lambda-lift x tmp new-names-args))
		       (cddr term)))))
     ((memq (car term) '(let let* letrec))
      (lift-let term boundvars new-names-args))
     ((eq? (car term) 'do)
      ;; check next line!!!
      (set! tmp (union (map car (cadr term)) boundvars))
      (cons 'do
	    (cons (map (lambda (x)
			 (if (null? (cddr x))
			     (list (car x)
				   (lambda-lift (cadr x) boundvars
						new-names-args))
			     (list (car x)
				   (lambda-lift (cadr x) boundvars
						new-names-args)
				   (lambda-lift (caddr x)
						tmp
						new-names-args))))
		       (cadr term))
		  (map (lambda (x)
			 (lambda-lift x tmp new-names-args))
		       (cddr term)))))
     ((symbol? (car term))
      (set! tmp (assq (car term) new-names-args))
      (let ((args (map (lambda (x)
			 (lambda-lift x boundvars new-names-args))
		       (cdr term))))
	(if tmp
	    (cons (cadr tmp)
		  (make-arglist (caddr tmp) args))
	    (cons (car term) args))))
     (else
      (cons (lambda-lift (car term) boundvars new-names-args)
	    (map (lambda (x)
		   (lambda-lift x boundvars new-names-args))
		 (cdr term)))))))



(define (lift-let letterm boundvars new-names-args)
  (let* ((bindings (cadr letterm))
	 (newvars (map car bindings))
	 (body (cddr letterm))
	 (fun-bindings
	  (filter (lambda (x)
		    (and (pair? (cadr x))
			 (eq? (caadr x) 'lambda)))
		  bindings))
	 (other-bindings
	  (filter (lambda (x)
		    (not (memq x fun-bindings)))
		  bindings))
	 (next-bound (union (map car other-bindings) boundvars)))

    (cond ((null? fun-bindings))
	  ((memq (car letterm) '(let* let))
	   (set! new-names-args
		 (make-new-funs-let
		  fun-bindings next-bound new-names-args #f)))
	  ((eq? (car letterm) 'letrec)
	   (set! new-names-args
		 (make-new-funs-letrec
		  fun-bindings next-bound new-names-args #f)))
	  (else (report-error "lift-let applied to non-let term " letterm)))

    (cond ((not (null? other-bindings))
	   (cons (car letterm)
		 (cons (map (lambda (x)
			      (list (car x)
				    (lambda-lift (cadr x)
						 next-bound
						 new-names-args)))
			    other-bindings)
		       (map (lambda (x)
			      (lambda-lift x next-bound new-names-args))
			    body))))
	  ((null? (cdr body))
	   (lambda-lift (car body) next-bound new-names-args))
	  (else
	   (lambda-lift (cons 'begin body) next-bound new-names-args)))))


(define (fetchify vars term)
  (if (null? vars) term (fetchify-aux vars term)))

(define (fetchify-aux vars term)
  (cond ((symbol? term)
	 (if (memq term vars)
	     (list *c-fetch* term)
	     term))
	((not (pair? term))
	 term)
	((eq? 'quote (car term))
	 term)
	((and (eq? *c-adr* (car term))
	      (memq (cadr term) vars))
	 (cadr term))
	(else
	 (cons (fetchify-aux vars (car term))
	       (fetchify-aux vars (cdr term))))))


(define (make-arglist new-args args)
  (if (null? (cadr new-args))
      (append (car new-args) args)
      (append (map (lambda (x) (list *c-adr* x)) (cadr new-args))
	      (car new-args)
	      args)))


(define (normalize-def-letrecs def)
  (let ((tmp '()))
    (set! *current-fun-name* (cadr def))
    (set! tmp
	  (normalize-def-letrecs-aux (caddr def)))
    (list* (car def) (cadr def) (list tmp))))

(define (normalize-def-letrecs-aux term)
  (cond ((not (pair? term)) term)
	((eq? 'quote (car term)) term)
	((eq? 'lambda (car term))
	 (list* (car term) (cadr term)
		(map normalize-def-letrecs-aux (cddr term))))
	((eq? 'letrec (car term))
	 (if (null? (cadr term))
	     (list* 'let* '() (map normalize-def-letrecs-aux (cddr term)))
	     (restructure-letrec (map normalize-def-letrecs-aux term))))
	(else
	 (map normalize-def-letrecs-aux term))))

(define (restructure-letrec letterm)
  (let* ((vars (map car (cadr letterm)))
	 (dependencies
	  (map (lambda (x)
		 (list (car x)
		       (occurrences-of vars (cadr x))))
	       (cadr letterm)))
	 (groups (build-sconnected-groups dependencies vars '())))
    (set! groups (topo-sort dependencies groups))
    (build-letrec-struct letterm dependencies groups)))


(define (build-letrec-struct letterm deps groups)
  (if (null? groups)
      (list (cddr letterm))
      (let ((bind (filter (lambda (x) (memq (car x) (car groups)))
			  (cadr letterm)))
	    (body (build-letrec-struct letterm deps (cdr groups))))
	(cond
	 ((and (null? (cdar groups))
	       (not (memq (caar groups) (cadr (assq (caar groups) deps)))))
	  (cons 'let
		(if (symbol? (car body))
		    (list bind body)
		    (cons bind (car body)))))
	 (else
	  (cons 'letrec
		(if (symbol? (car body))
		    (list bind body)
		    (cons bind (car body)))))))))

;;; lettify-lambdas has a topflag parameter, which is true iff
;;; the term is a third arg of a toplevel def

(define (lettify-lambdas term var-nr topflag)
  (cond ((not (pair? term)) term)
	((eq? 'quote (car term)) term)
	((memq (car term) '(define lambda))
	 (if (not (list? (cddr term)))
	     (report-error
	      *current-fun-name* " has incorrect syntax."))
	 (cons (car term)
	       (cons (cadr term)
		     (map (lambda (x) (lettify-lambdas x var-nr topflag))
			  (cddr term)))))
	((and (pair? term)
	      (not (list? term)))
	 (report-error
	  *current-fun-name* " has incorrect syntax."))
	((memq (car term) '(let let* letrec))
	 (if (not topflag)
	     (cons (car term)
		   (list* (map (lambda (x)
				 (list (car x)
				       (lettify-lambdas
					(cadr x) var-nr #f)))
			       (cadr term))
			  (lettify-lambdas (cddr term) var-nr #f)))
	     (cons (car term)
		   (list* (map (lambda (x)
				 (list (car x)
				       (lettify-lambdas
					(cadr x) var-nr #f)))
			       (cadr term))
			  (map
			   (lambda (x) (lettify-lambdas x var-nr #f))
			   (cddr term))))))

	((and (memq (car term) '(cond))
	      (find-if (lambda (cl)
			 (find-if (lambda (x) (and (pair? x) (eq? 'lambda (car x))))
				  (cdr cl)))
		       (cdr term)))
	 (let* ((lcl
		 (find-if (lambda (cl)
			    (find-if (lambda (x) (and(pair? x) (eq? 'lambda (car x))))
				     (cdr cl)))
			  (cdr term)))
		(lterm
		 (find-if (lambda (x) (and(pair? x) (eq? 'lambda (car x)))) lcl))
		(newvar (make-new-funname))
		(newlcl (replaceq lterm newvar lcl))
		(newclauses (replaceq lcl newlcl (cdr term))))
	   `(let ((,newvar ,(lettify-lambdas lterm (+ 1 var-nr) #f)))
	      ,(lettify-lambdas
		(cons 'cond newclauses) (+ 1 var-nr) #f))))
	((find-if (lambda (x) (and (pair? x) (eq? 'lambda (car x)))) term)

	 (let* ((lterm (find-if (lambda (x) (and (pair? x) (eq? 'lambda (car x))))
				term))
		(newvar (make-new-funname))
		(newterm (replaceq lterm newvar term)))
	   `(let ((,newvar ,(lettify-lambdas lterm  (+ 1 var-nr) #f)))
	      ,(lettify-lambdas newterm (+ 1 var-nr) #f))))
	(else
	 (map (lambda (x) (lettify-lambdas x var-nr #f)) term))))


(define (make-new-funname)
  (set! *new-fun-nr* (+ 1 *new-fun-nr*))
  (let ((name
	 (string->symbol
	  (string-append (symbol->string *current-fun-name*)
			 *new-letfun-infix*
			 (number->string *new-fun-nr*)))))
    (set! *new-fun-names* (cons name *new-fun-names*))
    name))


(define (beautify-lets term)
  (cond ((not (pair? term)) term)
	((eq? 'quote (car term)) term)
	((eq? (car term) 'lambda)
	 (cons (car term)
	       (cons (cadr term)
		     (map beautify-lets (cddr term)))))
	((and (memq (car term) '(let let*))
	      (eq? 3 (length term))
	      (pair? (caddr term))
	      (memq (car (caddr term)) '(let let*)))
	 (beautify-lets
	  (list* 'let*
		 (map beautify-lets
		      (append (cadr term) (cadr (caddr term))))
		 (cddr (caddr term)))))
	(else (map beautify-lets term))))


;--------------------------------------------------------------
;
;		topological sorting by dependencies
;
;--------------------------------------------------------------

(define (topo-sort deps groups)
  (let ((res (cons '() '())))
    (do ((part groups (cdr part)))
	((null? part) (cdr res))
      (topo-insert (car part) res deps))))

(define (topo-insert el lst deps)
  (let ((found-flag #f))
    (do ((last-part lst (cdr last-part)))
	((or found-flag (null? (cdr last-part)))
	 (if (not found-flag)
	     (set-cdr! last-part (list el)))
	 lst)
      (if (is-path? (caadr last-part) (car el) deps '())
	  (begin
	    (set-cdr! last-part (cons el (cdr last-part)))
	    (set! found-flag #t))))))

(define (build-sconnected-groups deps input groups)
  (let ((tmp '()))
    (cond
     ((null? input) groups)
     ((begin (set! tmp
		   (find-if
		    (lambda (grp)
		      (and (not (null? (cdr grp)))
			   (is-path? (car input) (car grp) deps '())
			   (is-path? (car grp) (car input) deps '())))
		    groups))
	     tmp)
      (build-sconnected-groups deps (cdr input)
			       (cons (cons (car input) tmp) (remove tmp groups))))
     ((begin (set! tmp
		   (find-if
		    (lambda (in)
		      (and (is-path? (car input) in deps '())
			   (is-path? in (car input) deps '())))
		    (cdr input)))
	     tmp)
      (build-sconnected-groups deps (remove tmp (cdr input))
			       (cons (list (car input) tmp) groups)))
     (else
      (build-sconnected-groups deps (cdr input)
			       (cons (list (car input)) groups))))))


(define (is-path? a b deps visited)
  (set! visited (cons a visited))
  (set! a (cadr (assq a deps)))
  (or (memq b a)
      (find-if (lambda (x)
		 (and (not (memq x visited))
		      (is-path? x b deps visited)))
	       a)))

(define (occurrences-of vars term)
  (cond ((symbol? term)
	 (if (memq term vars)
	     (list term)
	     '()))
	((not (pair? term)) '())
	((eq? (car term) 'quote) '())
	(else
	 (union (occurrences-of vars (car term))
		(occurrences-of vars (cdr term))))))


;---------------------------------------------------------------------
;
;		   build auxiliary functions
;
;--------------------------------------------------------------------


(define (make-new-funs-let fun-bindings boundvars new-names-args lazy-flag)
  (for-each
   (lambda (b)
     (let* ((freevars (merge-free-vars
		       (introduced-free-vars (cadr b) new-names-args)
		       (free-vars (cadr b) boundvars '())))
	    (new-name (make-new-funname))
	    (tmp (list 'define
		       new-name
		       (cons (caadr b)
			     (cons (make-arglist freevars (cadadr b))
				   (map (lambda (y)
					  (fetchify
					   (cadr freevars)
					   (lambda-lift
					    y
					    (union (args->list (cadadr b))
						   boundvars)
					    new-names-args)))
					(cddadr b)))))))
       (set! *new-funs-list* (cons tmp *new-funs-list*))
       (set! new-names-args
	     (cons (list (car b) new-name freevars)
		   new-names-args))))
   fun-bindings)
  new-names-args)

(define (make-new-funs-letrec fun-bindings boundvars new-names-args lazy-flag)
  (let* ((fun-bodies (cons 'begin (map cadr fun-bindings)))
	 (intro-vars (introduced-free-vars fun-bodies new-names-args))
	 (freevars (merge-free-vars intro-vars
				    (free-vars fun-bodies boundvars '())))
	 (new-names-args
	  (append
	   (map (lambda (b)
		  (list (car b) (make-new-funname) freevars))
		fun-bindings)
	   new-names-args)))
    (for-each
     (lambda (b)
       (set! *new-funs-list*
	     (cons
	      (list 'define
		    (cadr (assq (car b) new-names-args))
		    (cons (caadr b)
			  (cons (make-arglist freevars (cadadr b))
				(map (lambda (y)
				       (fetchify (cadr freevars)
						 (lambda-lift y
							      (union
							       (args->list (cadadr b))
							       boundvars)
							      new-names-args)))
				     (cddadr b)))))
	      *new-funs-list*)))
     fun-bindings)
    new-names-args))


(define (introduced-free-vars term names-args)
  (if (null? names-args)
      (list '() '())
      (introduced-free-vars-aux term names-args)))

(define (introduced-free-vars-aux term names-args)
  (cond ((symbol? term)
	 (let ((tmp (assq term names-args)))
	   (if tmp (caddr tmp) '(() ()))))
	((not (pair? term)) '(() ()))
	((eq? 'quote (car term)) '(() ()))
	(else
	 (merge-free-vars
	  (introduced-free-vars-aux (car term) names-args)
	  (introduced-free-vars-aux (cdr term) names-args)))))


(define (new-fun-name a)
  (if (memq a *new-fun-names*)
      a
      (string->symbol
       (string-append (symbol->string a)
		      *new-fun-infix*
		      (begin (set! *new-fun-nr* (+ 1 *new-fun-nr*))
			     (number->string *new-fun-nr*))))))


;-------------------------------------------------------------------
;
;		    free-vars collectors
;
;------------------------------------------------------------------


;;; all-free-vars takes a term and returns a list (a set) of all
;;; all free variables in term.

(define (all-free-vars term)
  (set! *free-vars-list* '())
  (all-free-aux! term '())
  *free-vars-list*)

(define (all-free-aux! term bound)
  (cond
   ((symbol? term)
    (if (and (not (memq term bound))
	     (not (memq term *free-vars-list*)))
	(set! *free-vars-list* (cons term *free-vars-list*))))
   ((not (pair? term)))
   ((eq? 'quote (car term)))
   ((eq? 'lambda (car term))
    (let ((new (union (args->list (cadr term)) bound)))
      (for-each (lambda (x) (all-free-aux! x new)) (cddr term))))
   ((eq? 'let (car term))
    (let ((new (union (map car (cadr term)) bound)))
      (for-each (lambda (x) (all-free-aux! (cadr x) bound)) (cadr term))
      (for-each (lambda (x) (all-free-aux! x new)) (cddr term))))
   ((eq? 'let* (car term))
    (for-each (lambda (x)
		(all-free-aux! (cadr x) bound)
		(if (not (memq (car x) bound))
		    (set! bound (cons (car x) bound))))
	      (cadr term))
    (for-each (lambda (x) (all-free-aux! x bound)) (cddr term)))
   ((eq? 'letrec (car term))
    (set! bound (union (map car (cadr term)) bound))
    (for-each (lambda (x) (all-free-aux! (cadr x) bound)) (cadr term))
    (for-each (lambda (x) (all-free-aux! x bound)) (cddr term)))
   ((eq? 'do (car term))
    (let ((new (union (map car (cadr term)) bound)))
      (for-each (lambda (x) (all-free-aux! (cadr x) bound)) (cadr term))
      (for-each (lambda (x)
		  (if (not (null? (cddr x)))
		      (all-free-aux! (caddr x) new)))
		(cadr term))
      (for-each (lambda (x) (all-free-aux! x new)) (caddr term))
      (for-each (lambda (x) (all-free-aux! x new)) (cdddr term))))
   (else
    (for-each (lambda (x) (all-free-aux! x bound)) term))))


;;; free-vars takes a term, a list of candidates for free vars (vars bound
;;; somewhere higher in the term) and a list of bound variables.
;;; The list of candidates is used in order not to consider the global
;;; variables (external function definitions, *vars*, etc) to be free.
;;; It returns a list of two disjoint sets: (<ordinary-free> <set!-free>),
;;; where <set!-free> is a list of free variables which have a set!
;;; applied to them somewhere in the term.
;;;	The differentiation is important, as ordinary (non-set!)
;;; free variables are passed as ordinary additional variables
;;; during lambda-lifting, whereas set!-variables have to be passed
;;; by reference and treated accordingly (fortunately this is simple
;;; in C: instead of x always write (*x)).


(define (free-vars term checkvars boundvars)
  (cond ((and (symbol? term) (memq term checkvars))
	 (if (memq term boundvars)
	     '(() ())
	     (list (list term) '())))
	((not (pair? term)) '(() ()))
	((eq? (car term) 'quote) '(() ()))
	((eq? (car term) 'set!)
	 (if (or (memq (cadr term) boundvars)
		 (not (memq (cadr term) checkvars)))
	     (free-vars (caddr term) checkvars boundvars)
	     (merge-free-vars (list '() (list (cadr term)))
			      (free-vars (caddr term) checkvars boundvars))))
	((eq? (car term) 'lambda)
	 (free-vars (cddr term)
		    checkvars
		    (append (args->list (cadr term))
			    boundvars )))
	((memq (car term) '(let let* letrec))
	 (free-vars (append (map cadr (cadr term))
			    (cddr term))
		    checkvars
		    (append (map car (cadr term))
			    boundvars )))
	((eq? (car term) 'do)
	 (free-vars (append (map cadr (cadr term))
			    (map (lambda (x)
				   (if (null? (cddr x)) 1 (caddr x)))
				 (cadr term))
			    (cddr term))
		    checkvars
		    (append (map car (cadr term))
			    boundvars )))
	(else
	 (merge-free-vars (free-vars (car term) checkvars boundvars)
			  (free-vars (cdr term) checkvars boundvars)))))

(define (merge-free-vars pair-a pair-b)
  (let* ((norm-a (car pair-a))
	 (norm-b (car pair-b))
	 (set-a  (cadr pair-a))
	 (set-b  (cadr pair-b))
	 (set-res (union set-a set-b)))
    (list (set-difference (union norm-a norm-b) set-res)
	  set-res )))

(define *var-nr* 0)

;================================================================
;
;	     substituting in inlined-functions and
;	      converting one-arg map-s to map1-s
;
;================================================================


(define (subst-inline-full term)
  (let ((new (subst-inline term)))
    (if (equal? term new)
	term
	(subst-inline-full new))))


(define (subst-inline term)
  (cond ((symbol? term)
	 (let ((tmp (assq term *inline-vars-data*)))
	   (if tmp (cadr tmp) term)))
	((not (pair? term)) term)
	((eq? 'quote (car term))
	 term)
	((and (eq? (car term) 'map)
	      (= 3 (length term))
	      (not *always-map->do-flag*)
	      (guaranteed-all-liftable? (list (car term) (cadr term))))
	 (set! *map1-needed-flag* #t)
	 (if (or (pair? (cadr term))
		 (top-nonlist-in-file-defined? (cadr term)))
	     (subst-inline (cons *map1-function* (cdr term)))
	     (subst-inline
	      (list *map1-function*
		    `(lambda (x) (,(cadr term) x))
		    (caddr term)))))
	((and (eq? (car term) 'for-each)
	      (= 3 (length term))
	      (not *always-for-each->do-flag*)
	      (guaranteed-all-liftable? (list (car term) (cadr term))))
	 (set! *for-each1-needed-flag* #t)
	 (if (or (pair? (cadr term))
		 (top-nonlist-in-file-defined? (cadr term)))
	     (subst-inline (cons *for-each1-function* (cdr term)))
	     (subst-inline
	      (list *for-each1-function*
		    `(lambda (x) (,(cadr term) x))
		    (caddr term)))))
	((memq (car term) *inline-funs*)
	 (let ((data (assq (car term) *inline-funs-data*))
	       (tmp (subst-inline (cdr term))))
	   (subst-inline-aux
	    (caddr (cadr data))
	    (map (lambda (par arg)
		   (list par arg))
		 (cadr (cadr data))
		 tmp))))
	((and (eq? (car term) 'set!)
	      (assq (cadr term) *inline-vars-data*))
	 (cons 'set! (cons (cadr term) (map subst-inline (cddr term)))))
	((list? term)
	 (map subst-inline term))
	(else
	 term)))


(define (subst-inline-aux term pairs)
  (cond ((symbol? term)
	 (let ((tmp (assq term pairs)))
	   (if tmp
	       (cadr tmp)
	       term)))
	((not (pair? term))
	 term)
	(else
	 (cons (subst-inline-aux (car term) pairs)
	       (subst-inline-aux (cdr term) pairs)))))

;================================================================
;
;	     normalization (simplifying transformation)
;
;================================================================

;;; normalize is a main normalizing function, which should
;;; normalize a term in one pass.
;;;
;;; MB! Quasiquote-compiler, normalize-defines and rename-vars
;;; must have been applied before the application of the current
;;; transformer.

(define (normalize term bool-flag var-nr)
  (cond ((not (pair? term)) term)
	((eq? (car term) 'quote) term)
	((memq (car term) '(set! set-car! set-cdr! vector-set!))
	 (list 'begin
	       (map (lambda (x) (normalize x bool-flag var-nr)) term)
	       *unspecified*))
	((eq? (car term) 'if)
	 (normalize-if (cdr term) bool-flag var-nr))
	((eq? (car term) 'cond)
	 (normalize-cond (cdr term) bool-flag var-nr))
	((eq? (car term) 'not)
	 (normalize-not (cdr term) bool-flag var-nr))
	((eq? (car term) 'and)
	 (normalize-and (cdr term) bool-flag var-nr))
	((eq? (car term) 'or)
	 (normalize-or (cdr term) bool-flag var-nr))
	((eq? (car term) 'case)
	 (normalize-case term bool-flag var-nr))
	((eq? (car term) 'do)
	 (normalize-do (cdr term) bool-flag var-nr))
	((eq? (car term) 'lambda)
	 (cons (car term)
	       (cons (cadr term)
		     (normalize (cddr term) bool-flag var-nr))))
	((eq? 'letrec (car term))
	 ;;(restructure-letrec
	 ;;	 (map (lambda (x) (normalize x bool-flag var-nr)) term))
	 (map (lambda (x) (normalize x bool-flag var-nr)) term))
        ((modified-fun? (car term))
	 (map (lambda (x) (normalize x bool-flag var-nr)) term))

	((eq? (car term) 'list)
	 (normalize-list term bool-flag var-nr))
	((eq? (car term) 'for-each)
	 (for-each->do term bool-flag var-nr))
	((eq? (car term) 'map)
	 (map->do term bool-flag var-nr))
	((eq? (car term) 'open-input-file)
	 (normalize-open-input-file (cdr term) bool-flag var-nr))
	((eq? (car term) 'open-output-file)
	 (normalize-open-output-file (cdr term) bool-flag var-nr))
	((eq? (car term) 'call-with-input-file)
	 (normalize-call-with-input-file (cdr term) bool-flag var-nr))
	((eq? (car term) 'call-with-output-file)
	 (normalize-call-with-output-file (cdr term) bool-flag var-nr))
	((eq? (car term) 'with-input-from-file)
	 (normalize-with-input-from-file (cdr term) bool-flag var-nr))
	((eq? (car term) 'with-output-to-file)
	 (normalize-with-output-to-file (cdr term) bool-flag var-nr))
	((eq? 'string-append (car term))
	 (normalize-string-append term bool-flag var-nr))
	((assq (car term) *associative-fun-table*)
	 (normalize-associative
	  (assq (car term) *associative-fun-table*)
	  (cdr term) bool-flag var-nr))
	((assq (car term) *comparison-fun-table*)
	 (normalize-comparison
	  (assq (car term) *comparison-fun-table*)
	  (cdr term) bool-flag var-nr))
	(else
	 (map (lambda (x) (normalize x bool-flag var-nr)) term))))


;;; for-each->do converts a for-each application to a do cycle.
;;; The aim is to convert a do cycle into the C for cycle later.
;;;
;;; NB! here and in the following transformers bool-flag denotes
;;; whether the current term occurs as a term of boolean type -
;;; eg, (if (for-each ....) term1 term2). This allows some
;;; optimizations (although not directly in for-each, of course).
;;;
;;; var-nr is a number of the last generated new variable.


(define (for-each->do term bool-flag var-nr)
  (let* ((fun (cadr term))
	 (args (cddr term))
	 (names (map (lambda (x)
		       (set! var-nr (+ 1 var-nr)) (make-new-var var-nr))
		     args )))
    `(do
	 ,(map (lambda (x y) (list x y (list 'cdr x)))
	       names
	       (map (lambda (x) (normalize x #f (+ 1 var-nr))) args))
	 ,(list (normalize
		 (if (null? (cdr args))
		     (list 'not (list 'pair? (car names)))
		     (list 'not (cons 'and (map (lambda (x)
						  (list 'pair? x))
						names))))
		 #t var-nr)
		*unspecified*)
       ,(cons (normalize fun #f (+ 1 var-nr))
	      (map (lambda (x) (list 'car x)) names)))))


;;; map->do converts a map application to a do cycle.

(define (map->do term bool-flag var-nr)
  (let* ((fun (cadr term))
	 (args (cddr term))
	 (res (begin (set! var-nr (+ 1 var-nr)) (make-new-var var-nr)))
	 (res-end (begin (set! var-nr (+ 1 var-nr)) (make-new-var var-nr)))
	 (tmp (begin (set! var-nr (+ 1 var-nr)) (make-new-var var-nr)))
	 (names (map (lambda (x)
		       (set! var-nr (+ 1 var-nr)) (make-new-var var-nr))
		     args )))
    `(do
	 (,@(map (lambda (x y) (list x y (list 'cdr x)))
		 names
		 (map (lambda (x) (normalize x #f (+ 1 var-nr))) args))
	  (,res '())
	  (,res-end '())
	  (,tmp '()))
	 ,(list (normalize
		 (if (null? (cdr args))
		     (list 'not (list 'pair? (car names)))
		     (list 'not (cons 'and (map (lambda (x)
						  (list 'pair? x))
						names))))
		 #t var-nr)
		res)
       (set! ,tmp ,(normalize
		    (cons fun (map (lambda (x) (list 'car x)) names))
		    #f (+ 1 var-nr)))
       (if (null? ,res)
	   (begin (set! ,res (cons ,tmp '()))
		  (set! ,res-end ,res))
	   (begin (set-cdr! ,res-end (cons ,tmp '()))
		  (set! ,res-end (cdr ,res-end)))))))



(define (normalize-if term bool-flag var-nr)
  (if (null? (cddr term))
      (list 'if
	    (normalize (car term) #t var-nr)
	    (normalize (cadr term) bool-flag var-nr)
	    *unspecified*)
      (list 'if
	    (normalize (car term) #t var-nr)
	    (normalize (cadr term) bool-flag var-nr)
	    (normalize (caddr term) bool-flag var-nr))))


(define (normalize-do term bool-flag var-nr)
  (if (or (null? (car term))
	  (null? (cdar term)))
      (list* 'do
	     (map (lambda (x) (normalize x #f var-nr))
		  (car term))
	     (cons (normalize (caadr term) #t var-nr)
		   (map (lambda (x) (normalize x #f var-nr))
			(cdadr term)))
	     (map (lambda (x) (normalize x #f var-nr))
		  (cddr term)))
      (begin
	(let* ((actual (filter (lambda (x) (not (null? (cddr x))))
			       (car term)))
	       (non-actual (filter (lambda (x) (null? (cddr x)))
				   (car term)))
	       (vars (map car actual))
	       (inits (map cadr actual))
	       (bodies (map caddr actual))
	       (new-var '())
	       (new-var-pairs '())
	       (new-bodies '()))
	  (do ((part actual (cdr part))
	       (vars-part vars (cdr vars-part))
	       (bodies-part bodies (cdr bodies-part)))
	      ((null? part))
	    (if (inside-term? (car vars-part) (cdr bodies-part))
		(begin
		  (set! var-nr (+ 1 var-nr))
		  (set! new-var (make-new-var var-nr))
		  (set! new-var-pairs
			(cons (list new-var (car vars-part))
			      new-var-pairs))
		  (set! bodies-part
			(cons (car bodies-part)
			      (subst-term new-var
					  (car vars-part)
					  (cdr bodies-part))))
		  (set! new-bodies
			(cons (car bodies-part) new-bodies)))
		(begin
		  (set! new-bodies
			(cons (car bodies-part) new-bodies)))))
	  (if (null? new-var-pairs)
	      (list* 'do
		     (map (lambda (x) (normalize x #f var-nr))
			  (car term))
		     (cons (normalize (caadr term) #t var-nr)
			   (map (lambda (x) (normalize x #f var-nr))
				(cdadr term)))
		     (map (lambda (x) (normalize x #f var-nr))
			  (cddr term)))
	      (list 'let*
		    (append
		     non-actual
		     (map (lambda (x) (list (car x) *dummy*)) new-var-pairs))
		    (list* 'do
			   (map (lambda (x y z)
				  (list x (normalize y #f var-nr)
					(normalize z #f var-nr)))
				vars
				inits
				(reverse new-bodies))
			   (cons (normalize (caadr term) #t var-nr)
				 (map (lambda (x) (normalize x #f var-nr))
				      (cdadr term)))
			   (append (map (lambda (x) (normalize x #f var-nr))
					(cddr term))
				   (map (lambda (x) (cons 'set! x))
					new-var-pairs)))))))))




;;; normalize-cond is one of the main transformers.
;;; It converts a cond to the if-ladder, introducing
;;; lets and new variables where needed.
;;;
;;; NB! In the following *and?* and *or?* are special new functions,
;;; which are considered to be strictly boolean, and can be
;;; converted directly to corresponding C operators.

(define (normalize-cond term bool-flag var-nr)
  (cond
   ((null? term) *unspecified*)
   ((null? (cdar term))
    (if bool-flag
	`(*and?* ,(normalize (caar term) #t var-nr)
		 ,(normalize-cond (cdr term) #t var-nr))
	(let ((new-var (make-new-var (+ 1 var-nr))))
	  `(let* ((,new-var ,(normalize (caar term) #f (+ 1 var-nr))))
	     (if ,new-var ,new-var
		 ,(normalize-cond (cdr term) #f var-nr))))))
   ((eq? (cadar term) '=>)
    (let ((new-var (make-new-var (+ 1 var-nr))))
      `(let* ((,new-var ,(normalize (caar term) #f (+ 1 var-nr))))
	 (if ,new-var
	     ,(normalize (list (caddar term) new-var)
			 bool-flag (+ 1 var-nr))
	     ,(normalize-cond (cdr term) bool-flag (+ 1 var-nr))))))
   ((eq? (caar term) 'else)
    (if (null? (cddar term))
	(normalize (cadar term) bool-flag var-nr)
	(normalize (cons 'begin (cdar term)) bool-flag var-nr)))
   ((null? (cddar term))
    `(if ,(normalize (caar term) #t var-nr)
	 ,(normalize (cadar term) bool-flag var-nr)
	 ,(normalize-cond (cdr term) bool-flag var-nr)))
   (else
    `(if ,(normalize (caar term) #t var-nr)
	 ,(normalize (cons 'begin (cdar term)) bool-flag var-nr)
	 ,(normalize-cond (cdr term) bool-flag var-nr)))))


;;; normalize-not creates a c-not (*not?* => !) or a scheme-not (not)

(define (normalize-not lst bool-flag var-nr)
  (if bool-flag
      (normalize (cons *not?* lst) #t var-nr)
      (list 'not (normalize (car lst) #t var-nr))))

;;; normalize-and and normalize-or make some optimizations
;;; and convert terms to if-ladders of *and?* and *or?*.

(define (normalize-and lst bool-flag var-nr)
  (cond ((null? lst) #t)
	((null? (cdr lst)) (normalize (car lst) bool-flag var-nr))
	((and bool-flag (not *lift-and-or-flag*))
	 (normalize (cons *and?* lst) #t var-nr))
	(else
	 `(if ,(normalize (car lst) #t var-nr)
	      ,(normalize-and (cdr lst) bool-flag var-nr)
	      #f ))))


(define (normalize-or lst bool-flag var-nr)
  (cond ((null? lst) #f)
	((null? (cdr lst)) (normalize (car lst) bool-flag var-nr))
	((and bool-flag (not *lift-and-or-flag*))
	 (normalize (cons *or?* lst) #t var-nr))
	(bool-flag
	 `(if ,(normalize (car lst) #t var-nr)
	      #t
	      ,(normalize-or (cdr lst) #t var-nr)))
	(else
	 (normalize `(cond ,@(map list (butlast lst 1))
			   (else ,(car (my-last-pair lst))))
		    bool-flag var-nr ))))


;;; normalize-case does the obvious thing.

(define (normalize-case term bool-flag var-nr)
  (let* ((new-var (make-new-var (+ 1 var-nr)))
	 (tmp
	  `(let* ((,new-var ,(cadr term)))
	     (cond
	      ,@(normalize-case-aux new-var (cddr term))))))
    (normalize tmp bool-flag (+ 1 var-nr))))


(define (normalize-case-aux var lst)
  (cond ((null? lst) '())
	((eq? (caar lst) 'else) (list (car lst)))
	((list? (caar lst))
	 (append (map (lambda (x) `((eqv? (quote ,x) ,var) ,@(cdar lst)))
		      (caar lst))
		 (normalize-case-aux var (cdr lst))))
	(else (report-error "Bad case clause syntax:" lst))))



;;; file-opening and calling with normalization assumes a single
;;; generic file opening two-arg function *open-file-function* and
;;; corresponding strings for input and output.
;;; Calling with files is normalized into a let with assuming
;;; a function *set-current-input-port-function* and a function
;;;  *set-current-output-port-function*

(define (normalize-open-input-file term bool-flag var-nr)
  (list *open-file-function*
	(normalize (car term) #f var-nr)
	*input-file-modifier*))

(define (normalize-open-output-file term bool-flag var-nr)
  (list *open-file-function*
	(normalize (car term) #f var-nr)
	*output-file-modifier*))

(define (normalize-with-input-from-file term bool-flag var-nr)
  (let* ((new-var1 (make-new-var (+ 1 var-nr)))
	 (new-var2 (make-new-var (+ 2 var-nr)))
	 (new-var3 (make-new-var (+ 3 var-nr))))
    `(let* ((,new-var1 (,*open-file-function*
			,(normalize (car term) #f new-var3)
			,*input-file-modifier*))
	    (,new-var2 (,*set-current-input-port-function* ,new-var1))
	    (,new-var3 (,(normalize (cadr term) bool-flag new-var3))))
       (close-input-port ,new-var1)
       (,*set-current-input-port-function* ,new-var2)
       ,new-var3)))

(define (normalize-with-output-to-file term bool-flag var-nr)
  (let* ((new-var1 (make-new-var (+ 1 var-nr)))
	 (new-var2 (make-new-var (+ 2 var-nr)))
	 (new-var3 (make-new-var (+ 3 var-nr))))
    `(let* ((,new-var1 (,*open-file-function*
			,(normalize (car term) #f new-var3)
			,*output-file-modifier*))
	    (,new-var2 (,*set-current-output-port-function* ,new-var1))
	    (,new-var3 (,(normalize (cadr term) bool-flag new-var3))))
       (,*set-current-output-port-function* ,new-var2)
       (close-output-port ,new-var1)
       ,new-var3)))

(define (normalize-call-with-input-file term bool-flag var-nr)
  (let* ((new-var1 (make-new-var (+ 1 var-nr)))
	 (new-var2 (make-new-var (+ 2 var-nr))))
    `(let* ((,new-var1 (,*open-file-function*
			,(normalize (car term) #f new-var2)
			,*input-file-modifier*))
	    (,new-var2 (,(normalize (cadr term) bool-flag new-var2)
			,new-var1)))
       (close-input-port ,new-var1)
       ,new-var2)))


(define (normalize-call-with-output-file term bool-flag var-nr)
  (let* ((new-var1 (make-new-var (+ 1 var-nr)))
	 (new-var2 (make-new-var (+ 2 var-nr))))
    `(let* ((,new-var1 (,*open-file-function*
			,(normalize (car term) #f new-var2)
			,*output-file-modifier*))
	    (,new-var2 (,(normalize (cadr term) bool-flag new-var2)
			,new-var1)))
       (close-output-port ,new-var1)
       ,new-var2)))


;;; The following normalize-comparisons and
;;; normalize-associative convert associative functions into
;;; functions of exactly the arity two. List function is replaced
;;; by a corresponding cons structure.

(define (normalize-list term bool-flag var-nr)
  (normalize (normalize-list-aux (cdr term)) bool-flag var-nr))

(define (normalize-list-aux lst)
  (cond ((null? lst) ''())
	((null? (cdr lst)) `(cons ,(car lst) '()))
	(else
	 `(cons ,(car lst)
		,(normalize-list-aux (cdr lst))))))

(define (normalize-list-for-c lst)
  (cond ((null? lst) '())
	((null? (cdr lst)) `(cons ,(car lst) ()))
	(else
	 `(cons ,(car lst)
		,(normalize-list-for-c (cdr lst))))))

(define (normalize-comparison data lst bool-flag var-nr)
  (cond ((null? lst) (report-error "too few args in comparison " (car data)))
	((null? (cdr lst))
	 (report-error "too few args in comparison " (car data)))
	((null? (cddr lst))
	 (list (car data)
	       (normalize (car lst)  #f var-nr)
	       (normalize (cadr lst) #f var-nr)))
	;;at least three args left
	(else (list *and?*
		    (normalize-comparison data (butlast lst 1)
					  #t var-nr)
		    (let* ((rev (reverse lst))
			   (tmp (list
				 (normalize (cadr rev) #t var-nr)
				 (normalize (car rev) #t var-nr))))
		      (cons (car data) tmp))))))


(define (normalize-string-append term bool-flag var-nr)
  (list (car term)
	(normalize (normalize-list-aux (cdr term)) #f var-nr)))


(define (normalize-associative data lst bool-flag var-nr)
  (cond ((null? lst) (cadr data))
	((null? (cdr lst))
	 (list (car data)
	       (cadr data)
	       (normalize (car lst) (boolean? (cadr data)) var-nr)))
	((null? (cddr lst))
	 (list (car data)
	       (normalize (car lst) (boolean? (cadr data))  var-nr)
	       (normalize (cadr lst) (boolean? (cadr data)) var-nr)))
	;;at least three args left
	((boolean? (cadr data))		; *or?* and *and?*
	 (list (car data)
	       (normalize (car lst) #t var-nr)
	       (normalize-associative data (cdr lst) #t var-nr)))
	(else
	 (list (car data)
	       (normalize-associative data (butlast lst 1) #f var-nr)
	       (normalize (car (my-last-pair lst)) #f var-nr)))))


(define *associative-fun-table*
  (append
   (list (cons *or?* '(#f bool)) (cons *and?* '(#t bool)))

   '((append '() lst) ;;; (string-append "" str)
     (+ 0 num) (- 0 num) (* 1 num) (/ 1 num) (max -99999 num) (min 99999 num)
     (%+ 0 num) (%- 0 num) (%* 1 num) (%/ 1 num))))

(define *comparison-fun-table*
  '((= num) (< num) (> num) (<= num) (>= num)
    (%= num) (%< num) (%> num) (%<= num) (%>= num)
    (char=? chr) (char<? chr) (char>? chr) (char<=? chr) (char>=? chr)
    (char-ci=? chr) (char-ci<? chr) (char-ci>? chr)
    (char-ci<=? chr) (char-ci>=? chr)
    (string=? str) (string<? str) (string>? str) (string<=? str) (string>=? str)
    (string-ci=? str) (string-ci<? str) (string-ci>? str)
    (string-ci<=? str) (string-ci>=? str)))

; pre-4d-version:
;(define (make-new-var nr)
;  (string->symbol (string-append *new-var-name* (number->string nr))))

; from-4d-version:
(define (make-new-var nr)
  (set! *new-vars-nr-for-topfun* (+ 1 *new-vars-nr-for-topfun*))
  (string->symbol (string-append *new-var-name*
				 (number->string *new-vars-nr-for-topfun*))))

;==================================================================
;
;		  delay transformer
;
;=================================================================


;;; The following normalizes applications of 'delay'.
;;; It should be used as a preprocessor to normalizer.

(define (normalize-delay term)
  (cond ((not (pair? term)) term)
	((not (some-in-fun-position? '(delay force) term))
	 term)
	((eq? (car term) 'quote) term)
	((eq? (car term) 'lambda)
	 `(lambda ,(cadr term) ,@(normalize-delay (cddr term))))
	((eq? (car term) 'define)
	 (cons 'define
	       (cons (cadr term)
		     (normalize-delay (cddr term)))))
	((and (eq? (car term) 'delay)
	      (pair? (cdr term))
	      (null? (cddr term)))
	 `(,*make-promise-function*
	   (lambda () ,(normalize-delay (cadr term)))))
	((and (eq? (car term) 'force)
	      (pair? (cdr term))
	      (null? (cddr term)))
	 `(,*force-function* ,(normalize-delay (cadr term))))
	(else
	 (map normalize-delay term))))


;==================================================================
;
;		  quasiquote transformer
;
;=================================================================


;;; The following compiles quasiquotes. It should be used as a
;;; preprocessor to normalizer. It should compile the full
;;; quasiquote syntax, including nested quasiquotes.

(define (compile-quasiquote term)
  (cond ((not (pair? term)) term)
	((not (occurs-in-function-position? 'quasiquote term)) term)
	((eq? (car term) 'quote) term)
	((eq? (car term) 'lambda)
	 `(lambda ,(cadr term) ,@(compile-quasiquote (cddr term))))
	((eq? (car term) 'define)
	 (cons 'define
	       (cons (cadr term)
		     (compile-quasiquote (cddr term)))))
	((eq? (car term) 'quasiquote)
	 (normalize-quasiquote (cadr term) 1))
	(else
	 (map compile-quasiquote term))))


(define (normalize-quasiquote term depth)
  (cond
;;;     ((not (or (occurs-in-function-position? 'unquote term)
;;;		(occurs-in-function-position? 'unquote-splicing term)))
;;;	 `(quote ,term))
   ((vector? term)
    `(apply vector ,(normalize-quasiquote (vector->list term) depth)))
   ((not (pair? term))
    `(quote ,term))
   ((and (eq? (car term) 'unquote) (eqv? depth 1))
    (car (compile-quasiquote (cdr term))))
   ((not (pair? (car term)))
    `(cons (quote ,(car term))
	   ,(normalize-quasiquote (cdr term) depth)))
   ((eq? (caar term) 'unquote)
    (if (eqv? depth 1)
	`(cons ,(compile-quasiquote (cadar term))
	       ,(normalize-quasiquote (cdr term) depth))
	(list 'cons
	      (list  'cons
		     ''unquote
		     (normalize-quasiquote (cdar term) (- depth 1)))
	      (normalize-quasiquote (cdr term) depth))))
   ((eq? (caar term) 'unquote-splicing)
    (if (eqv? depth 1)
	`(append ,(compile-quasiquote (cadar term))
		 ,(normalize-quasiquote (cdr term) depth))
	(list 'cons
	      (list  'cons
		     ''unquote-splicing
		     (normalize-quasiquote (cdar term) (- depth 1)))
	      (normalize-quasiquote (cdr term) depth))))
   ((eq? (caar term) 'quasiquote)
    `(cons ,(normalize-quasiquote (car term) (+ 1 depth))
	   ,(normalize-quasiquote (cdr term) depth)))
   (else
    `(cons ,(normalize-quasiquote (car term) depth)
	   ,(normalize-quasiquote (cdr term) depth)))))




(define (occurs-in-function-position? f term)
  (and (pair? term)
       (or (and (eq? (car term) f) (list? (cdr term)))
	   (occurs-in-function-position? f (car term))
	   (occurs-in-function-position? f (cdr term)))))

;=============================================================
;
;          removing topmost surrounding let's
;
;===========================================================


(define (remove-lambdasurrounding-let def)
  (if (and (list? def)
	   (eq? 3 (length def))
	   (pair? (caddr def))
	   (or (eq? 'let (car (caddr def)))
	       (eq? 'let* (car (caddr def))))
	   (pair? (cddr (caddr def)))
	   (pair? (caddr (caddr def)))
	   (null? (cdddr (caddr def)))
	   (eq? 'lambda (car (caddr (caddr def)))))
      (remove-lambdasurrounding-let-aux def)
      def))

(define (remove-lambdasurrounding-let-aux def)
  (let* ((letbindings (cadr (caddr def)))
	 (lambdaterm (caddr (caddr def))))
    (set! *global-vars-list*
	  (append *global-vars-list* (map car letbindings)))
    (set! *top-actions-list*
	  (append  (map (lambda (x)
			  (let ((name (make-constant-name)))
			    (set! *var-make-list*
				  (cons
				   (list 'set! (car x)
					 (list 'scm-gc-protect
					       (list *c-adr* name)))
				   *var-make-list*))
			    (set! *via-interpreter-defined*
				  (cons (car x) *via-interpreter-defined*))
			    (list 'set! (car x) (cadr x))))
			letbindings)
		   *top-actions-list*))
    (list (car def) (cadr def) lambdaterm)))




;=============================================================
;
;		      variable renaming
;
;============================================================


;;; rename-vars performs a very important function: it renames
;;; vars, removing clashes of bound variable names.
;;; rename-vars tries to rename as few variables as possible;
;;; in doing that it takes into account that all variable declarations
;;; in the term should be liftable to the very top of the term.
;;;
;;; After applying rename-vars, all variable bindings in lets can
;;; (and should) be changed to simple set!s in the corresponding order.
;;;
;;; That is, the resulting let is actually a let*, or, better yet,
;;; (let ((a b) ... (g h)) ...) should be treated
;;; as (begin (set! a b) ... (set! g h) ...).
;;; All the variables introduced in such lets should be declared
;;; as local variables of a pointer type in the corresponding
;;; c function, and set! should be translated to = in the
;;; obvious way. Thus the resulting let can be translated to the
;;; C block, for example.
;;;
;;; NB! Different types of lets (including the one in do) are all
;;; converted to the scheme explained above.
;;;
;;; NB! Letrec is not handled fully here, in the sense that when
;;; we perform lambda-lifting, there are some special complexities
;;; which must be handled.


(define *passed-locvars-list* '())

(define (rename-vars term)
  (set! *var-nr* 0)
  (set! *passed-locvars-list* '())
  (set! *free-vars-list* (all-free-vars term))
  (rename-vars-aux term '() #t))


;;; rename-vars-aux takes a topflag, which is true iff term is NOT yet
;;; inside some lambdaterm. In that case all the vars bound in let are
;;; renamed by a global scheme in order to be initialized in the
;;; initialization function.

(define (rename-vars-aux term env topflag)
  (cond
   ((symbol? term)
    (cond ((assq term env) => cdr)
	  (else term)))
   ((not (pair? term)) term)
   ((eq? 'quote (car term)) term)
   ((eq? 'lambda (car term))
    `(lambda
	 ,@(rename-vars-aux (cdr term)
			    (make-new-env-lambda
			     (args->list (cadr term))
			     env)
			    #f)))
   ((eq? 'let (car term))
    (let ((new-env (make-new-env (map car (cadr term)) env topflag)))
      `(let
	   ,(map (lambda (x)
		   (list (rename-vars-aux (car x) new-env topflag)
			 (rename-vars-aux (cadr x) env topflag)))
		 (cadr term))
	 ,@(rename-vars-aux (cddr term) new-env topflag))))
   ((eq? 'do (car term))
    (let ((new-env (make-new-env (map car (cadr term)) env #f)))
      `(do
	   ,(map (lambda (x)
		   (cons (rename-vars-aux (car x) new-env #f)
			 (cons (rename-vars-aux (cadr x) env #f)
			       (rename-vars-aux (cddr x)
						new-env #f))))
		 (cadr term))
	   ,@(rename-vars-aux (cddr term) new-env #f))))
   ((eq? 'let* (car term))
    (let ((new-env env)
	  (old-env env)
	  (new-args '()))
      (do ((part (cadr term) (cdr part)))
	  ((null? part)
	   `(let
		,(reverse new-args)
	      ,@(rename-vars-aux (cddr term) new-env topflag)))
	(set! old-env new-env)
	(set! new-env (make-new-env (list (caar part)) new-env topflag))
	(set! new-args
	      (cons (list (rename-vars-aux (caar part) new-env topflag)
			  (rename-vars-aux
			   (cadar part) old-env topflag))
		    new-args )))))
   ((eq? 'letrec (car term))
    (let ((new-env (make-new-env (map car (cadr term)) env topflag)))
      `(letrec ,@(rename-vars-aux (cdr term) new-env topflag))))
   ((eq? 'define (car term))
    (map (lambda (x) (rename-vars-aux x env topflag)) term))
   ((list? term)
    (map (lambda (x) (rename-vars-aux x env #f)) term))
   (else (cons (rename-vars-aux (car term) env #f)
	       (rename-vars-aux (cdr term) env #f)))))


(define (args->list args)
  (cond ((symbol? args) (list args))
	((list? args)
	 (map (lambda (x) (if (pair? x) (cadr x) x)) args))
	((pair? args)
	 (cons (if (pair? (car args)) (cadar args) (car args))
	       (args->list (cdr args))))
	(else (report-error "Bad argument list:" args))))


(define (make-new-env vars env topflag)
  (let ((name '()))
    (append (map (lambda (x)
		   (cond
		    ((or (memq x *new-fun-names*)
			 (and (not (assq x env))
			      (not topflag)
			      (not (memq x *keywords*))
			      (not (memq x *primitives*))
			      (not (memq x *top-level-names*))
			      (not (memq x *passed-locvars-list*))
			      (not (memq x *free-vars-list*))))
		     (set! *passed-locvars-list*
			   (cons x *passed-locvars-list*))
		     (cons x x))
		    ((not topflag)
		     (set! *var-nr* (+ 1 *var-nr*))
		     (cons x
			   (string->symbol
			    (string-append
			     (symbol->string x)
			     *local-var-infix*
			     (number->string *var-nr*)))))
		    (else
		     (set! *new-fun-nr* (+ 1 *new-fun-nr*))
		     (cons x
			   (string->symbol
			    (string-append
			     (symbol->string x)
			     *new-fun-infix*
			     (number->string *new-fun-nr*)))))))
		 vars)
	    env )))

(define (make-new-env-lambda vars env)
  (append (map (lambda (x)
		 (if (or (assq x env)
			 (memq x *keywords*)
			 (memq x *primitives*)
			 (memq x *top-level-names*))
		     (cons x
			   (string->symbol
			    (string-append
			     (symbol->string x)
			     *local-var-infix*
			     (begin (set! *var-nr* (+ 1 *var-nr*))
				    (number->string *var-nr*)))))
		     (cons x x)))
	       vars )
	  env ))


;===============================================================
;
;		  define - transformer
;
;===============================================================

;;; normalize-defines converts fancy defines into basic ones.

(define (normalize-defines term)
  (cond
   ((not (pair? term)) term)
   ((eq? (car term) 'quote) term)
   ((eq? (car term) 'define)
    ;; the coming if removes let in the case:
    ;; (define foo (let ((bar bar)) ...))
    ;;  (if (and (pair? (cdr term))
    ;;		(pair? (cddr term))
    ;;		(pair? (caddr term))
    ;;		(memq (car (caddr term)) '(let let* letrec))
    ;;		(pair? (cadr (caddr term)))
    ;;		(not (find-if (lambda (x) (not (eq? (car x) (cadr x))))
    ;;			   (cadr (caddr term)))))
    ;;	   (set! term (cons 'define (cons (cadr term) (cddr (caddr term))))))
    (if (pair? (cadr term))
	`(define ,(caadr term)
	   ,(normalize-defines
	     (cons 'lambda (cons (cdadr term) (cddr term)))))
	`(define ,(cadr term) ,(normalize-defines (caddr term)))))
   ((and (memq (car term) '(let* letrec))
	 (not (list? (cadr term))))
    (report-error
     "In " *current-fun-name* " there is wrong let: " term))

    ;;; the next case rewrites a named let to a letrec, never succeeds.
   ((begin
      (if (and (eq? (car term) 'let)
	       (not (null? (cdr term)))
	       (not (null? (cddr term)))
	       (symbol? (cadr term))
	       (not (null? (cadr term))))
	   ;;; a named let
	  (if (find-if (lambda (x)
			 (or (null? x) (not (list? x)) (null? (cdr x))))
		       (caddr term))
	      (report-error
	       *current-fun-name*
	       " contains an incorrect named let: " term)
	      (let ((param (map car (caddr term)))
		    (args  (map cadr (caddr term))))
		(set! term
		      (list 'letrec
			    (list
			     (list (cadr term)
				   (list* 'lambda param (cdddr term))))
			    (cons (cadr term) args))))))
      #f))
   ((and (memq (car term) '(lambda let let* letrec do))
	 (pair? (caddr term))
	 (eq? 'define (caaddr term)))
    (let ((defs (normalize-defines-aux (cddr term)))
	  (other (member-if
		  (lambda (x)
		    (or (not (pair? x)) (not (eq? (car x) 'define))))
		  (cddr term))))
      (if (not other) (report-error "Body is missing:" term))
      `(,(car term)
	,(normalize-defines (cadr term))
	,(normalize-defines (cons 'letrec (cons defs other))))))
   ((list? term)
    (map normalize-defines term))
   (else
    (cons (normalize-defines (car term))
	  (normalize-defines (cdr term))))))


(define (normalize-defines-aux lst)
  (if (and (not (null? lst)) (pair? (car lst)) (eq? 'define (caar lst)))
      (cons (cdr (normalize-defines (car lst)))
	    (normalize-defines-aux (cdr lst)))
      '()))

;=================================================================
;
;                     Global analysis
;
;=================================================================


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;
;            analysis for liftability and mutability
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


; lift-analyse-def! takes a def where all the lambdas are renamed
; (by taking extra let-s with names), vars are renamed and letrecs
; are ordered.
;
; it dest. changes the def body by replacing the leading lambda of
; all the liftable lambdaterms with the value of *liftable-lambda*


(define *local-liftnames* '())
(define *liftable-lambda* '**liftable-lambda**)
(define *def-hofname* '())

(define (lift-analyse-def! def)
  (let* ((funname (cadr def))
         (lterm (caddr def)))
    (set! *local-liftnames* '())
    (set! *def-hofname* funname)
    (lift-analyse-def-aux! lterm)
    (if (and (pair? lterm)
	     (not (all-liftable? (cdr lterm))))
	(set! *not-all-liftable-names*
	      (cons funname *not-all-liftable-names*)))))


; lift-analyse-def-aux! term:
;  term is a term
;
;  all lambdaterms must be named.
;
;  liftable lambdas are destr. replaced by *liftable-lambda*,
;  their names are added to *local-liftnames*
;

(define (lift-analyse-def-aux! term)
  (let* ((name '())
	 (passed '())
	 (tmp '()))
    (cond
     ((not (pair? term)))
     ((eq? 'quote (car term)))
     ((eq? 'lambda (car term))
      (for-each (lambda (x) (lift-analyse-def-aux! x)) (cddr term)))
;;;      ((eq? *liftable-lambda* (car term))
;;;         (for-each (lambda (x) (lift-analyse-def-aux! x)) (cddr term)))
     ((and (pair? (car term))
	   (eq? 'lambda (caar term)))
      (for-each lift-analyse-def-aux! (cddar term))
      (if (all-liftable? (cddar term))
	  (set-car! (car term) *liftable-lambda*))
      (for-each lift-analyse-def-aux! (cdr term)))
     ((memq (car term) '(let* let))
      (for-each lift-analyse-def-aux! (cddr term))
      (do ((part (reverse (cadr term)) (cdr part)))
	  ((null? part))
	(set! name (caar part))
	(lift-analyse-def-aux! (cadar part))
	(if (and (pair? (cadar part))
		 (eq? 'lambda (caadar part))
		 (all-liftable? (cddr (cadar part)))
		 (liftable-nameocc? name (cddr (cadar part)))
		 (liftable-nameocc? name (cons 'begin passed))
		 (liftable-nameocc? name (cons 'begin (cddr term))))
	    (begin
	      (set! *local-liftnames*
		    (cons name *local-liftnames*))
	      (set-car! (cadar part) *liftable-lambda*)))
	(set! passed (cons (cadar part) passed))))
     ((eq? (car term) 'letrec)
      (for-each lift-analyse-def-aux! (cddr term))
      (for-each (lambda (el) (lift-analyse-def-aux! (cadr el))) (cadr term))
      (if (and (every1 (lambda (el)
			 (and (pair? (cadr el))
			      (eq? 'lambda (caadr el))
			      (all-liftable? (cddr (cadr el)))))
		       (cadr term))
	       (every1 (lambda (el)
			 (and (liftable-nameocc? (car el)
						 (cons 'begin (cddr term)))
			      (every1 (lambda (el2)
					(liftable-nameocc?
					 (car el) (cddr (cadr el2))))
				      (cadr term))))
		       (cadr term)))
	  (for-each (lambda (el)
		      (set! *local-liftnames*
			    (cons (car el) *local-liftnames*))
		      (set-car! (cadr el) *liftable-lambda*))
		    (cadr term))))
     ((and (liftable-hofname? (car term))
	   (not (eq? (car term) *def-hofname*)))
      (set! tmp (assq (car term) *liftable-hof-database*))
      ;;  (if tmp (begin (newline) (display "term: ") (display term) (newline)))
      (if tmp
	  ;; case for top-level def of a higher-order fun:
	  (if (every1 (lambda (x)
			;;  (newline) (display "x: ") (display x) (newline)
			(let ((param (car x))
			      (arg (cdr x)))
			  ;;  (display "param: ") (display param) (newline)
			  ;;  (display "arg: ") (display arg) (newline)
			  (if param
			      (or (and (pair? arg)
				       (or (eq? 'lambda (car arg))
					   (eq? *liftable-lambda*
						(car arg)))
				       (begin
					 (for-each
					  lift-analyse-def-aux!(cddr arg))
					 ;; (newline) (display "cddr arg:")
					 ;; (display (cddr arg)) (newline)
					 (all-liftable? (cddr arg))))
				  (and (symbol? arg)
				       (memq arg *top-level-names*)
				       (not (modified-fun? arg))))
			      #t)))
		      (map cons (cdr tmp) (cdr term)))
	      (for-each (lambda (param arg)
			  (if (and param
				   (pair? arg)
				   (eq? 'lambda (car arg))
				   (all-liftable? (cddr arg)))
			      (set-car! arg *liftable-lambda*)))
			(cdr tmp)
			(cdr term))
	      (let ((name
		     (string->symbol
		      (string-append (symbol->string (car term))
				     *export-hof-postfix*))))
		(set! *top-level-names*
		      (cons name *top-level-names*))
		(set-car! term name)))
	  ;; case for map and for-each:
	  (for-each (lambda (arg)
		      (if (and (pair? arg)
			       (eq? 'lambda (car arg)))
			  (set-car! arg *liftable-lambda*)))
		    (cdr term)))
      (for-each (lambda (x) (lift-analyse-def-aux! x)) (cdr term)))
     (else
      (for-each (lambda (x) (lift-analyse-def-aux! x)) term)))))


(define (all-liftable? term)
  (cond ((not (pair? term)) (not (eq? 'lambda term)))
	((eq? 'quote (car term)) #t)
	(else (and (all-liftable? (car term))
		   (all-liftable? (cdr term))))))

(define (guaranteed-all-liftable? term)
  (cond ((not (pair? term)) (not (eq? 'lambda term)))
	((eq? 'quote (car term)) #t)
	((and (or (eq? 'map (car term)) (eq? 'for-each (car term))
		  (eq? *map1-function* (car term))
		  (eq? *for-each1-function* (car term)))
	      (pair? (cdr term))
	      (pair? (cadr term)))
	 (and (guaranteed-all-liftable? (cdadr term))
	      (guaranteed-all-liftable? (cddr term))))
	(else (and (guaranteed-all-liftable? (car term))
		   (guaranteed-all-liftable? (cdr term))))))

(define (lift-unmark-def! term)
  (cond
   ((not (pair? term)))
   ((eq? 'quote (car term)))
   ((eq? *liftable-lambda* (car term))
    (set-car! term 'lambda)
    (for-each lift-unmark-def! (cdr term)))
   ((list? term)
    (for-each lift-unmark-def! term))
   (else term)))


; liftable-nameocc? name term:
;   name a name of some fun,
;   term is the term where the use of name is checked.
;
; gives #f iff name is used in the nonliftable context

(define (liftable-nameocc? name term)
  (cond
   ((not (pair? term)) #t)
   ((eq? 'quote (car term)) #t)
   ((eq? 'lambda (car term))
    (not (inside-term? name (cddr term))))
   ((and (memq name (cdr term))
	 (not (liftable-hofname? (car term))))
    #f)
   (else
    (every1 (lambda (x) (liftable-nameocc? name x)) term))))


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;
;           checking liftability of higher-order funs
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define *liftable-hof-primitives*
  (list *map1-function* *for-each1-function* 'map 'for-each))

(define (liftable-hofname? name)
  (and (or (memq name *liftable-hof-primitives*)
	   (memq name *liftable-hof-names*))
       (not (modified-fun? name))))


; liftable-hof? lterm name:
;  checks whether lterm with name is a liftable hof


(define (liftable-hof? lterm name)
  (let* ((args (args->list (cadr lterm)))
         (hof-args  (filter-hof-args lterm args)))
    (if (and (not (null? hof-args))
	     (liftable-nameocc? name (cddr lterm))
	     (not (member-if (lambda (x)
			       (not (liftable-hofvars-usage?
				     x name hof-args args)))
			     (cddr lterm))))
	(begin
	  (set! *liftable-hof-database*
		(cons (cons name (map (lambda (x) (if (memq x hof-args) #t #f))
				      args))
		      *liftable-hof-database*))
	  #t)
	(if (not (null? hof-args))
	    (begin (set! *non-liftable-hof-names*
			 (cons name *non-liftable-hof-names*))
		   #f)
	    #f))))


; filter-hof-args term args:
;   filters out the functional args from args


(define *found-hof-args* '())
(define *check-hof-args* '())

(define (filter-hof-args term args)
  (if (null? args)
      '()
      (begin
        (set! *check-hof-args* args)
        (set! *found-hof-args* '())
        (filter-hof-args-aux! term)
        *found-hof-args*)))


(define (filter-hof-args-aux! term)
  (let* ((tmp '()))
    (cond
     ((null? *check-hof-args*))
     ((not (pair? term)))
     ((eq? 'quote (car term)))
     ((eq? 'lambda (car term))
      (for-each filter-hof-args-aux! (cddr term)))
     ((begin (set! tmp (memq (car term) *check-hof-args*))
	     tmp)
      (set! *found-hof-args* (cons (car tmp) *found-hof-args*))
      (set! *check-hof-args* (remove-one (car tmp) *check-hof-args*))
      (for-each filter-hof-args-aux! term))
     (else
      (for-each filter-hof-args-aux! term)))))

; liftable-hofvars-usage? term name hof-args:
;   checks that hof-args are used in the term with name only
;   in the function position or as same args to name itself and
;   that the name is not called with lambdaterms at hof-places
;   and that hof-places are exactly the same args.
;   hof-args may also not occur in the inside lambda-terms.

(define (liftable-hofvars-usage? term name hof-args args)
  (cond
   ((not (pair? term)) #t)
   ((eq? 'quote (car term)) #t)
   ((or (eq? 'lambda (car term)) (eq? *liftable-lambda* (car term)))
    (not (find-if (lambda (x) (some-inside-term? hof-args x)) (cddr term))))
   ((eq? name (car term))
    (every1 (lambda (x)
	      (let ((param (car x))
		    (arg (cdr x)))
		(if (memq param hof-args)
		    (eq? param arg)
		    (and (not (memq arg hof-args))
			 (not (and (pair? arg) (eq? 'lambda (car arg))))))))
	    (map cons args (cdr term))))
   (else
    (and (every1 (lambda (el) (not (memq el hof-args))) (cdr term))
	 (every1 (lambda (x) (liftable-hofvars-usage? x name hof-args args))
		 term)))))


; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
;
;           checking for redefining of functions
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -


(define *keywords*
  '(=> and begin case cond define delay do else if lambda
       let let letrec or quasiquote quote set! unquote
       unquote-splicing))

(define (modified-fun? name)
  (cond
   (*all-funs-modified-flag*
    (or (memq name *primitives*)
	(memq name *top-level-names*)))
   (*new-funs-modified-flag*
    (or (memq name *modified-primitives*)
	(memq name *top-level-names*)))
   (else
    (or (memq name *modified-primitives*)
	(memq name *modified-top-level-names*)))))

(define *top-level-names* '())
(define *modified-primitives* '())
(define *modified-top-level-names* '())

(define (make-top-level-namelist! expr-list)
  (set! *top-level-names* '())
  (set! *modified-primitives* '())
  (set! *modified-top-level-names* '())
  (set! *check-redefining-passed* '())
  (make-top-level-namelist-aux! expr-list #f)
;;;  (pretty-print expr-list)
  (make-top-level-namelist-aux! expr-list #t)
;;;  (pretty-print expr-list)
  (set! *modified-primitives*
	(set-difference *modified-primitives* *general-transcedentals*))
  *top-level-names*)

(define *general-transcedentals*
  '(sqrt log expt exp sin cos tan asin acos atan sinh cosh tanh asinh tanh
	 asinh acosh atanh))

(define (make-top-level-namelist-aux! expr-list redefining-flag)
  (if (and (pair? expr-list) (list? expr-list))
      (for-each
       (lambda (expr)
	 (cond
	  ((not (pair? expr)))
	  ((not (list? expr)))
	  ((eq? 'quote (car expr)))
	  ((eq? (car expr) 'define)
	   (let ((new (normalize-top-define expr)))
	     (if redefining-flag
		 (check-redefining! new #t)
		 (set! *top-level-names*
		       (cons (cadr new) *top-level-names*)))))
	  ((and (eq? 'set! (car expr))
		redefining-flag)
	   (check-redefining! expr #t))
	  ((not redefining-flag)
	   (for-each (lambda (x) (make-top-level-namelist-aux! x #f))
		     expr))))
       expr-list)))

(define (compute-floats-flag! term opsflag)
  (cond (*floats-flag* #t)
        ((pair? term)
	 (if (eq? 'quote (car term))
	     (compute-floats-flag! (cdr term) #f)
	     (or (compute-floats-flag! (car term) opsflag)
		 (compute-floats-flag! (cdr term) opsflag))))
        ((vector? term)
	 (do ((i (- (vector-length term) 1) (- i 1)))
	     ((< i 0) *floats-flag*)
	   (compute-floats-flag! (vector-ref term i) opsflag)))
        ((number? term)
	 (if (or (not (integer? term))
		 (not (exact? term))
		 (> term most-positive-fixnum)
		 (< term most-negative-fixnum))
	     (begin (set! *floats-flag* term) #t)
	     #f))
        ((not opsflag) #f)
        ((symbol? term)
	 (if (memq term *float-recognize-ops*)
	     (begin (set! *floats-flag* term) #t)
	     #f))
        (else #f)))

(define *float-recognize-ops*
  '(ln sqrt log exp
       sin cos tan asin acos atan
       sinh cosh tanh asinh acosh atanh
       real-sin real-cos real-tan
       real-asin real-acos real-atan
       real-sinh real-cosh real-tanh
       real-asinh real-acosh real-atanh
       real-sqrt real-expt real-ln real-exp
       $sin $cos $tan $asin $acos $atan
       $sinh $cosh $tanh $asinh $acosh $atanh
       $sqrt $expt $log $abs $exp))

(define *check-redefining-passed* '())

(define (check-redefining! term top-level-flag)
  (let* ((new '()))
    (cond
     ((not (pair? term)))
     ((not (list? term)))
     ((eq? 'quote (car term)))
     ((or (eq? 'set! (car term))
	  (and top-level-flag (eq? 'define (car term))))
      (if (eq? 'define (car term))
	  (set! new (normalize-top-define term))
	  (set! new term))
      (if (not (eq? 3 (length term)))
	  (report-error " wrong set! or define syntax: " term))
      (if (memq (cadr new) *keywords*)
	  (report-error " a keyword is set! or defined: " term))
      (if (memq (cadr new) *primitives*)
	  (or (memq (cadr new) *modified-primitives*)
	      (set! *modified-primitives*
		    (cons (cadr term) *modified-primitives*))))
      (if (and (memq (cadr new) *top-level-names*)
	       (not (memq (cadr new) *hobbit-declaration-vars*)))
	  (or (memq (cadr new) *modified-top-level-names*)
	      (if (and (eq? 'define (car new))
		       (not (memq (cadr new) *check-redefining-passed*)))
		  (set! *check-redefining-passed*
			(cons (cadr new) *check-redefining-passed*))
		  (set! *modified-top-level-names*
			(cons (cadr new) *modified-top-level-names*)))))
      (check-redefining! (caddr new) #f))
     ((eq? 'begin (car term))
      (for-each (lambda (x) (check-redefining! x #t)) term))
     (else
      (for-each (lambda (x) (check-redefining! x #f)) term)))))


;=====================================================================
;
;                      Building closures
;
;====================================================================

;(define *closure-name-suffix* "_cl")
;(define *closure-name-nr* 0)
;(define *closure-vector-name* "clargsv_")
;(define *closure-vector-name-nr* 0)
;(define *closurefun-arg* 'closurearg_0)
;(define *closurefun-arg-car* 'closurearg_car_0)

; The whole closurebuilding process is carried on top-down breadth-first:
; there is no excplicit recursion. Instead, once a new closurefun def
; is created, it is put into the list *lifted-closures-to-do*, which
; is afterwards passed and the lambdaterms inside these new funs are
; made into closures again, etc, until *lifted-closures-to-do* is empty.

; try-closure-making-def is the topmost closure-builder applied to a def.

(define (try-closure-making-def def)
  (let* ((body (caddr def)))
    (set! *letrec-closure-nr* 0)
    (cond ((not (pair? body)) def)
	  ((eq? 'quote (car body)) def)
	  ((eq? 'lambda (car body)) (try-closure-making-ldef def))
	  (else  (report-error "try-closure-making-non-ldef called")))))

; try-closure-making-ldef builds closures for lambdaterm-defs.
; It is never called from anywhere except try-closure-making-def
; (the topmost closurebuilder)

(define (try-closure-making-ldef def)
  (let* ((lterm (caddr def))
         (lvars (args->list (cadr lterm)))
	 (letvars (collect-local-vars (cddr lterm)))
	 (vars (union lvars letvars))
	 ;; closurevars is the subset of set! <vars> inside lambdas:
	 (closurevars (closure-building-vars (cddr lterm) vars))
	 (vectname (make-closure-vector-name)))
    (set! *current-fun-name* (cadr def))
    (if (null? closurevars)
	;; no set! closurevars found:
	(list (car def) (cadr def)
	      (list* (car lterm) (cadr lterm)
		     (map (lambda (x)
			    (cdr (make-closure-making
				  x vars closurevars
				  *closurefun-arg-car* vectname)))
			  (cddr lterm))))
	;; in the next case some closurevars were found.
	(let* ((tmp (make-closure-making
		     (cddr lterm) vars closurevars
		     *closurefun-arg-car* vectname))
	       (varsmapping (car tmp))
	       (newterm (cdr tmp))
	       (initialize-argsv
		(make-initialize-closureargsv vectname lvars varsmapping)))
	  (if (not (null? varsmapping))
	      (beautify-closure
	       (list
		(car def)
		(cadr def)
		(cons (car lterm)
		      (list (cadr lterm)
			    (cons 'let*
				  (cons (cons (list vectname
						    (list 'make-vector
							  (length closurevars)))
					      '())
					(append initialize-argsv
						newterm)))))))
	      (beautify-closure
	       (list
		(car def)
		(cadr def)
		(cons (car lterm)
		      (list (cadr lterm)
			    (append initialize-argsv newterm))))))))))


; make-closure-vector-name builds a new vector for these local vars
; which are passed to (and set! inside) closures.
; default: clargsv_
;
; It is called from try-closure-making-ldef, ..-non-ldef, ...-lterm.
;
; The created vector-name is added to *closure-var-vectornames*
; for later recognition as such.

(define (make-closure-vector-name)
  (set! *closure-vector-name-nr* (+ 1 *closure-vector-name-nr*))
  (let ((res (string->symbol
	      (string-append *closure-vector-name*
			     (number->string *closure-vector-name-nr*)))))
    (if (not (memq res *closure-var-vectornames*))
	(set! *closure-var-vectornames* (cons res *closure-var-vectornames*)))
    res))

; make-closure-name adds a suffix (default: _cl (+nr)) to the
; argument functionname. The returned name will be used as a name
; of the created closurefunction.
;
; called from: make-closure-making-aux and make-trivial-closuremaking

(define (make-closure-name currentfunname)
  (set! *closure-name-nr* (+ 1 *closure-name-nr*))
  (string->symbol (string-append (symbol->string currentfunname)
				 *closure-name-suffix*
				 (number->string *closure-name-nr*))))

; make-initialize-closureargsv takes a vectorname
; (made by make-closure-vector for keeping local vars to be passed),
; lvars (argument vars of a lambdaterm) and varsmapping
; (mapping of local vars to be kept in vector 'vectorname' to the
; elements of this vector)
;
; It adds vector-set! to each element of varsmapping and filters
; out (keeps) exactly these which are in lvars. The resulting
; sequence of assigments ... (set! (vector-ref clargsv_nrn nrx) x)
; is inserted into function body after creating vector 'vectorname'
; in order to use the vector-elements instead of the parametric vars
; of the lambdaterm.
;
; called from: try-closure-making-ldef, ..-non-ldef, ...-lterm.

(define (make-initialize-closureargsv vectname lvars varsmapping)
  (filter-map (lambda (x)
		(if (memq (car x) lvars)
		    (list 'vector-set! vectname
			  (cdr x) (car x))
		    #f))
	      varsmapping))

; make-trivial-closuremaking is called in case the argument term
; contains no mutable vars in the environment, ie when it does not have
; to be a proper closure at all, but just a function without a
; local environment. It returns just the name of the function, to
; be inserted into the surrounding procedure at the place of the
; original lambdaterm.
;
; called from: make-closuremaking-aux and try-closure-making-ldef, ...-lterm.

(define (make-trivial-closuremaking term)
  (cond
   ((not (pair? term)) term)
   ((eq? 'quote (car term)) term)
   ((eq? 'lambda (car term))
    (let* ((fun-name (make-closure-name *current-fun-name*))
	   (procname (make-closure-scmobj-name fun-name))
	   (newdef (list 'define fun-name term)))
      (set! *lifted-trivial-closure-names*
	    (cons fun-name *lifted-trivial-closure-names*))
      (set! *top-level-funs*
	    (cons fun-name *top-level-funs*))
      (if (not (memq procname *special-c-vars*))
	  (set! *special-c-vars* (cons procname *special-c-vars*)))
      (set! *lifted-closures-to-do*
	    (cons newdef *lifted-closures-to-do*))
      procname))
   ((not (list? term)) term)
   (else
    (map (lambda (x) (make-trivial-closuremaking x)) term))))

; - - - - - - - - - proper closure-body-building begins - - - - - - - -


; make-closure-making creates the correct body of the closure (inside
; non-liftable lambdaterm which is used together with the vector
; of its environment) together with the creation/instantiation code
; inserted into the surrounding fun at the place of the original lambdaterm.
;
; vars is the set of environment vars, closurevars is the set of
; set! environment vars.
; called from: try-closure-making-ldef, ..-non-ldef, ...-lterm

(define (make-closure-making term vars closurevars vectname clvectname)
  (let* ((varsnr (length closurevars))
	 (tmp -1)
	 (clvarsmapping (map (lambda (x)
			       (set! tmp (+ 1 tmp)) (cons x tmp))
			     closurevars))
	 (newterm '()))
    (set! newterm (vars->closureaccess
		   term '() clvarsmapping vectname clvectname))
    (begin (set! newterm
		 (make-closure-making-aux
		  (cdr newterm) '() vars (map car clvarsmapping)
		  vectname clvectname))
	   (cons clvarsmapping newterm))))


(define (make-closure-making-aux
	 term holes vars clvars vectname clvectname)
  (cond
   ((not (pair? term)) term)
   ((eq? 'quote (car term)) term)
   ((or (eq? 'lambda (car term)) (eq? *liftable-lambda* (car term)))
    (make-closure-making-aux-lterm
     term holes vars clvars vectname clvectname))
   ((not (list? term)) term)
   ((and (eq? 'set! (car term))
	 (pair? (cdr term))
	 (pair? (cddr term))
	 (pair? (caddr term))
	 ;; if it is not lambda, and is inside-term,
	 ;; then the set! var must be in clvars.
	 (eq? 'lambda (caaddr term))
	 (not (memq (cadr term) clvars))
	 (inside-term? (cadr term) (caddr term)))
    (make-closure-making-aux-set!
     term holes vars clvars vectname clvectname))
   ((and (eq? 'letrec (car term))
	 (pair? (cdr term))
	 ;; if some bound var is inside-nonliftable-term,
	 ;; and some leading fun is not a (nonliftable)lambda,
	 ;; then all the bound vars must be in clvars.
	 (every1 (lambda (el) (eq? 'lambda (caadr el)))
		 (cadr term))
	 (find-if (lambda (el) (not (memq (car el) clvars)))
		  (cadr term)))
    (make-closure-making-aux-letrec
     term holes vars clvars vectname clvectname))
   (else
    (map (lambda (x)
	   (make-closure-making-aux
	    x holes vars clvars vectname clvectname))
	 term))))

(define (make-closure-making-aux-set!
	 term holes vars clvars vectname clvectname)
  (let* ((tmp '())
	 (newholes (cons (cadr term) holes)))
    (set! *letrec-closures* '())
    (set! *letrec-closure-init* '())
    (set! tmp
	  (make-closure-making-aux
	   (caddr term) newholes vars clvars vectname clvectname))
    (list* 'let* *letrec-closures*
	   (append (list (list 'set! (cadr term) tmp))
		   *letrec-closure-init*))))


(define (make-closure-making-aux-letrec
	 letterm holes vars clvars vectname clvectname)
  (let* ((bindings (cadr letterm))
	 (body (cddr letterm))
	 (newbindings '())
	 (newholes (append (map car bindings) holes)))
    (set! *letrec-closures* '())
    (set! *letrec-closure-init* '())
    (set! newbindings
	  (map (lambda (el)
		 (list (car el)
		       (make-closure-making-aux
			(cadr el) newholes vars clvars vectname clvectname)))
	       bindings))
    (list* 'let* (append *letrec-closures* newbindings)
	   (append
	    *letrec-closure-init*
	    (map (lambda (x)
		   (make-closure-making-aux
		    x holes vars clvars vectname clvectname))
		 body)))))

; make-closure-making-aux-lterm creates the correct body of the closure (inside
; non-liftable lambdaterm which is used together with the vector
; of its environment) together with the creation/instantiation code
; inserted into the surrounding fun at the place of the original lambdaterm.
;
; vars is the set of environment vars, closurevars is the set of
; set! environment vars.
; called from: try-closure-making-ldef, ..-non-ldef, ...-lterm and
; recursively from make-closure-making-aux

(define (make-closure-making-aux-lterm
	 lterm holes vars clvars vectname clvectname)
  (let ((params (args->list (cadr lterm))))
    ;; filter out the subsets of vars actually occurring in lterm,
    ;; previously throwing away these which are bound in lambda-args.
    (or (null? vars)
	(set! vars
	      (filter-inside-term
	       (filter (lambda (x) (not (memq x params)))
		       vars)
	       (cddr lterm))))
    (cond
     ((eq? *liftable-lambda* (car lterm))
      (list* (car lterm) (cadr lterm)
	     (map (lambda (x)
		    (make-closure-making-aux
		     x holes vars clvars vectname clvectname))
		  (cddr lterm))))
     ((and (null? vars)
	   (not (some-inside-term? *closure-var-vectornames* (cddr lterm))))
      ;; trivial case: no closure has to be built, function suffices
      (make-trivial-closuremaking lterm))
     (else
      ;; nontrivial case: closure has to be built, but there are no
      ;; set! closurevars to be handled.
      (let* ((fun-name (make-closure-name *current-fun-name*))
	     (definf (make-lifted-closure-fun
		      lterm fun-name vars clvars))
	     (applic (make-lifted-closure-applic definf holes)))
	(set! *lifted-closure-names*
	      (cons fun-name *lifted-closure-names*))
	(set! *lifted-closures-to-do*
	      (cons (caddr definf) *lifted-closures-to-do*))
	applic)))))

; make-lifted-closure-fun builds a body of the lambdaterm which is
; used as a proper closure.
;
; vars is the (nonempty) list of free variables occurring in lterm
;
; make-lifted-closure-fun is called only from make-closure-making-aux.


(define (make-lifted-closure-fun lterm name vars clvars)
  (let* ((args (cdr (sort-out-clargs (cadr lterm))))
	 (passed-clargsv-lst
	  (filter-inside-term *closure-var-vectornames* (cddr lterm)))
	 (clargstranslation
	  (make-wrapped-clargs-init
	   passed-clargsv-lst *closurefun-arg-car* 1))
	 (varstranslation
	  (make-wrapped-clargs-init
	   vars *closurefun-arg-car* (+ 1 (length passed-clargsv-lst))))
	 (argstranslation
	  (make-wrapped-args-init
	   args *closurefun-arg* 1)))
    (list
     vars
     passed-clargsv-lst
     (list 'define
	   name
	   (list 'lambda
		 (list *closurefun-arg*)
		 (cons 'let*
		       (cons (append
			      (list (list *closurefun-arg-car*
					  (list 'car  *closurefun-arg*)))
			      clargstranslation
			      varstranslation
			      argstranslation)
			     (cddr lterm))))))))


; sort-out-clargs takes the parameters of the function to the used as
; a closure-body. It splits these into the pair of two lists,
; the car being all these parameters which are closure-var-vectornames
; and the cdr being these parameters which are not.

; called only from make-lifted-closure-fun.

(define (sort-out-clargs inargs)
  (let* ((clargs '())
	 (args '()))
    (do ((part inargs (cdr part)))
	((not (pair? part))
	 (cons (reverse clargs)
	       (append (reverse args) part)))
      (if (memq (car part) *closure-var-vectornames*)
	  (set! clargs (cons (car part) clargs))
	  (set! args (cons (car part) args))))))

; make-wrapped-clargs-init takes a list of vars which are
; closure-var-vectornames. It creates a let-initialization-list
; of the form ((<listel> (vector-ref <closurefun-arg-car> 1) ... (..2) ...)

; called only from make-lifted-closure-fun.

(define (make-wrapped-clargs-init clargs varname nr)
  (cond
   ((null? clargs) '())
   (else
    (cons (list (car clargs) (list 'vector-ref varname nr))
	  (make-wrapped-clargs-init (cdr clargs) varname (+ 1 nr))))))


; make-wrapped-args-init takes a list of vars which are _not_
; closure-var-vectornames. It creates a let-initialization-list
; of the form ((<listel> (begin (set! <closurefun-arg> (cdr closurefun-arg>))
;                               (car <closurefun-arg>))).
;
; called only from make-lifted-closure-fun.

(define (make-wrapped-args-init args varname nr)
  (cond
   ((null? args) '())
   ((not (pair? args))
    (list (list args (list 'cdr varname))))
   ((zero? nr)
    (cons (list (car args)
		(list 'car varname))
	  (make-wrapped-args-init (cdr args) varname (+ 1 nr))))
   (else
    (cons (list (car args)
		(list 'begin
		      (list 'set! varname (list 'cdr varname))
		      (list 'car varname)))
	  (make-wrapped-args-init (cdr args) varname (+ 1 nr))))))


; make-lifted-closure-applic takes a newly built closurefun body def
; and creates code for creating the closure and initializing the
; environment-vector-part of the closure.
;
; called only from make-closure-making-aux.

(define (make-lifted-closure-applic definf holes)
  (let* ((vars (car definf))
	 (clvects (cadr definf))
	 (newdef (caddr definf))
	 (funname (cadr newdef))
	 (procname (make-closure-scmobj-name funname))
	 (lterm (caddr newdef))
	 (lbody (cddr lterm))
	 (assignments '())
	 (nr 0)
	 (closurename (string->symbol *new-closure-var*))
	 (letrec-assignments '()))
    (if (not (null? holes))
	(begin (set! *letrec-closure-nr* (+ 1 *letrec-closure-nr*))
	       (set! closurename
		     (string->symbol
		      (string-append
		       *new-closure-var*
		       (string-append
			"_" (number->string *letrec-closure-nr*)))))))
    (for-each (lambda (x)
		(set! nr (+ 1 nr))
		(set! assignments
		      (cons (list 'vector-set! closurename nr x)
			    assignments)))
	      clvects)
    (for-each (lambda (x)
		(set! nr (+ 1 nr))
		(set! assignments
		      (cons (list 'vector-set! closurename nr x)
			    assignments)))
	      vars)
    (cond ((null? holes)		; closure does not occur in letrec top
	   (if (null? assignments)
	       (list *make-cclo* procname (list *actual-c-int* (+ 1 nr)))
	       `(let* ((,closurename
			(,*make-cclo*
			 ,procname ,(list *actual-c-int* (+ 1 nr)))))
		  ,@(reverse assignments)
		  ,closurename)))
	  (else
	   (set! letrec-assignments
		 (filter (lambda (x) (member (cadddr x) holes))
			 assignments))
	   (set! assignments
		 (filter (lambda (x) (not (member x letrec-assignments)))
			 assignments))
	   (set! *letrec-closures*
		 (append *letrec-closures*
			 (list (list closurename
				     (list *make-cclo* procname
					   (list *actual-c-int* (+ 1 nr)))))))
					; closure occurs in letrec top
	   (set! *letrec-closure-init*
		 (append *letrec-closure-init*
			 (reverse letrec-assignments)))
	   (if (null? assignments)
	       closurename
	       (cons 'begin (append assignments (list closurename))))))))


;;  - - - - - - - - - proper closure-body-building ends - - - - - - - -


(define (make-closure-scmobj-name funname)
  (let ((res (string->symbol
	      (string-append
	       (symbol->string funname) *closure-proc-suffix*))))
    (or (memq res *special-c-vars*)
	(set! *special-c-vars* (cons res *special-c-vars*)))
    res))

(define (list->conses lst)
  (if (null? lst)
      (list 'quote '())
      (let ((tmp (list->conses (cdr lst))))
	(list 'cons (car lst) tmp))))

(define (cl-vectorname? symb)
  (memq symb *closure-var-vectornames*))

;vars->closureaccess takes a term and two mappings of vars to closureaccess.
; a mapping has the format: (<var> . <nr>)
;
; it returns a pair (<flag> . <result-term>) where <flag> is #f iff
; the term does not contain closurevars.
;
; it assumes that vars in let-s, lambda, do have been already renamed
; so that there are no varname-clashes.
;
; called only from make-closure-making and recursively.

(define (vars->closureaccess term varsmap clvarsmap vectname clvectname)
  (cond
   ((symbol? term)
    (set! clvarsmap (assq term clvarsmap))
    (set! varsmap (assq term varsmap))
    (cond
     ((and clvarsmap
	   (not (memq term *closure-var-vectornames*)))
      (cons #t (list 'vector-ref clvectname (cdr clvarsmap))))
     ((and varsmap
	   (not (memq term *closure-var-vectornames*)))
      (cons #t (list 'vector-ref vectname (cdr varsmap))))
     (else
      (cons #f term))))
   ((not (pair? term)) (cons #f term))
   ((eq? 'quote (car term)) (cons #f term))
   ((eq? *liftable-lambda* (car term))
    (let* ((vars (args->list (cadr term)))
	   (newmap (filter (lambda (x) (not (memq (car x) vars))) varsmap))
	   (newclmap (filter (lambda (x) (not (memq (car x) vars))) clvarsmap))
	   (tmp (vars->closureaccess
		 (cddr term) newmap newclmap vectname clvectname)))
      (cons (car tmp) (list* *liftable-lambda* (cadr term) (cdr tmp)))))
   ((eq? 'lambda (car term))
    (let* ((vars (args->list (cadr term)))
	   (newmap (filter (lambda (x) (not (memq (car x) vars))) varsmap))
	   (newclmap (filter (lambda (x) (not (memq (car x) vars))) clvarsmap))
	   (tmp (vars->closureaccess
		 (cddr term) newmap newclmap vectname clvectname)))
      (if (car tmp)			; closurevars used?
					; yes, closurevars used:
	  (cons #t (cons 'lambda
			 (cons (cons clvectname (cadr term))
			       (cdr tmp))))
					; no, no closurevars were used:
	  (cons #f term))))
   (else
    (let ((tmp (map (lambda (x) (vars->closureaccess
				 x varsmap clvarsmap vectname clvectname))
		    term)))
      (if (find-if (lambda (x) (car x)) tmp)
	  (cons #t (map cdr tmp))
	  (cons #f (map cdr tmp)))))))


; closure-building-vars assumes that vars in the term are renamed
;   so that no varname or varname-funname or varname-syntax
;   conflicts occur.
; it returns the subset of vars in funvars occurring freely and set!
; inside lambdaterms in term, plus funvars fi occurring freely in the
; contexts:
; (1)  (set! fi t), where t=/=(lambda (...)...) and fi occurs
; inside a non-liftable lambdaterm in t.
; (2)  (letrec (... (fi ti) ...) ...), where ti=/=(lambda (...)...) and
; at least one of fj bound in letrec occurs inside a non-liftable
; lambdaterm in a tr body in letrec. NB! If some ti=/=(lambda (...)...),
; the latter condition is automatically guaranteed by previous lifting
; analysis.

(define *closure-building-vars* '())

(define (closure-building-vars term funvars)
  (set! *local-vars* funvars)
  (set! *closure-building-vars* '())
  (closure-building-vars-aux! term)
  (filter (lambda (x) (memq x *closure-building-vars*)) funvars))

(define (closure-building-vars-aux! term)
  (cond
   ((not (pair? term)))
   ((eq? 'quote (car term)))
   ((eq? 'lambda (car term))
    (for-each (lambda (var)
		(if (and (not (memq var *closure-building-vars*))
			 (not (inside-term? var (cadr term)))
			 (inside-term-set? var (cddr term)))
		    (set! *closure-building-vars*
			  (cons var *closure-building-vars*))))
	      *local-vars*))
   ((eq? *liftable-lambda* (car term))
    (for-each closure-building-vars-aux! (cddr term)))
   ((eq? 'set! (car term))
    (if (and (pair? (caddr term))
	     (not (eq? 'lambda (car (caddr term))))
	     (inside-nonliftable-term? (cadr term) (caddr term))
	     (not (memq (cadr term) *closure-building-vars*)))
	(set! *closure-building-vars*
	      (cons (cadr term) *closure-building-vars*)))
    (for-each closure-building-vars-aux! (cdr term)))
   ((eq? 'letrec (car term))
    (if (and (find-if (lambda (x)
			(and (pair? (cadr x))
			     (not (eq? 'lambda (car (cadr x))))))
		      (cadr term))
	     (find-if (lambda (x)
			(find-if (lambda (y)
				   (inside-nonliftable-term? (car x) (cadr y)))
				 (cadr term)))
		      (cadr term)))
	(for-each (lambda (x)
		    (or (memq (car x) *closure-building-vars*)
			(set! *closure-building-vars*
			      (cons (car x) *closure-building-vars*))))
		  (cadr term)))
    (for-each closure-building-vars-aux! (cdr term)))
   (else
    (for-each closure-building-vars-aux! term))))


(define (inside-nonliftable-term? name term)
  (cond
   ((not (pair? term)) #f)
   ((eq? 'quote (car term)) #f)
   ((eq? 'lambda (car term))
    (inside-term? name (cddr term)))
   (else
    (find-if (lambda (x) (inside-nonliftable-term? name x)) term))))

(define (inside-term-set? x term)
  (cond ((not (pair? term)) #f)
	((eq? 'quote (car term)) #f)
	((eq? 'set! (car term))
	 (or (and (pair? (cdr term))
		  (eq? x (cadr term))
		  (pair? (cddr term))
		  (null? (cdddr term)))
	     (inside-term-set? x (cdr term))))
	(else
	 (or (inside-term-set? x (car term))
	     (inside-term-set? x (cdr term))))))

(define (collect-local-vars term)
  (set! *local-vars* '())
  (collect-local-vars-aux term)
  *local-vars*)
(define (collect-local-vars-aux term)
  (cond
   ((not (pair? term)))
   ((eq? (car term) 'quote))
   ((or (eq? (car term) 'let*) (eq? (car term) *op-let*)
        (eq? (car term) 'let) (eq? (car term) 'letrec))
    (set! *local-vars*
	  (union (filter-map
		  (lambda (el)
		    (if (and (pair? (cadr el))
			     (eq? *liftable-lambda* (caadr el)))
			#f
			(car el)))
		  (cadr term))
		 *local-vars*))
    (for-each (lambda (x) (collect-local-vars-aux (cadr x))) (cadr term))
    (for-each (lambda (x) (collect-local-vars-aux x)) (cddr term)))
   ((eq? (car term) 'do)
    (set! *local-vars* (union (map car (cadr term)) *local-vars*))
    (for-each (lambda (x)
		(for-each (lambda (y) (collect-local-vars-aux y)) (cdr x)))
	      (cadr term))
    (for-each (lambda (x) (collect-local-vars-aux x)) (caddr term))
    (for-each (lambda (x) (collect-local-vars-aux x)) (cdddr term)))
   ((eq? (car term) 'lambda))
   (else
    (for-each (lambda (x) (collect-local-vars-aux x)) term))))

; beautify-closure takes a built closure-fun and corrects the
; following: (let* (... ((vector-ref foo n) bar) ...) ...) is
; replaced by (let* (...) (vector-set! foo n bar) (let* (...) ...)),
; (set! (vector-ref foo n) bar) is replaced by (vector-set! foo n bar)
(define (beautify-closure term)
  (cond
   ((not (pair? term)) term)
   ((eq? 'quote (car term)) term)
   ((and (eq? 'set! (car term))
	 (pair? (cdr term))
	 (pair? (cadr term))
	 (eq? 'vector-ref (caadr term))
	 (pair? (cdadr term))
	 (pair? (cddr term))
	 (memq (cadadr term) *closure-var-vectornames*))
    (list 'vector-set!
	  (cadadr term) (caddr (cadr term)) (beautify-closure (caddr term))))
   ((and (memq (car term) '(let* let letrec))
	 (not (null? (cdr term)))
	 (pair? (cadr term))
	 (find-if (lambda (x) (pair? (car x))) (cadr term)))
    (beautify-closure-let (car term) (cadr term) (cddr term)))
   ((list? term)
    (map beautify-closure term))
   (else
    term)))
(define (beautify-closure-let key bindings rest)
  (if (null? bindings)
      (cons 'begin (map beautify-closure rest))
      (let* ((okpart '()))
	(do ((part bindings (cdr part)))
	    ((or (null? part)
		 (and (pair? (car part))
		      (pair? (caar part))
		      (eq? 'vector-ref (caaar part))
		      (pair? (cdaar part))
		      (memq (cadaar part) *closure-var-vectornames*)))
	     (if (null? part)
		 (list* key (reverse okpart) (map beautify-closure rest))
		 (list key
		       (reverse okpart)
		       (list 'vector-set! (cadaar part)
			     (caddar (car part))
			     (beautify-closure (cadar part)))
		       (beautify-closure-let key (cdr part) rest))))
	  (set! okpart (cons (list (caar part)
				   (beautify-closure (cadar part)))
			     okpart))))))



;====================================================================
;
;	       auxiliary functions - a library
;
;===================================================================

(define (filter f lst)
  (cond ((null? lst) '())
	((f (car lst))  (cons (car lst) (filter f (cdr lst))))
	(else (filter f (cdr lst)))))

(define (filter-map f lst)
  (if (pair? lst)
      (let ((res (f (car lst))))
	(if res
	    (cons res (filter-map f (cdr lst)))
	    (filter-map f (cdr lst))))
      '()))

(define (filter-inside-term lst term)
  (define *filter-inside-term-res* '())
  (define (filter-inside-term-aux! lst term)
    (cond ((not (pair? term))
	   (and (memq term lst)
		(not (memq term *filter-inside-term-res*))
		(set! *filter-inside-term-res*
		      (cons term *filter-inside-term-res*))))
	  ((eq? 'quote (car term)))
	  (else (filter-inside-term-aux! lst (car term))
		(filter-inside-term-aux! lst (cdr term)))))
  (filter-inside-term-aux! lst term)
  (filter (lambda (x) (memq x *filter-inside-term-res*)) lst))

(define (inside-term? x term)
  (cond ((eq? x term) #t)
	((not (pair? term)) #f)
	((eq? 'quote (car term)) #f)
	(else (or (inside-term? x (car term))
		  (inside-term? x (cdr term))))))

(define (some-inside-term? obs term)
  (cond ((memq term obs) #t)
	((not (pair? term)) #f)
	((eq? 'quote (car term)) #f)
	(else (or (some-inside-term? obs (car term))
		  (some-inside-term? obs (cdr term))))))

(define (subst-term-equal! what for term)
  (cond ((not (pair? term)))
	((equal? (car term) for)
	 (set-car! term what)
	 (subst-term-equal! what for (cdr term)))
	((not (eq? 'quote (car term)))
	 (subst-term-equal! what for (car term))
	 (subst-term-equal! what for (cdr term)))))

(define (subst-term what for term)
  (cond ((eq? term for) what)
	((not (pair? term)) term)
	((eq? 'quote (car term)) term)
	(else (cons (subst-term what for (car term))
		    (subst-term what for (cdr term))))))

(define (in-fun-position? x term)
  (cond ((or (not (pair? term)) (eq? 'quote (car term))) #f)
	((not (list? term)) #f)
	((eq? x (car term)) #t)
	(else (find-if (lambda (y) (in-fun-position? x y)) term))))

(define (some-in-fun-position? lst term)
  (cond ((or (not (pair? term)) (eq? 'quote (car term))) #f)
	((not (list? term)) #f)
	((memq (car term) lst) #t)
	(else (find-if (lambda (y) (some-in-fun-position? lst y)) term))))

(define (replaceq what with lst)
  (cond ((null? lst) '())
	((eq? what (car lst)) (cons with (replaceq what with (cdr lst))))
	(else (cons (car lst) (replaceq what with (cdr lst))))))

;;; Like LAST-PAIR, but works for non-lists.
(define (my-last-pair lst)
  (define (my-last-pair-aux lst)
    (if (pair? (cdr lst))
	(my-last-pair-aux (cdr lst))
	lst))
  (if (not (pair? lst))
      lst
      (my-last-pair-aux lst)))

;;; Like REMOVE, but removes at most one element.
(define (remove-one what from)
  (cond ((null? from) from)
	((eq? what (car from)) (cdr from))
	(else (cons (car from) (remove-one what (cdr from))))))

;;; Like FIND-IF, but works for non-lists.
(define (pair-find-if f lst)
  (if (pair? lst)
      (if (f (car lst)) (car lst) (pair-find-if f (cdr lst)))
      (if (f lst) lst #f)))

;;; slib/comlist.scm functions:

(define (find-if f lst)
  (if (null? lst)
      #f
      (if (f (car lst)) (car lst) (find-if f (cdr lst)))))

(define (remove what lst)
  (cond ((null? lst) '())
	((eq? what (car lst)) (remove what (cdr lst)))
	(else (cons (car lst) (remove what (cdr lst))))))

(define (every1 f lst)
  (if (null? lst)
      #t
      (if (f (car lst)) (every1 f (cdr lst)) #f)))

(define (member-if f lst)
  (if (null? lst)
      #f
      (if (f (car lst)) lst (member-if f (cdr lst)))))

(define (list* obj1 . obj2)
  (define (list*1 obj)
    (if (null? (cdr obj))
	(car obj)
	(cons (car obj) (list*1 (cdr obj)))))
  (if (null? obj2)
      obj1
      (cons obj1 (list*1 obj2))))

(define (butlast lst n)
  (letrec
      ((len (- (length lst) n))
       (bl (lambda (lst n)
	     (let build-until-zero ((lst lst)
				    (n n)
				    (result '()))
	       (cond ((null? lst) (reverse result))
		     ((positive? n)
		      (build-until-zero
		       (cdr lst) (- n 1) (cons (car lst) result)))
		     (else (reverse result)))))))
    (bl lst (if (negative? n)
		(slib:error "negative argument to butlast" n)
		len))))

(define (union lst1 lst2)
  (define ans (if (null? lst1) lst2 lst1))
  (define (adjoin obj lst) (if (memv obj lst) lst (cons obj lst)))
  (cond ((null? lst2) lst1)
	(else (for-each (lambda (elt) (set! ans (adjoin elt ans)))
			lst2)
	      ans)))

(define (set-difference lst1 lst2)
  (if (null? lst2)
      lst1
      (let build-difference ((lst1 lst1)
			     (result '()))
	(cond ((null? lst1) (reverse result))
	      ((memv (car lst1) lst2) (build-difference (cdr lst1) result))
	      (else (build-difference (cdr lst1) (cons (car lst1) result)))))))

(define (intersection lst1 lst2)
  (if (null? lst2)
      lst2
      (let build-intersection ((lst1 lst1)
			       (result '()))
	(cond ((null? lst1) (reverse result))
	      ((memv (car lst1) lst2)
	       (build-intersection (cdr lst1) (cons (car lst1) result)))
	      (else
	       (build-intersection (cdr lst1) result))))))

;===========================  END ===============================
