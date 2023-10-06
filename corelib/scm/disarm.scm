;;;; "disarm.scm", Make SCM safe for client-server applications.
;; Copyright (C) 1998 Free Software Foundation, Inc.
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

;;; Author: Aubrey Jaffer.

(define (disarm name)
  (lambda args
    ;;(if (memq? name slib:features) (set! slib:features (remove name *features)))
    (error name 'disabled)))

(define abort		quit)
(define restart		(disarm 'restart))
(define ed		(disarm 'ed))
#+vms
(define vms-debug		(disarm 'vms-debug))

;; opening files
(define open-file	(disarm 'open-file))
(define transcript-on	(disarm 'transcript-on))

#+i/o-extensions
(begin
  (define system		(disarm 'system))
  (define execvp		(disarm 'exec))
  (define execv		execvp)
  (define execlp		execvp)
  (define execl		execvp)
  (define putenv		(disarm 'putenv))
  (define stat		(disarm 'stat))
  (define reopen-file	(disarm 'reopen-file))
  (define duplicate-port	(disarm 'duplicate-port))
  (define redirect-port!	(disarm 'redirect-port!))
  (define opendir		(disarm 'opendir))
  (define mkdir		(disarm 'mkdir))
  (define rmdir		(disarm 'rmdir))
  (define chdir		(disarm 'chdir))
  (define rename-file	(disarm 'rename-file))
  (define chmod		(disarm 'chmod))
  (define utime		(disarm 'utime))
  (define umask		(disarm 'umask))
  (define fileno		(disarm 'fileno))
  (define access		(disarm 'access))
  )
#+posix
(begin
  (define open-pipe	(disarm 'open-pipe))
  (define fork		(disarm 'fork))
  (define setuid		(disarm 'setuid))
  (define setgid		(disarm 'setgid))
  (define seteuid		(disarm 'seteuid))
  (define setegid		(disarm 'setegid))
  (define kill		(disarm 'kill))
  (define waitpid		(disarm 'waitpid))
  (define uname		(disarm 'uname))
  (define getpw		(disarm 'getpw))
  (define getgr		(disarm 'getgr))
  (define getgroups	(disarm 'getgroups))
  (define link		(disarm 'link))
  (define chown		(disarm 'chown))
  )
;;#+unix
;;(begin
;;  (define symlink		(disarm 'symlink))
;;  (define readlink	(disarm 'readlink))
;;  (define lstat		(disarm 'lstat))
;;  (define nice		(disarm 'nice))
;;  (define acct		(disarm 'acct))
;;  (define mknod		(disarm 'mknod))
;;  )

#+edit-line
(error 'edit-line 'inappropriate-for-server)
#+curses
(error 'curses 'inappropriate-for-server)
#+turtle-graphics
(error 'turtle-graphics 'inappropriate-for-server)

;;#+socket
;;(begin
;;  (define make-stream-socket	(disarm 'make-stream-socket))
;;  (define make-stream-socketpair	(disarm 'make-stream-socketpair))
;;  (define socket:connect	(disarm 'socket:connect))
;;  (define socket:bind	(disarm 'socket:bind))
;;  (define socket:listen	(disarm 'socket:listen))
;;  (define socket:accept	(disarm 'socket:accept))
;;  )

;; load
(define load		(disarm 'load))
(define try-load		load)
(define scm:load		load)
(define scm:load-source		load)
(define link:link		(disarm 'link:link))

;; SLIB loads
(define base:load		load)
(define slib:load		load)
(define slib:load-compiled	load)
(define slib:load-source	load)
(define defmacro:load	load)
(define macro:load	load)
;;(define macwork:load	load)
;;(define syncase:load	load)
;;(define synclo:load	load)

;;;; eval
;;(define eval		(disarm 'eval))
;;(define eval-string	eval)
;;(define interaction-environment	(disarm 'interaction-environment))
;;(define scheme-report-environment	(disarm 'scheme-report-environment))

;;;; SLIB evals
;;(define base:eval		eval)
;;(define slib:eval		eval)
;;(define defmacro:eval	eval)
;;(define macro:eval	eval)
;;(define macwork:eval	eval)
;;(define repl:eval		eval)
;;(define syncase:eval	eval)
;;(define syncase:eval-hook	eval)
;;(define synclo:eval	eval)
