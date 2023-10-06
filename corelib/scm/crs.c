/* "crs.c" interface to `curses' interactive terminal control library.
 * Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.
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

/* Author: Aubrey Jaffer */

#include "scm.h"

#ifdef __FreeBSD__
# include <ncurses.h>
#else
# ifdef __NetBSD__
#  include <ncurses.h>
# else
#  include <curses.h>
# endif
#endif

#ifdef MWC
# include <unctrl.h>
#endif

#ifndef STDC_HEADERS
	int wrefresh P((WINDOW *));
	int wgetch P((WINDOW *));
#endif

/* define WIN port type */
#define WIN(obj) ((WINDOW*)CDR(obj))
#define WINP(obj) (tc16_window==TYP16(obj))
int freewindow(win)
     WINDOW *win;
{
  if (win==stdscr) return 0;
  delwin(win);
  return 0;
}
int bwaddch(c, win) int c; WINDOW *win; {waddch(win, c);return c;}
int bwaddstr(str, win) char *str; WINDOW *win; {waddstr(win, str);return 0;}
sizet bwwrite(str, siz, num, win)
     sizet siz, num;
     char *str; WINDOW *win;
{
  sizet i = 0, prod = siz*num;
  for (;i < prod;i++) waddch(win, str[i]);
  return num;
}
int tc16_window;
static ptobfuns winptob = {
  "window",
  mark0,
  freewindow,
  0,
  equal0,
  bwaddch,
  bwaddstr,
  bwwrite,
  wrefresh,
  wgetch,
  freewindow};

SCM mkwindow(win)
     WINDOW *win;
{
  SCM z;
  if (NULL==win) return BOOL_F;
  DEFER_INTS;
  z = scm_port_entry((FILE *)win, tc16_window, OPN | RDNG | WRTNG);
  ALLOW_INTS;
  return z;
}

SCM *loc_stdscr = 0;
SCM linitscr()
{
  WINDOW *win;
  if (NIMP(*loc_stdscr)) {
    refresh();
    return *loc_stdscr;
  }
  win = initscr();
  return *loc_stdscr = mkwindow(win);
}
SCM lendwin()
{
  if (IMP(*loc_stdscr)) return BOOL_F;
  return ERR==endwin() ? BOOL_F : BOOL_T;
}

static char s_newwin[] = "newwin", s_subwin[] = "subwin", s_mvwin[] = "mvwin",
	    s_overlay[] = "overlay", s_overwrite[] = "overwrite";
SCM lnewwin(lines, cols, args)
     SCM lines, cols, args;
{
  SCM begin_y, begin_x;
  WINDOW *win;
  ASRTER(INUMP(lines), lines, ARG1, s_newwin);
  ASRTER(INUMP(cols), cols, ARG2, s_newwin);
  ASRTER(2==ilength(args), args, WNA, s_newwin);
  begin_y = CAR(args);
  begin_x = CAR(CDR(args));
  ASRTER(INUMP(begin_y), begin_y, ARG3, s_newwin);
  ASRTER(INUMP(begin_x), begin_y, ARG4, s_newwin);
  win = newwin(INUM(lines), INUM(cols),
	       INUM(begin_y), INUM(begin_x));
  return mkwindow(win);
}

SCM lmvwin(win, y, x)
     SCM win, y, x;
{
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_mvwin);
  ASRTER(INUMP(x), x, ARG2, s_mvwin);
  ASRTER(INUMP(y), y, ARG3, s_mvwin);
  return ERR==mvwin(WIN(win), INUM(y), INUM(x)) ? BOOL_F : BOOL_T;
}

SCM lsubwin(win, lines, args)
     SCM win, lines, args;
{
  SCM cols, begin_y, begin_x;
  WINDOW *nwin;
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_subwin);
  ASRTER(INUMP(lines), lines, ARG2, s_subwin);
  ASRTER(3==ilength(args), args, WNA, s_subwin);
  cols = CAR(args);
  args = CDR(args);
  begin_y = CAR(args);
  begin_x = CAR(CDR(args));
  ASRTER(INUMP(cols), cols, ARG3, s_subwin);
  ASRTER(INUMP(begin_y), begin_y, ARG3, s_subwin);
  ASRTER(INUMP(begin_x), begin_y, ARG4, s_subwin);
  nwin = subwin(WIN(win), INUM(lines), INUM(cols),
		INUM(begin_y), INUM(begin_x));
  return mkwindow(nwin);
}

SCM loverlay(srcwin, dstwin)
     SCM srcwin, dstwin;
{
  ASRTER(NIMP(srcwin) && WINP(srcwin), srcwin, ARG1, s_overlay);
  ASRTER(NIMP(dstwin) && WINP(dstwin), dstwin, ARG2, s_overlay);
  return ERR==overlay(WIN(srcwin), WIN(dstwin)) ? BOOL_F : BOOL_T;
}

SCM loverwrite(srcwin, dstwin)
     SCM srcwin, dstwin;
{
  ASRTER(NIMP(srcwin) && WINP(srcwin), srcwin, ARG1, s_overwrite);
  ASRTER(NIMP(dstwin) && WINP(dstwin), dstwin, ARG2, s_overwrite);
  return ERR==overwrite(WIN(srcwin), WIN(dstwin)) ? BOOL_F : BOOL_T;
}

static char s_wmove[] = "wmove", s_wadd[] = "wadd", s_winsch[] = "winsch",
	s_box[] = "box";
SCM lwmove(win, y, x)
     SCM win, y, x;
{
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_wmove);
  ASRTER(INUMP(x), x, ARG2, s_wmove);
  ASRTER(INUMP(y), y, ARG3, s_wmove);
  return ERR==wmove(WIN(win), INUM(y), INUM(x)) ? BOOL_F : BOOL_T;
}

SCM lwadd(win, obj)
     SCM win, obj;
{
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_wadd);
  if (ICHRP(obj))
    return ERR==waddch(WIN(win), ICHR(obj)) ? BOOL_F : BOOL_T;
  if (INUMP(obj))
    return ERR==waddch(WIN(win), INUM(obj)) ? BOOL_F : BOOL_T;
  ASRTER(NIMP(obj) && STRINGP(obj), obj, ARG2, s_wadd);
  return ERR==waddstr(WIN(win), CHARS(obj)) ? BOOL_F : BOOL_T;
}

SCM lwinsch(win, obj)
     SCM win, obj;
{
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_winsch);
  if (INUMP(obj))
    return ERR==winsch(WIN(win), INUM(obj)) ? BOOL_F : BOOL_T;
  ASRTER(ICHRP(obj), obj, ARG2, s_winsch);
  return ERR==winsch(WIN(win), ICHR(obj)) ? BOOL_F : BOOL_T;
}

SCM lbox(win, vertch, horch)
     SCM win, vertch, horch;
{
  int v, h;
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_box);
  if (INUMP(vertch)) v = INUM(vertch);
  else {
    ASRTER(ICHRP(vertch), vertch, ARG2, s_box);
    v = ICHR(vertch);
  }
  if (INUMP(horch)) h = INUM(horch);
  else {
    ASRTER(ICHRP(horch), horch, ARG3, s_box);
    h = ICHR(horch);
  }
  return ERR==box(WIN(win), v, h) ? BOOL_F : BOOL_T;
}

static char s_getyx[] = "getyx", s_winch[] = "winch", s_unctrl[] = "unctrl";
SCM lgetyx(win)
     SCM win;
{
  int y, x;
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_getyx);
  getyx(WIN(win), y, x);
  return cons2(MAKINUM(y), MAKINUM(x), EOL);
}

SCM lwinch(win)
     SCM win;
{
  ASRTER(NIMP(win) && WINP(win), win, ARG1, s_winch);
  return MAKICHR(winch(WIN(win)));
}

SCM lunctrl(c)
     SCM c;
{
  ASRTER(ICHRP(c), c, ARG1, s_unctrl);
  {
    char *str = unctrl(ICHR(c));
    return makfrom0str(str);
  }
}
static char s_owidth[] = "output-port-width";
static char s_oheight[] = "output-port-height";
SCM owidth(arg)
     SCM arg;
{
  if (UNBNDP(arg)) arg = cur_outp;
  ASRTER(NIMP(arg) && OPOUTPORTP(arg), arg, ARG1, s_owidth);
  if (NIMP(*loc_stdscr)) {
    if (WINP(arg)) return MAKINUM(WIN(arg)->_maxx+1);
    else return MAKINUM(COLS);
  }
  return MAKINUM(80);
}
SCM oheight(arg)
     SCM arg;
{
  if (UNBNDP(arg)) arg = cur_outp;
  ASRTER(NIMP(arg) && OPOUTPORTP(arg), arg, ARG1, s_owidth);
  if (NIMP(*loc_stdscr))
    if (WINP(arg)) return MAKINUM(WIN(arg)->_maxy+1);
    else return MAKINUM(LINES);
  return MAKINUM(24);
}
SCM lrefresh()
{
  return MAKINUM(wrefresh(curscr));
}

#define SUBR0(lname, name) SCM lname(){name();return UNSPECIFIED;}
SUBR0(lnl, nl)
SUBR0(lnonl, nonl)
SUBR0(lcbreak, cbreak)
SUBR0(lnocbreak, nocbreak)
SUBR0(lecho, echo)
SUBR0(lnoecho, noecho)
SUBR0(lraw, raw)
SUBR0(lnoraw, noraw)
SUBR0(lsavetty, savetty)
SUBR0(lresetty, resetty)

static char s_nonl[] = "nonl", s_nocbreak[] = "nocbreak",
	    s_noecho[] = "noecho", s_noraw[] = "noraw";

static iproc subr0s[] = {
	{"initscr", linitscr},
	{"endwin", lendwin},
	{&s_nonl[2], lnl},
	{s_nonl, lnonl},
	{&s_nocbreak[2], lcbreak},
	{s_nocbreak, lnocbreak},
	{&s_noecho[2], lecho},
	{s_noecho, lnoecho},
	{&s_noraw[2], lraw},
	{s_noraw, lnoraw},
	{"resetty", lresetty},
	{"savetty", lsavetty},
	{"refresh", lrefresh},
	{0, 0}};

#define SUBRW(ln, n, s_n, sn) static char s_n[]=sn;\
	SCM ln(w)SCM w;\
	{ASRTER(NIMP(w) && WINP(w), w, ARG1, sn);\
	return ERR==n(WIN(w))?BOOL_F:BOOL_T;}

SUBRW(lwerase, werase, s_werase, "werase")
SUBRW(lwclear, wclear, s_wclear, "wclear")
SUBRW(lwclrtobot, wclrtobot, s_wclrtobot, "wclrtobot")
SUBRW(lwclrtoeol, wclrtoeol, s_wclrtoeol, "wclrtoeol")
SUBRW(lwdelch, wdelch, s_wdelch, "wdelch")
SUBRW(lwdeleteln, wdeleteln, s_wdeleteln, "wdeleteln")
SUBRW(lwinsertln, winsertln, s_winsertln, "winsertln")
SUBRW(lscroll, scroll, s_scroll, "scroll")
SUBRW(ltouchwin, touchwin, s_touchwin, "touchwin")
SUBRW(lwstandout, wstandout, s_wstandout, "wstandout")
SUBRW(lwstandend, wstandend, s_wstandend, "wstandend")

static iproc subr1s[] = {
	{s_werase, lwerase},
	{s_wclear, lwclear},
	{s_wclrtobot, lwclrtobot},
	{s_wclrtoeol, lwclrtoeol},
	{s_wdelch, lwdelch},
	{s_wdeleteln, lwdeleteln},
	{s_winsertln, lwinsertln},
	{s_scroll, lscroll},
	{s_touchwin, ltouchwin},
	{s_wstandout, lwstandout},
	{s_wstandend, lwstandend},
	{s_getyx, lgetyx},
	{s_winch, lwinch},
	{s_unctrl, lunctrl},
	{0, 0}};

#define SUBROPT(ln, n, s_n, sn) static char s_n[]=sn;\
	SCM ln(w, b)SCM w, b;\
	{ASRTER(NIMP(w) && WINP(w), w, ARG1, sn);\
	return ERR==n(WIN(w), BOOL_F != b)?BOOL_F:BOOL_T;}

SUBROPT(lidlok, idlok, s_idlok, "idlok")
SUBROPT(lleaveok, leaveok, s_leaveok, "leaveok")
SUBROPT(lscrollok, scrollok, s_scrollok, "scrollok")
SUBROPT(lnodelay, nodelay, s_nodelay, "nodelay")

/*  SUBROPT(lclearok, clearok, s_clearok, "clearok") */
static char s_clearok[] = "clearok";
SCM lclearok(w, b) SCM w, b;
{
  if (BOOL_T==w) return ERR==clearok(curscr, BOOL_F != b)?BOOL_F:BOOL_T;
  ASRTER(NIMP(w) && WINP(w), w, ARG1, s_clearok);
  return ERR==clearok(WIN(w), BOOL_F != b)?BOOL_F:BOOL_T;
}

static iproc subr2s[] = {
	{s_overlay, loverlay},
	{s_overwrite, loverwrite},
	{s_wadd, lwadd},
	{s_winsch, lwinsch},
	{s_clearok, lclearok},
	{s_idlok, lidlok},
	{s_leaveok, lleaveok},
	{s_scrollok, lscrollok},
	{s_nodelay, lnodelay},
	{0, 0}};

void init_crs()
{
  /*  savetty(); */
  /* "Stdscr" is a nearly inaccessible symbol used as a GC protect. */
  loc_stdscr = &CDR(sysintern("Stdscr", UNDEFINED));
  tc16_window = newptob(&winptob);

  init_iprocs(subr0s, tc7_subr_0);
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr2s, tc7_subr_2);

  make_subr(s_owidth, tc7_subr_1o, owidth);
  make_subr(s_oheight, tc7_subr_1o, oheight);

  make_subr(s_newwin, tc7_lsubr_2, lnewwin);
  make_subr(s_subwin, tc7_lsubr_2, lsubwin);

  make_subr(s_wmove, tc7_subr_3, lwmove);
  make_subr(s_mvwin, tc7_subr_3, lmvwin);
  make_subr(s_box, tc7_subr_3, lbox);
  add_feature("curses");
  add_final(lendwin);
}
