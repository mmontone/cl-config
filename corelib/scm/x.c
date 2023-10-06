/* "x.c" SCM interface to Xlib.
 * Copyright (C) 1999 Free Software Foundation, Inc.
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

/* Authors: Aubrey Jaffer (I have rewritten nearly all of it) and:
 *
 * Modified by Shigenobu Kimura (skimu@izanagi.phys.s.u-tokyo.ac.jp)
 * Author: Larry Campbell (campbell@world.std.com)
 *
 * Copyright 1992 by The Boston Software Works, Inc.
 * Permission to use for any purpose whatsoever granted, as long
 * as this copyright notice remains intact.  Please send bug fixes
 * or enhancements to the above email address.
 *
 * Generic X and Xlib functions for scm.
 * These functions do not depend on any toolkit.
 */

#include <stdio.h>
#include <X11/X.h>
#include <X11/Xlib.h>
#include <X11/Xcms.h>
/*#include <X11/Xcmsint.h>*/	/* For IntensityTbl */
#include <X11/Xresource.h>
#include <X11/Xutil.h>
#include <X11/Xatom.h>

#include "scm.h"

/* These structs are mallocated for use in SMOBS. */

struct xs_Display {
  SCM after;
  int screen_count;
  Display *dpy;
};

/* An array of struct xs_screen (following xs_Display) holds the
   root-windows and default colormaps. */

struct xs_screen {
  SCM root_window;
  SCM default_gcontext;
  SCM default_visual;
  SCM default_colormap;
};

struct xs_Window {
  SCM display;
  int screen_number;
  Display *dpy;
  union {
    Window win;
    Pixmap pm;
    Drawable drbl;
  } p;
};

struct xs_GContext {
  SCM display;
  int screen_number;
  Display *dpy;
  GC gc;
  SCM font;
  SCM tile;
  SCM stipple;
  SCM clipmask;
};

struct xs_Cursor {
  SCM display;
  Cursor cursor;
};

struct xs_Font {
  SCM display;
  Font font;
  SCM name;
};

struct xs_Colormap {
  SCM display;
  Display *dpy;
  Colormap cm;
};

/* These structs are for returning multiple values when processing
   procedure arguments. */

struct display_screen{
  SCM display;
  Display *dpy;
  int screen_number;
};

/* The cproto program fills x.h with ANSI-C prototypes of the
   functions in x.c. */

#include "x.h"

	       /* Macros for accessing these structs */

#define DISPLAY(x)	((struct xs_Display *)	CDR(x))
#define WINDOW(x)	((struct xs_Window *)	CDR(x))
#define CURSOR(x)	((struct xs_Cursor *)	CDR(x))
#define FONT(x)		((struct xs_Font *)	CDR(x))
#define COLORMAP(x)	((struct xs_Colormap *)	CDR(x))
#define GCONTEXT(x)	((struct xs_GContext *)	CDR(x))

#define XDISPLAY(x)	(DISPLAY(x)->dpy)
#define XWINDOW(x)	(WINDOW(x)->p.win)
#define XWINDISPLAY(x)	(WINDOW(x)->dpy)
#define XCURSOR(x)	(CURSOR(x)->cursor)
#define XFONT(x)	(FONT(x)->font)
#define XGCONTEXT(x)	(GCONTEXT(x)->gc)
#define XCOLORMAP(x)	(COLORMAP(x)->cm)
#define XGCONDISPLAY(x)	(GCONTEXT(x)->dpy)

/* Notice that types XVisualInfo, XcmsCCC, and XEvent don't have
   struct wrappers. */

#define XVISUALINFO(x)	((XVisualInfo *)	CDR(x))
#define XVISUAL(x)	(XVISUALINFO(x)->visual)
#define XCCC(x)		((XcmsCCC)		CDR(x))
#define XEVENT(x)	((XEvent *)		CDR(x))

			/* Type predicates */

#define DISPLAYP(x)	(TYP16(x)==tc16_xdisplay)
#define OPDISPLAYP(x)	(((0xffff | OPN) & (int)CAR(x))==(tc16_xdisplay | OPN))
#define WINDOWP(x)	(TYP16(x)==tc16_xwindow)
#define OPWINDOWP(x)	(((0xffff | OPN) & (int)CAR(x))==(tc16_xwindow | OPN))
#define COLORMAPP(x)	(TYP16(x)==tc16_xcolormap)
#define GCONTEXTP(x)	(TYP16(x)==tc16_xgcontext)
#define CCCP(x)		(TYP16(x)==tc16_xccc)
#define CURSORP(x)	(TYP16(x)==tc16_xcursor)
#define FONTP(x)	(TYP16(x)==tc16_xfont)
#define VISUALP(x)	(TYP16(x)==tc16_xvisual)
#define XEVENTP(x)	(TYP16(x)==tc16_xevent)

		     /* Scheme Procedure Names */

static char s_x_open_display[]		= "x:open-display";
static char s_x_close[]			= "x:close";
static char s_x_display_debug[]		= "x:display-debug";
static char s_x_default_screen[]	= "x:default-screen";
static char s_x_root_window[]		= "x:root-window";
static char s_x_default_gcontext[]	= "x:default-gc";
static char s_x_default_visual[]	= "x:default-visual";
static char s_x_default_colormap[]	= "x:default-colormap";
static char s_x_default_ccc[]		= "x:default-ccc";
/* static char s_x_ccc_screen_info[]	= "x:ccc-screen-info"; */
static char s_x_create_window[]		= "x:create-window";
static char s_x_window_set[]		= "x:window-set!";
static char s_x_window_ref[]		= "x:window-ref";
static char s_x_create_pixmap[]		= "x:create-pixmap";
static char s_x_get_window_property[]	= "x:get-window-property";
static char s_x_list_properties[]	= "x:list-properties";

static char s_x_map_window[]		= "x:map-window";
static char s_x_map_subwindows[]	= "x:map-subwindows";
static char s_x_unmap_window[]		= "x:unmap-window";
static char s_x_unmap_subwindows[]	= "x:unmap-subwindows";

static char s_x_create_gc[]		= "x:create-gc";
static char s_x_gc_set[]		= "x:gc-set!";
static char s_x_gc_ref[]		= "x:gc-ref";
static char s_x_copy_gc[]		= "x:copy-gc-fields!";

static char s_x_create_cursor[]		= "x:create-cursor";

static char s_x_load_font[]		= "x:load-font";

static char s_x_protocol_version[]	= "x:protocol-version";
static char s_x_vendor_release[]	= "x:vendor-release";
static char s_x_server_vendor[]		= "x:server-vendor";
static char s_x_next_event[]		= "x:next-event";
static char s_x_peek_event[]		= "x:peek-event";
static char s_x_events_queued[]		= "x:events-queued";
static char s_x_q_length[]		= "x:q-length";
static char s_x_pending[]		= "x:pending";
static char s_x_screen_count[]		= "x:screen-count";
static char s_x_screen_cells[]		= "x:screen-cells";
static char s_x_screen_depths[]		= "x:screen-depths";
static char s_x_screen_depth[]		= "x:screen-depth";
static char s_x_screen_size[]		= "x:screen-size";
static char s_x_screen_dimm[]		= "x:screen-dimensions";
static char s_x_screen_white[]		= "x:screen-white";
static char s_x_screen_black[]		= "x:screen-black";
static char s_x_make_visual[]		= "x:make-visual";
static char s_x_visual_class[]		= "x:visual-class";
static char s_x_visual_geometry[]	= "x:visual-geometry";
static char s_x_window_geometry[]	= "x:window-geometry";
static char s_x_window_geometry_set[]	= "x:window-geometry-set!";

static char s_x_create_colormap[]	= "x:create-colormap";
static char s_x_recreate_colormap[]	= "x:copy-colormap-and-free";
static char s_x_alloc_color_cells[]	= "x:alloc-colormap-cells";
static char s_x_free_color_cells[]	= "x:free-colormap-cells";
static char s_x_find_color[]		= "x:colormap-find-color";
static char s_x_color_set[]		= "x:colormap-set!";
static char s_x_color_ref[]		= "x:colormap-ref";
static char s_x_install_colormap[]	= "x:install-colormap";
/*  static char s_x_colormap_basis[]	= "x:colormap-basis"; */
/*  static char s_x_colormap_limits[]	= "x:colormap-limits"; */

static char s_x_clear_area[]		= "x:clear-area";
static char s_x_fill_rectangle[]	= "x:fill-rectangle";
/*  static char s_x_copy_area[]		= "x:copy-area"; */
static char s_x_draw_points[]		= "x:draw-points";
static char s_x_draw_segments[]		= "x:draw-segments";
static char s_x_draw_lines[]		= "x:draw-lines";
static char s_x_fill_poly[]		= "x:fill-polygon";
static char s_x_draw_string[]		= "x:draw-string";
static char s_x_image_string[]		= "x:image-string";

static char s_x_flush[]			= "x:flush";
static char s_x_event_ref[]		= "x:event-ref";
static char s_x_event_keysym[]		= "x:event->keysym";

		       /* Type-name strings */

static char s_gc[]	= "graphics-context";
#define s_display  (&s_x_open_display[7])
#define s_window   (&s_x_root_window[7])
#define s_cursor   (&s_x_create_cursor[9])
#define s_font     (&s_x_load_font[7])
#define s_colormap (&s_x_create_colormap[9])
#define s_visual   (&s_x_make_visual[7])

	   /* Scheme (SMOB) types defined in this module */

long tc16_xdisplay;
long tc16_xgcontext;
long tc16_xcolormap;
long tc16_xwindow;
long tc16_xcursor;
long tc16_xfont;
long tc16_xvisual;
long tc16_xevent;
long tc16_xccc;
XContext xtc_ccc, xtc_cmp;

/* We use OPN (which is already defined and used for PTOB ports) to
   keep track of whether objects of types Display and Window are open.
   The type xs_Window includes screen root-windows and pixmaps.  The
   SMOB (CAR) header bits SCROOT and PXMP keep track of which type of
   window the SMOB is. */

/*  #define OPN		(1L<<16) */
/*  #define RDNG		(2L<<16) */
/*  #define WRTNG		(4L<<16) */
#define SCROOT		(8L<<16)
#define PXMP		(16L<<16)

/* Utility routines for creating SCM-wrapped X structs and the SMOB
   routines for collecting them. */

SCM make_xwindow(display, screen_number, win, pxmp, rootp)
     SCM display;
     int screen_number;
     Drawable win;
     char pxmp, rootp;
{
  SCM z;
  struct xs_Window *xsw;
  DEFER_INTS;
  z = must_malloc_cell((long)sizeof(struct xs_Window),
		       (SCM)(tc16_xwindow | OPN
			     | (pxmp ? PXMP : 0L)
			     | (rootp ? SCROOT : 0L)),
		       s_window);
  xsw = WINDOW(z);
  xsw->display = display;
  xsw->dpy = XDISPLAY(display);
  xsw->screen_number = screen_number;
  if (pxmp) xsw->p.pm = (Pixmap)win;
  else xsw->p.win = (Window)win;
  ALLOW_INTS;
  return z;
}
static SCM mark_xwindow(ptr)
     SCM ptr;
{
  if (CLOSEDP(ptr)) return BOOL_F;
  return WINDOW(ptr)->display;
}
static sizet free_xwindow(ptr)
     CELLPTR ptr;
{
  SCM td = CAR((SCM)ptr);
  if (!(td & OPN)) return 0;
  if (!(td & SCROOT)) {
    struct xs_Window *xsw = WINDOW((SCM)ptr);
    SCM sd = xsw->display;
    if (NIMP(sd) && OPDISPLAYP(sd)) {
      if (td & PXMP) XFreePixmap(xsw->dpy, xsw->p.pm);
      else XDestroyWindow(xsw->dpy, xsw->p.win);
    }
  }
  must_free((char *)CDR((SCM)ptr), sizeof(struct xs_Window));
  CAR((SCM)ptr) = td & ~OPN;
  return sizeof(struct xs_Window);
}

SCM make_xcolormap(sdpy, cmp)
     SCM sdpy;
     Colormap cmp;
{
  SCM z;
  struct xs_Colormap *xcm;
  XPointer scmptr;
  if (!XFindContext(XDISPLAY(sdpy), (XID)cmp, xtc_cmp, &scmptr))
    return (SCM)scmptr;
  DEFER_INTS;
  z = must_malloc_cell((long)sizeof(struct xs_Colormap),
		       (SCM)tc16_xcolormap,
		       s_colormap);
  xcm = COLORMAP(z);
  xcm->display = sdpy;
  xcm->dpy = DISPLAY(xcm->display)->dpy;
  xcm->cm = cmp;
  XSaveContext(XDISPLAY(sdpy), (XID)cmp, xtc_cmp, z);
  ALLOW_INTS;
  return z;
}
static SCM mark_xcolormap(ptr)
     SCM ptr;
{
  struct xs_Colormap *xcm;
  if (CLOSEDP(ptr)) return BOOL_F;
  xcm = COLORMAP(ptr);
  gc_mark(CCC2SCM_P(XcmsCCCOfColormap(xcm->dpy, xcm->cm)));
  return xcm->display;
}
static sizet free_xcolormap(ptr)
     CELLPTR ptr;
{
  struct xs_Colormap *xcmp = COLORMAP((SCM)ptr);
  SCM sdpy = xcmp->display;
  if (NIMP(sdpy) && OPDISPLAYP(sdpy))
    XFreeColormap(xcmp->dpy, xcmp->cm);
  must_free((char *)CDR((SCM)ptr), sizeof(struct xs_Colormap));
  return sizeof(struct xs_Colormap);
}

SCM make_xdisplay(d)
     Display *d;
{
  SCM z;
  struct xs_screen *scrns;
  struct xs_Display *xsd;
  int idx = ScreenCount(d);
  DEFER_INTS;
  z = must_malloc_cell((long)sizeof(struct xs_Display)
		       + idx * sizeof(struct xs_screen),
		       (SCM)tc16_xdisplay | OPN,
		       s_display);
  xsd = DISPLAY(z);
  xsd->after = BOOL_F;
  xsd->screen_count = idx;
  xsd->dpy = d;
  scrns = (struct xs_screen *)(xsd + 1);
  while (idx--) {
    scrns[idx].root_window = BOOL_F;
    scrns[idx].default_gcontext = BOOL_F;
    scrns[idx].default_visual = BOOL_F;
    scrns[idx].default_colormap = BOOL_F;
  }
  ALLOW_INTS;
  idx = xsd->screen_count;
  while (idx--) {
    scrns[idx].root_window =
      make_xwindow(z, idx, RootWindow(d, idx), (char) 0, (char) 1);
    scrns[idx].default_gcontext =
      make_xgcontext(z, idx, XDefaultGC(d, idx), !0);
    scrns[idx].default_visual =
      make_xvisual(visual2visualinfo(d, DefaultVisual(d, idx)));
    scrns[idx].default_colormap =
      make_xcolormap(z, DefaultColormap(d, idx));
  }
  return z;
}
static SCM mark_xdisplay(ptr)
     SCM ptr;
{
  if (CLOSEDP(ptr)) return BOOL_F;
  {
    struct xs_Display *xsd = DISPLAY((SCM)ptr);
    struct xs_screen *scrns = (struct xs_screen *)(xsd + 1);
    int idx = xsd->screen_count;
    while (--idx) {
      SCM scmp = scrns[idx].default_colormap;
      gc_mark(scrns[idx].root_window);
      gc_mark(scrns[idx].default_gcontext);
      gc_mark(scrns[idx].default_visual);
      gc_mark(scmp);
      gc_mark (CCC2SCM_P(XcmsCCCOfColormap(xsd->dpy, XCOLORMAP(scmp))));
    }
    gc_mark(scrns[idx].root_window);
    gc_mark(scrns[idx].default_gcontext);
    gc_mark(scrns[idx].default_visual);
    return scrns[idx].default_colormap;
  }
}
static sizet free_xdisplay(ptr)
     CELLPTR ptr;
{
  SCM td = CAR((SCM)ptr);
  if (!(td & OPN)) return 0;
  {
    struct xs_Display *xsd = DISPLAY((SCM)ptr);
    sizet len = sizeof(struct xs_Display) +
      xsd->screen_count * sizeof(struct xs_screen);
    XCloseDisplay(xsd->dpy);
    must_free((char *)xsd, len);
    CAR((SCM)ptr) = td & ~OPN;
    return len;
  }
}

SCM make_xgcontext(d, screen_number, gc, rootp)
     SCM d;
     int screen_number;
     GC gc;
     int rootp;
{
  SCM z;
  struct xs_GContext *xgc;
  DEFER_INTS;
  z = must_malloc_cell((long)sizeof(struct xs_GContext),
		       (SCM)tc16_xgcontext | (rootp ? SCROOT : 0L),
		       s_gc);
  xgc = GCONTEXT(z);
  xgc->display = d;
  xgc->screen_number = screen_number;
  xgc->dpy = XDISPLAY(d);
  xgc->gc = gc;
  xgc->font = BOOL_F;
  xgc->tile = BOOL_F;
  xgc->stipple = BOOL_F;
  xgc->clipmask = BOOL_F;
  ALLOW_INTS;
  return z;
}
static SCM mark_xgcontext(ptr)
     SCM ptr;
{
  struct xs_GContext *xgc = GCONTEXT(ptr);
  gc_mark(xgc->font);
  gc_mark(xgc->tile);
  gc_mark(xgc->stipple);
  gc_mark(xgc->clipmask);
  return xgc->display;
}
static sizet free_xgcontext(ptr)
     CELLPTR ptr;
{
  SCM td = CAR((SCM)ptr);
  if (!(td & OPN)) return 0;
  if (!(td & SCROOT)) {
    struct xs_GContext *xgc = GCONTEXT((SCM)ptr);
    SCM sd = xgc->display;
    if (NIMP(sd) && OPDISPLAYP(sd)) XFreeGC(xgc->dpy, xgc->gc);
  }
  must_free((char *)CDR((SCM)ptr), sizeof(struct xs_GContext));
  return sizeof(struct xs_GContext);
}

SCM make_xcursor(display, cursor)
     SCM display;
     Cursor cursor;
{
  SCM z;
  struct xs_Cursor *xcsr;
  DEFER_INTS;
  z = must_malloc_cell((long)sizeof(struct xs_Cursor),
		       (SCM)tc16_xcursor,
		       s_cursor);
  xcsr = CURSOR(z);
  xcsr->display = display;
  xcsr->cursor = cursor;
  ALLOW_INTS;
  return z;
}
static SCM mark_xcursor(ptr)
     SCM ptr;
{
  if (CLOSEDP(ptr)) return BOOL_F;
  return CURSOR(ptr)->display;
}
static sizet free_xcursor(ptr)
     CELLPTR ptr;
{
  struct xs_Cursor *xcsr = CURSOR((SCM)ptr);
  SCM sdpy = xcsr->display;
  if (NIMP(sdpy) && OPDISPLAYP(sdpy)) {
    struct xs_Display *xdp = DISPLAY(sdpy);
    XFreeCursor(xdp->dpy, xcsr->cursor);
  }
  must_free((char *)CDR((SCM)ptr), sizeof(struct xs_Cursor));
  return sizeof(struct xs_Cursor);
}
SCM make_xfont(display, font, name)
     SCM display;
     Font font;
     SCM name;
{
  SCM z;
  struct xs_Font *xfnt;
  DEFER_INTS;
  z = must_malloc_cell((long)sizeof(struct xs_Font),
		       (SCM)tc16_xfont,
		       s_font);
  xfnt = FONT(z);
  xfnt->display = display;
  xfnt->font = font;
  xfnt->name = name;
  ALLOW_INTS;
  return z;
}
static SCM mark_xfont(ptr)
     SCM ptr;
{
  struct xs_Font *xfn = FONT(ptr);
  gc_mark(xfn->name);
  return xfn->display;
}
static sizet free_xfont(ptr)
     CELLPTR ptr;
{
  struct xs_Font *xfnt = FONT((SCM)ptr);
  SCM sdpy = xfnt->display;
  if (NIMP(sdpy) && OPDISPLAYP(sdpy)) {
    struct xs_Display *xdp = DISPLAY(sdpy);
    XUnloadFont(xdp->dpy, xfnt->font);
  }
  must_free((char *)CDR((SCM)ptr), sizeof(struct xs_Font));
  return sizeof(struct xs_Font);
}

SCM make_xvisual(vsl)
     XVisualInfo *vsl;
{
  SCM s_vsl;
  NEWCELL(s_vsl);
  DEFER_INTS;
  CAR(s_vsl) = tc16_xvisual;
  SETCDR(s_vsl, vsl);
  ALLOW_INTS;
  return s_vsl;
}

SCM CCC2SCM_P(ccc)
     XcmsCCC ccc;
{
  XPointer scmptr;
  if (XFindContext(ccc->dpy, (XID)ccc, xtc_ccc, &scmptr))
    return BOOL_F;
  return (SCM)scmptr;
}
SCM CCC2SCM(ccc)
     XcmsCCC ccc;
{
  SCM s_ccc = CCC2SCM_P(ccc);
  if (FALSEP(s_ccc)) {
    NEWCELL(s_ccc);
    DEFER_INTS;
    CAR(s_ccc) = tc16_xccc;
    SETCDR(s_ccc, ccc);
    XSaveContext(ccc->dpy, (XID)ccc, xtc_ccc, s_ccc);
    ALLOW_INTS;
  }
  return s_ccc;
}
static sizet free_xccc(ptr)
     CELLPTR ptr;
{
  XcmsCCC ccc = XCCC((SCM)ptr);
  XDeleteContext(ccc->dpy, (XID)ccc, xtc_ccc);
  XcmsFreeCCC(ccc);
  return 0;
}

SCM make_xevent(e)
XEvent *e;
{
  SCM w;
  XEvent *ec;

  ec = (XEvent *) must_malloc(sizeof(XEvent), "X event");
  (void)memcpy(ec, e, sizeof(XEvent));
  NEWCELL(w);
  DEFER_INTS;
  CAR(w) = tc16_xevent;
  SETCDR(w, ec);
  ALLOW_INTS;
  return w;
}
sizet x_free_xevent(ptr)
     CELLPTR ptr;
{
  must_free(CHARS(ptr), sizeof(XEvent));
  return sizeof(XEvent);
}

/* Utility macro and functions for checking and coercing SCM arguments. */

#define GET_NEXT_INT(result, args, err, rtn) \
	ASRTER(NIMP(args) && CONSP(args) && INUMP(CAR(args)), args, err, rtn); \
	result = INUM(CAR(args)); \
	args = CDR(args);

void scm2XPoint(signp, dat, ipr, pos, s_caller)
     int signp;
     SCM dat;
     XPoint *ipr;
     char *pos, *s_caller;
{
  SCM x, y;
  if (IMP(dat)) badarg: wta(dat, pos, s_caller);
  if (CONSP(dat)) {
    if (INUMP(CDR(dat))) {
      x = CAR(dat);
      y = CDR(dat);
    }
    else {
      ASRTGO(2==ilength(dat), badarg);
      x = CAR(dat);
      y = CAR(CDR(dat));
    }
  }
  else switch TYP7(dat) {
  default: goto badarg;
  case tc7_vector:
    ASRTGO(2==LENGTH(dat), badarg);
    x = VELTS(dat)[0];
    y = VELTS(dat)[1];
    break;
  case tc7_VfixN32: case tc7_VfixZ32:
    ASRTGO(2==LENGTH(dat), badarg);
    x = MAKINUM(((long *)VELTS(dat))[0]);
    y = MAKINUM(((long *)VELTS(dat))[1]);
    break;
  case tc7_VfixZ16:
    ASRTGO(2==LENGTH(dat), badarg);
    x = MAKINUM(((short *)VELTS(dat))[0]);
    y = MAKINUM(((short *)VELTS(dat))[1]);
    break;
  case tc7_smob:
    ASRTGO(ARRAYP(dat) && 1==ARRAY_NDIM(dat) &&
	   0==ARRAY_DIMS(dat)[0].lbnd && 1==ARRAY_DIMS(dat)[0].ubnd,
	   badarg);
      x = aref(dat, MAKINUM(0L));
      y = aref(dat, MAKINUM(1L));
      break;
  }
  ASRTGO(INUMP(x) && INUMP(y), badarg);
  ipr->x = INUM(x);
  ipr->y = INUM(y);
  ASRTGO((ipr->x==INUM(x)) && (ipr->y==INUM(y))
	 && (signp ? !0 : ((x >= 0) && (y >= 0))), badarg);
}
int scm2XColor(s_dat, xclr)
     SCM s_dat;
     XColor *xclr;
{
  SCM dat = s_dat;
  unsigned int ura[3];
  int idx;
/*    if (INUMP(dat)) { */
/*      xclr->red   = (dat>>16 & 0x00ff) * 0x0101; */
/*      xclr->green = (dat>>8  & 0x00ff) * 0x0101; */
/*      xclr->blue  = (dat     & 0x00ff) * 0x0101; */
/*    } */
/*    else */
  if (IMP(dat)) return 0;
  else if (3==ilength(dat))
    for (idx = 0; idx < 3; idx++) {
      SCM clr = CAR(dat);
      if (!INUMP(clr)) return 0;
      ura[idx] = INUM(clr);
      dat = CDR(dat);
    }
  else if (VECTORP(dat) && (3==LENGTH(dat)))
    for (idx = 0; idx < 3; idx++) {
      if (!INUMP(VELTS(dat)[idx])) return 0;
      ura[idx] = INUM(VELTS(dat)[idx]);
    }
  else return 0;
  xclr->red = ura[0];
  xclr->green = ura[1];
  xclr->blue = ura[2];
  return !0;
}
int scm2xpointslen(sara, s_caller)
     SCM sara;
     char *s_caller;
{
  array_dim *adm;
  int len;
  if (!(NIMP(sara) && ARRAYP(sara) && 2==ARRAY_NDIM(sara))) return -1;
  adm = ARRAY_DIMS(sara);
  if (!((1==(adm[1].ubnd - adm[1].lbnd))
	&& (1==adm[1].inc)
	&& ARRAY_CONTP(sara)
	&& (tc7_VfixZ16==TYP7(ARRAY_V(sara))))) return -1;
  len = 1 + adm[0].ubnd - adm[0].lbnd;
  if (len < 0) return 0;
  return len;
}
void scm2display_screen(dat, optidx, dspscn, s_caller)
     SCM dat;
     SCM optidx;
     struct display_screen *dspscn;
     char *s_caller;
{
  ASRTGO(NIMP(dat), badarg);
  if (OPDISPLAYP(dat)) {
    dspscn->display = dat;
    dspscn->dpy = XDISPLAY(dat);
    if (UNBNDP(optidx)) dspscn->screen_number = DefaultScreen(dspscn->dpy);
    else if (INUMP(optidx) && (INUM(optidx) < DISPLAY(dat)->screen_count))
      dspscn->screen_number = INUM(optidx);
    else wta(optidx, (char *)ARG2, s_caller);
  }
  else if (OPWINDOWP(dat)) {
    struct xs_Window *xsw = WINDOW(dat);
    dspscn->display = xsw->display;
    dspscn->dpy = xsw->dpy;
    dspscn->screen_number = xsw->screen_number;
    ASRTGO(UNBNDP(optidx), badarg);
  }
  else badarg: wta(dat, (char *)ARG1, s_caller);
}

#define OpPxmpMask (0xffff | OPN | PXMP)
#define OpPxmp (tc16_xwindow | OPN | PXMP)

SCM thevalue(obj)
     SCM obj;
{
  if (NIMP(obj) && SYMBOLP(obj))
    return ceval(obj, (SCM)EOL, (SCM)EOL);
  else return obj;
}

Pixmap thepxmap(obj, s_caller)
     SCM obj;
     char *s_caller;
{
  if (FALSEP(obj) || (INUM0==obj)) return 0L;
  ASRTER(NIMP(obj) && ((OpPxmpMask & (int)CAR(obj))==OpPxmp),
	 obj, ARGn, s_caller);
  return WINDOW(obj)->p.pm;
}
Font thefont(obj, s_caller)
     SCM obj;
     char *s_caller;
{
  ASRTER(NIMP(obj) && FONTP(obj), obj, ARGn, s_caller);
  return FONT(obj)->font;
}
Colormap thecmap(obj, s_caller)
     SCM obj;
     char *s_caller;
{
  if (FALSEP(obj) || (INUM0==obj)) return 0L;
  ASRTER(NIMP(obj) && COLORMAPP(obj), obj, ARGn, s_caller);
  return COLORMAP(obj)->cm;
}
Cursor thecsr(obj, s_caller)
     SCM obj;
     char *s_caller;
{
  if (FALSEP(obj) || (INUM0==obj)) return 0L;
  ASRTER(NIMP(obj) && CURSORP(obj), obj, ARGn, s_caller);
  return CURSOR(obj)->cursor;
}
Bool thebool(obj, s_caller)
     SCM obj;
     char *s_caller;
{
  SCM val = thevalue(obj);
  ASRTER(BOOL_F==val || BOOL_T==val, obj, ARGn, s_caller);
  return NFALSEP(val);
}
int theint(obj, s_caller)
     SCM obj;
     char *s_caller;
{
  SCM val = thevalue(obj);
  ASRTER(INUMP(val), obj, ARGn, s_caller);
  return INUM(val);
}
int theuint(obj, s_caller)
     SCM obj;
     char *s_caller;
{
  SCM val = thevalue(obj);
  ASRTER(INUMP(val) && (0 <= INUM(val)), obj, ARGn, s_caller);
  return INUM(val);
}

static int args2valmask(oargs, s_caller)
     SCM oargs;
     char *s_caller;
{
  SCM args = oargs;
  int attr, len, attr_mask = 0;
  if (!(len = ilength(args))) return 0;
  while (len) {
    ASRTER(NIMP(args), oargs, WNA, s_caller);
    attr = theint(CAR(args), s_caller); args = CDR(args);
    attr_mask |= attr;
    len -= 1;
  }
  return attr_mask;
}
static int args2xgcvalues(sgc, vlu, oargs)
     SCM sgc;
     XGCValues *vlu;
     SCM oargs;
{
  struct xs_GContext *xgc = GCONTEXT(sgc);
  SCM sval, args = oargs;
  int attr, len, attr_mask = 0;
/*    (void)memset((char *)vlu, 0, sizeof(XGCValues)); */
  if (!(len = ilength(args))) return 0;
  ASRTER(len > 0 && (! (len & 1)), oargs, WNA, s_gc);
  while (len) {
    ASRTER(NIMP(args), oargs, WNA, s_gc);
    attr = theint(CAR(args), s_gc); args = CDR(args);
    ASRTER(NIMP(args), oargs, WNA, s_gc);
    sval = CAR(args); args = CDR(args);
    attr_mask |= attr;
    switch (attr) {

    case GCFunction:	vlu->function	=  theint(sval, s_gc); break;
    case GCPlaneMask:	vlu->plane_mask	= theuint(sval, s_gc); break;
    case GCForeground:	vlu->foreground	= theuint(sval, s_gc); break;
    case GCBackground:	vlu->background	= theuint(sval, s_gc); break;
    case GCLineWidth:	vlu->line_width	=  theint(sval, s_gc); break;
    case GCLineStyle:	vlu->line_style	=  theint(sval, s_gc); break;
    case GCCapStyle:	vlu->cap_style	=  theint(sval, s_gc); break;
    case GCJoinStyle:	vlu->join_style	=  theint(sval, s_gc); break;
    case GCFillStyle:	vlu->fill_style	=  theint(sval, s_gc); break;
    case GCFillRule:	vlu->fill_rule	=  theint(sval, s_gc); break;
    case GCTile:	vlu->tile	= thepxmap(sval, s_gc);
      xgc->tile = sval;
      break;
    case GCStipple:	vlu->stipple	= thepxmap(sval, s_gc);
      xgc->stipple = sval;
      break;
    case GCTileStipXOrigin: vlu->ts_x_origin = theint(sval, s_gc); break;
    case GCTileStipYOrigin: vlu->ts_y_origin = theint(sval, s_gc); break;
    case (GCTileStipXOrigin | GCTileStipYOrigin): {
      XPoint position;
      scm2XPoint(!0, sval, &position, (char *)ARGn, s_gc);
      vlu->ts_x_origin = position.x;
      vlu->ts_y_origin = position.y;
    } break;
    case GCFont:	vlu->font	= thefont(sval, s_gc);
      xgc->font = sval;
      break;
    case GCSubwindowMode: vlu->subwindow_mode = theint(sval, s_gc); break;
    case GCGraphicsExposures: vlu->graphics_exposures = thebool(sval, s_gc); break;
    case GCClipXOrigin:	vlu->clip_x_origin = theint(sval, s_gc); break;
    case GCClipYOrigin:	vlu->clip_y_origin = theint(sval, s_gc); break;
    case (GCClipXOrigin | GCClipYOrigin): {
      XPoint position;
      scm2XPoint(!0, sval, &position, (char *)ARGn, s_gc);
      vlu->clip_x_origin = position.x;
      vlu->clip_y_origin = position.y;
    } break;
    case GCClipMask:	vlu->clip_mask	= thepxmap(sval, s_gc);
      xgc->clipmask = sval;
      break;
    case GCDashOffset:	vlu->dash_offset = theint(sval, s_gc); break;
    case GCDashList:	vlu->dashes	= (char)theint(sval, s_gc); break;
    case GCArcMode:	vlu->arc_mode	= theint(sval, s_gc); break;

    default: ASRTER(0, MAKINUM(attr), ARGn, s_gc);
    }
    len -= 2;
  }
  return attr_mask;
}
static int args2winattribs(vlu, oargs)
     XSetWindowAttributes *vlu;
     SCM oargs;
{
  SCM sval, args = oargs;
  int attr, len, attr_mask = 0;
  /* (void)memset((char *)vlu, 0, sizeof(XSetWindowAttributes)); */
  if (!(len = ilength(args))) return 0;
  ASRTER(len > 0 && (! (len & 1)), oargs, WNA, s_window);
  while (len) {
    ASRTER(NIMP(args), oargs, WNA, s_window);
    attr = theint(CAR(args), s_window); args = CDR(args);
    ASRTER(NIMP(args), oargs, WNA, s_window);
    sval = CAR(args); args = CDR(args);
    attr_mask |= attr;
    switch (attr) {

    case CWBackPixmap:	vlu->background_pixmap=thepxmap(sval, s_window); break;
    case CWBackPixel:	vlu->background_pixel = theuint(sval, s_window); break;
    case CWBorderPixmap:vlu->border_pixmap    =thepxmap(sval, s_window); break;
    case CWBorderPixel:	vlu->border_pixel     = theuint(sval, s_window); break;
    case CWBitGravity:	vlu->bit_gravity      =  theint(sval, s_window); break;
    case CWWinGravity:	vlu->win_gravity      =  theint(sval, s_window); break;
    case CWBackingStore:vlu->backing_store    =  theint(sval, s_window); break;
    case CWBackingPlanes:vlu->backing_planes  = theuint(sval, s_window); break;
    case CWBackingPixel:vlu->backing_pixel    = theuint(sval, s_window); break;
    case CWOverrideRedirect:vlu->override_redirect =
						thebool(sval, s_window); break;
    case CWSaveUnder:	vlu->save_under	      = thebool(sval, s_window); break;
    case CWEventMask:	vlu->event_mask	      =  theint(sval, s_window); break;
    case CWDontPropagate:vlu->do_not_propagate_mask =
						thebool(sval, s_window); break;
    case CWColormap:	vlu->colormap	      = thecmap(sval, s_window); break;
    case CWCursor:	vlu->cursor	      =  thecsr(sval, s_window); break;

    default: ASRTER(0, MAKINUM(attr), ARGn, s_window);
    }
    len -= 2;
  }
  return attr_mask;
}
static int args2wincfgs(vlu, oargs)
     XWindowChanges *vlu;
     SCM oargs;
{
  SCM sval, args = oargs;
  int cfgs, len, cfgs_mask = 0;
  /* (void)memset((char *)vlu, 0, sizeof(XWindowChanges)); */
  if (!(len = ilength(args))) return 0;
  ASRTER(len > 0 && (! (len & 1)), oargs, WNA, s_window);
  while (len) {
    ASRTER(NIMP(args), oargs, WNA, s_window);
    cfgs = theint(CAR(args), s_window); args = CDR(args);
    ASRTER(NIMP(args), oargs, WNA, s_window);
    sval = CAR(args); args = CDR(args);
    cfgs_mask |= cfgs;
    switch (cfgs) {

    case CWX:		vlu->x		      = theuint(sval, s_window); break;
    case CWY:		vlu->y		      = theuint(sval, s_window); break;
    case CWWidth:	vlu->width	      = theuint(sval, s_window); break;
    case CWHeight:	vlu->height	      = theuint(sval, s_window); break;
    case CWBorderWidth:	vlu->border_width     = theuint(sval, s_window); break;
    case CWSibling:	vlu->sibling          =thepxmap(sval, s_window); break;
    case CWStackMode:	vlu->stack_mode       =  theint(sval, s_window); break;
    default: ASRTER(0, MAKINUM(cfgs), ARGn, s_window);
    }
    len -= 2;
  }
  return cfgs_mask;
}

		   /* Scheme-visible procedures */

SCM x_open_display(dpy_name)
     SCM dpy_name;
{
  Display *display;
  if (FALSEP(dpy_name)) dpy_name = nullstr;
  ASRTER(NIMP(dpy_name) && STRINGP(dpy_name), dpy_name, ARG1, s_x_open_display);
  display = XOpenDisplay(CHARS(dpy_name));
  return (display ? make_xdisplay(display) : BOOL_F);
}
SCM x_display_debug(sd, si)
     SCM sd, si;
{
  int (*previous_after_function)();
  struct display_screen dspscn;
  scm2display_screen(sd, UNDEFINED, &dspscn, s_x_display_debug);
  previous_after_function =
    XSynchronize(dspscn.dpy, thebool(si, s_x_display_debug));
  return UNSPECIFIED;
}
SCM x_default_screen(sdpy)
     SCM sdpy;
{
  ASRTER(NIMP(sdpy) && OPDISPLAYP(sdpy), sdpy, ARG1, s_x_default_screen);
  return MAKINUM(DefaultScreen(XDISPLAY(sdpy)));
}

SCM x_create_window(swin, spos, sargs)
     SCM swin, spos, sargs;
{
  XPoint position, size;
  unsigned int border_width;
  Window window;
  int len = ilength(sargs);

  ASRTER(NIMP(swin) && OPWINDOWP(swin), swin, ARG1, s_x_create_window);
  scm2XPoint(!0, spos, &position, (char *)ARG2, s_x_create_window);
  scm2XPoint(0, CAR(sargs), &size, (char *)ARG3, s_x_create_window);
  sargs = CDR(sargs);
  GET_NEXT_INT(border_width, sargs, ARG4, s_x_create_window);
  if (4==len) {
    unsigned long border;
    unsigned long background;
    GET_NEXT_INT(border, sargs, ARG5, s_x_create_window);
    GET_NEXT_INT(background, sargs, ARGn, s_x_create_window);
    window = XCreateSimpleWindow(XWINDISPLAY(swin), XWINDOW(swin),
				 position.x, position.y, /* initial placement */
				 size.x, size.y,
				 border_width,
				 border, background); /* pixel values */
  } else {
    int depth;
    unsigned int class;
    SCM svis;
    unsigned long valuemask;
    XSetWindowAttributes attributes;
    ASRTER(5 <= len, sargs, WNA, s_x_create_window);
    GET_NEXT_INT(depth, sargs, ARG5, s_x_create_window);
    GET_NEXT_INT(class, sargs, ARGn, s_x_create_window);
    svis = CAR(sargs); sargs = CDR(sargs);
    ASRTER(NIMP(svis) && VISUALP(svis), svis, ARGn, s_x_create_window);
    valuemask = args2winattribs(&attributes, sargs);
    window = XCreateWindow(XWINDISPLAY(swin), XWINDOW(swin),
			   position.x, position.y, /* initial placement */
			   size.x, size.y,
			   border_width,
			   depth,
			   class,
			   XVISUAL(svis),
			   valuemask,
			   &attributes);
  }
  return window ? make_xwindow(WINDOW(swin)->display,
			       WINDOW(swin)->screen_number,
			       window, (char) 0, (char) 0)
    : BOOL_F;
}
SCM x_create_pixmap(obj, s_size, s_depth)
     SCM obj, s_size, s_depth;
{
  unsigned int depth = INUM(s_depth);
  SCM display;
  Display *dpy;
  int scn;
  Drawable drawable;
  Pixmap p;
  XPoint size;
  if (IMP(obj)) badarg1: wta(obj, (char *)ARG1, s_x_create_pixmap);
  if (OPDISPLAYP(obj)) {
    display = obj;
    dpy = XDISPLAY(display);
    scn = DefaultScreen(dpy);
    drawable = RootWindow(dpy, scn);
  }
  else if (OPWINDOWP(obj)) {
    display = WINDOW(obj)->display;
    dpy = XDISPLAY(display);
    scn = WINDOW(obj)->screen_number;
    drawable = WINDOW(obj)->p.drbl;
  }
  else goto badarg1;
  scm2XPoint(0, s_size, &size, (char *)ARG2, s_x_create_pixmap);
  ASRTER(INUMP(s_depth) && depth >= 0, s_depth, ARG3, s_x_create_pixmap);
  p = XCreatePixmap(dpy, drawable, size.x, size.y, depth);
  return make_xwindow(display, scn, p, (char) 1, (char) 0);
}
SCM x_window_ref(oargs)
     SCM oargs;
{
  SCM swn, args = oargs, sval = BOOL_F;
  SCM vals = cons(BOOL_T, EOL), valend = vals;
  struct xs_Window *xwn;
  XWindowAttributes vlu;
  int attr, len = ilength(args);
  /* (void)memset((char *)&vlu, 0, sizeof(XWindowAttributes)); */
  ASRTER(len > 0, oargs, WNA, s_x_window_ref);
  if (1==len--) return EOL;
  swn = CAR(args); args = CDR(args);
  ASRTER(NIMP(swn) && WINDOWP(swn), swn, ARG1, s_x_window_ref);
  xwn = WINDOW(swn);
  if (!XGetWindowAttributes(xwn->dpy, xwn->p.win, &vlu)) return BOOL_F;
  while (len) {
    attr = theint(CAR(args), s_x_window_ref); args = CDR(args);
    switch (attr) {

    case CWBackPixel:	 sval = MAKINUM(vlu.backing_pixel); break;
    case CWBitGravity:	 sval = MAKINUM(vlu.bit_gravity); break;
    case CWWinGravity:	 sval = MAKINUM(vlu.win_gravity); break;
    case CWBackingStore: sval = MAKINUM(vlu.backing_store); break;
    case CWBackingPlanes:sval = MAKINUM(vlu.backing_planes); break;
    case CWBackingPixel: sval = MAKINUM(vlu.backing_pixel); break;
    case CWOverrideRedirect:sval = x_make_bool(vlu.override_redirect); break;
    case CWSaveUnder:	 sval = x_make_bool(vlu.save_under); break;
    case CWEventMask:	 sval = MAKINUM(vlu.your_event_mask); break;
    case CWDontPropagate:sval = MAKINUM(vlu.do_not_propagate_mask); break;
    case CWColormap:	 sval = make_xcolormap(xwn->display, vlu.colormap); break;

    default: ASRTER(0, MAKINUM(attr), ARGn, s_x_window_ref);
    }
    CAR(valend) = sval;
    CDR(valend) = cons(BOOL_T, EOL);
    len -= 1;
    if (len) valend = CDR(valend);
    else CDR(valend) = EOL;
  }
  return vals;
}
SCM x_window_set(args)
     SCM args;
{
  SCM swn;
  struct xs_Window *xwn;
  XSetWindowAttributes vlu;
  unsigned long mask;

  ASRTER(NIMP(args), args, WNA, s_x_window_set);
  swn = CAR(args); args = CDR(args);
  ASRTER(NIMP(swn) && WINDOWP(swn), swn, ARG1, s_x_window_set);
  xwn = WINDOW(swn);
  mask = args2winattribs(&vlu, args);
  XChangeWindowAttributes(xwn->dpy, xwn->p.win, mask, &vlu);
  return UNSPECIFIED;
}

SCM x_window_geometry(swin)
     SCM swin;
{
  struct xs_Window *sxw;
  Window root;
  Status sts;
  int x, y;
  unsigned int w, h, border_width, depth;

  ASRTER(NIMP(swin) && OPWINDOWP(swin), swin, ARG1, s_x_window_geometry);
  sxw = WINDOW(swin);
  sts = XGetGeometry(sxw->dpy, sxw->p.drbl, &root, &x, &y,
		     &w, &h, &border_width, &depth);
  if (!sts) return BOOL_F;
  return cons2(cons2(MAKINUM(x), MAKINUM(y), EOL),
	       cons2(MAKINUM(w), MAKINUM(h), EOL),
	       cons2(MAKINUM(border_width), MAKINUM(depth), EOL));
}
SCM x_window_geometry_set(args)
     SCM args;
{
  SCM swn;
  struct xs_Window *xwn;
  XWindowChanges vlu;
  unsigned long mask;

  ASRTER(NIMP(args), args, WNA, s_x_window_geometry_set);
  swn = CAR(args); args = CDR(args);
  ASRTER(NIMP(swn) && WINDOWP(swn), swn, ARG1, s_x_window_geometry_set);
  xwn = WINDOW(swn);
  mask = args2wincfgs(&vlu, args);
  XConfigureWindow(xwn->dpy, xwn->p.win, mask, &vlu);
  return UNSPECIFIED;
}

SCM x_close(obj)
     SCM obj;
{
  ASRTER(NIMP(obj), obj, ARG1, s_x_close);
  if (WINDOWP(obj)) {
    Display *dpy;
    ASRTER(!(CAR((SCM)obj) & SCROOT), obj, ARG1, s_x_close);
    if (CLOSEDP(obj)) return UNSPECIFIED;
    DEFER_INTS;
    dpy = XWINDISPLAY(obj);
    free_xwindow((CELLPTR)obj);
    XFlush(dpy);
    ALLOW_INTS;
  } else {
    ASRTER(DISPLAYP(obj), obj, ARG1, s_x_close);
    DEFER_INTS;
    free_xdisplay((CELLPTR)obj);
    ALLOW_INTS;
  }
  return UNSPECIFIED;
}
SCM x_flush(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  if (NIMP(sd) && UNBNDP(si) && GCONTEXTP(sd)) {
    dspscn.dpy = XGCONDISPLAY(sd);
    XFlushGC(dspscn.dpy, XGCONTEXT(sd));
  } else {
    scm2display_screen(sd, si, &dspscn, s_x_flush);
    XFlush(dspscn.dpy);
  }
  return UNSPECIFIED;
}
			   /* Colormaps */

SCM x_create_colormap(swin, s_vis, s_alloc)
     SCM swin, s_vis, s_alloc;
{
  SCM alloc;
  int allo;
  struct xs_Window *sxw;
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_create_colormap);
  sxw = WINDOW(swin);
  ASRTER(NIMP(s_vis) && VISUALP(s_vis), s_vis, ARG2, s_x_create_colormap);
  alloc = thevalue(s_alloc);
  allo = INUM(alloc);
  ASRTER(INUMP(alloc) && (allo==AllocNone || allo==AllocAll),
	 s_alloc, ARG3, s_x_create_colormap);
  return make_xcolormap(sxw->display,
			XCreateColormap(sxw->dpy, sxw->p.win,
					XVISUAL(s_vis), allo));
}
SCM x_recreate_colormap(s_cm)
     SCM s_cm;
{
  struct xs_Colormap *sxw;
  ASRTER(NIMP(s_cm) && COLORMAPP(s_cm), s_cm, ARG1, s_x_recreate_colormap);
  sxw = COLORMAP(s_cm);
  return make_xcolormap(sxw->display,
			XCopyColormapAndFree(XDISPLAY(sxw->display), sxw->cm));
}
SCM x_install_colormap(s_cm, s_flg)
     SCM s_cm, s_flg;
{
  struct xs_Colormap *xcm;
  ASRTER(NIMP(s_cm) && COLORMAPP(s_cm), s_cm, ARG1, s_x_install_colormap);
  if (UNBNDP(s_flg)) s_flg = BOOL_T;
  xcm = COLORMAP(s_cm);
  if (FALSEP(s_flg)) XUninstallColormap(XDISPLAY(xcm->display), xcm->cm);
  XInstallColormap(XDISPLAY(xcm->display), xcm->cm);
  return UNSPECIFIED;
}
/*  SCM x_colormap_basis(svsl) */
/*       SCM svsl; */
/*  { */
/*    XColormapInfo *vsl; */
/*    ASRTER(NIMP(svsl) && COLORMAPP(svsl), svsl, ARG1, s_x_colormap_basis); */
/*    vsl = XCOLORMAPINFO(svsl); */
/*    return cons2(vsl->red_mult, vsl->green_mult, */
/*	       cons2(vsl->blue_mult, vsl->base_pixel, EOL)); */
/*  } */
/*  SCM x_colormap_limits(svsl) */
/*       SCM svsl; */
/*  { */
/*    XColormapInfo *vsl; */
/*    ASRTER(NIMP(svsl) && COLORMAPP(svsl), svsl, ARG1, s_x_colormap_limits); */
/*    vsl = XCOLORMAPINFO(svsl); */
/*    return cons2(vsl->red_mult, vsl->green_mult, */
/*	       cons2(vsl->blue_mult, vsl->base_pixel, EOL)); */
/*  } */

		       /* Colors in Colormap */

SCM x_alloc_color_cells(scmap, spxls, sargs)
     SCM scmap, spxls, sargs;
{
  XColor xclr;
  Status sts;
  struct xs_Colormap *xcm;
  Bool contig = 0;
  SCM pxra, plra;
  unsigned int npixels, nplanes;
  ASRTER(NIMP(scmap) && COLORMAPP(scmap), scmap, ARG1, s_x_alloc_color_cells);
  xcm = COLORMAP(scmap);
  npixels = INUM(spxls);
  ASRTER(INUMP(spxls) && npixels > 0, spxls, ARG2, s_x_alloc_color_cells);
  pxra = make_uve(npixels, MAKINUM(32L)); /* Uniform vector of long */
  switch (ilength(sargs) + 2) {
  default: wta(sargs, (char *)WNA, s_x_alloc_color_cells);
  case 3: case 4:
    if (scm2XColor(CAR(sargs), &xclr)) {
      unsigned long rmask_return, gmask_return, bmask_return;
      sargs = CDR(sargs);
      if (NNULLP(sargs)) contig = thebool(CAR(sargs), s_x_alloc_color_cells);
      sts = XAllocColorPlanes(xcm->dpy, xcm->cm, contig,
			      VELTS(pxra), npixels,
			      xclr.red, xclr.green, xclr.blue,
			      &rmask_return, &gmask_return, &bmask_return);
      if (!sts) return BOOL_F;
      return cons2(pxra, MAKINUM(rmask_return),
		   cons2(MAKINUM(gmask_return),
			 MAKINUM(bmask_return), EOL));
    }
    nplanes = theuint(CAR(sargs), s_x_alloc_color_cells);
    sargs = CDR(sargs);
    if (NNULLP(sargs)) contig = thebool(CAR(sargs), s_x_alloc_color_cells);
    plra = make_uve(nplanes, MAKINUM(32L)); /* Uniform vector of long */
    sts = XAllocColorCells(xcm->dpy, xcm->cm, contig,
			   VELTS(plra), nplanes, VELTS(pxra), npixels);
    if (!sts) return BOOL_F;
    return cons2(pxra, plra, EOL);
  }
}
SCM x_free_color_cells(scmap, spxls, sargs)
     SCM scmap, spxls, sargs;
{
  struct xs_Colormap *xcm;
  unsigned int planes = 0;
  ASRTER(NIMP(scmap) && COLORMAPP(scmap), scmap, ARG1, s_x_free_color_cells);
  xcm = COLORMAP(scmap);
  ASRTER(NIMP(spxls) && (TYP7(spxls)==tc7_VfixN32), spxls, ARG2,
	 s_x_free_color_cells);
  switch (ilength(sargs) + 2) {
  default: wta(sargs, (char *)WNA, s_x_free_color_cells);
  case 4:
    planes = theuint(CAR(sargs), s_x_free_color_cells);
  case 3:
    XFreeColors(xcm->dpy, xcm->cm, VELTS(spxls), INUM(spxls), planes);
    return UNSPECIFIED;
  }
}

SCM x_find_color(scmap, dat)
     SCM scmap, dat;
{
  XColor xclr;
  struct xs_Colormap *xcm;
  (void)memset((char *)&xclr, 0, sizeof(xclr));
  ASRTER(NIMP(scmap) && COLORMAPP(scmap), scmap, ARG1, s_x_find_color);
  xcm = COLORMAP(scmap);
  if (!scm2XColor(dat, &xclr)) {
    ASRTER(NIMP(dat) && STRINGP(dat), dat, (char*)ARG2, s_x_find_color);
    if (XAllocNamedColor(xcm->dpy, xcm->cm, CHARS(dat), &xclr, &xclr))
      return MAKINUM(xclr.pixel);
    else return BOOL_F;
  }
  if (XAllocColor(xcm->dpy, xcm->cm, &xclr))
    return MAKINUM(xclr.pixel);
  else return BOOL_F;
}
SCM x_color_set(scmap, s_pix, dat)
     SCM scmap, s_pix, dat;
{
  XColor xclr;
  struct xs_Colormap *xcm;
  (void)memset((char *)&xclr, 0, sizeof(xclr));
  ASRTER(NIMP(scmap) && COLORMAPP(scmap), scmap, ARG1, s_x_color_set);
  ASRTER(INUMP(s_pix), s_pix, ARG2, s_x_color_set);
  xcm = COLORMAP(scmap);
  xclr.pixel = INUM(s_pix);
  xclr.flags = DoRed | DoGreen | DoBlue;
  if (!scm2XColor(dat, &xclr)) {
    ASRTER(NIMP(dat) && STRINGP(dat), dat, (char*)ARG3, s_x_color_set);
    XStoreNamedColor(xcm->dpy, xcm->cm, CHARS(dat), xclr.pixel, xclr.flags);
  }
  else XStoreColor(xcm->dpy, xcm->cm, &xclr);
  return UNSPECIFIED;
}
SCM x_color_ref(scmap, sidx)
     SCM scmap, sidx;
{
  XColor xclr;
  struct xs_Colormap *xcm;
  (void)memset((char *)&xclr, 0, sizeof(xclr));
  ASRTER(NIMP(scmap) && COLORMAPP(scmap), scmap, ARG1, s_x_color_ref);
  xcm = COLORMAP(scmap);
  ASRTER(INUMP(sidx), sidx, (char*)ARG2, s_x_color_ref);
  xclr.pixel = INUM(sidx);
  XQueryColor(xcm->dpy, xcm->cm, &xclr);
  if (xclr.flags==(DoRed | DoGreen | DoBlue))
    return cons2(MAKINUM(xclr.red), MAKINUM(xclr.green),
		 cons(MAKINUM(xclr.blue), EOL));
  else return BOOL_F;
}

			 /* Window Mapping */

SCM x_map_window(swin)
     SCM swin;
{
  struct xs_Window *w;
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_map_window);
  w = WINDOW(swin);
  XMapWindow(w->dpy, w->p.win);
  return UNSPECIFIED;
}
SCM x_map_subwindows(swin)
     SCM swin;
{
  struct xs_Window *w;
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_map_subwindows);
  w = WINDOW(swin);
  XMapSubwindows(w->dpy, w->p.win);
  return UNSPECIFIED;
}
SCM x_unmap_window(swin)
     SCM swin;
{
  struct xs_Window *w;
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_unmap_window);
  w = WINDOW(swin);
  XUnmapWindow(w->dpy, w->p.win);
  return UNSPECIFIED;
}
SCM x_unmap_subwindows(swin)
     SCM swin;
{
  struct xs_Window *w;
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_unmap_subwindows);
  w = WINDOW(swin);
  XUnmapSubwindows(w->dpy, w->p.win);
  return UNSPECIFIED;
}

SCM x_create_gc(args)
     SCM args;
{
  SCM swin;
  struct xs_Window *xsw;
  struct xs_GContext *xgc;
  XGCValues v;
  unsigned long mask;
  SCM ans;

  ASRTER(NIMP(args), args, WNA, s_x_create_gc);
  swin = CAR(args); args = CDR(args);
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_create_gc);
  xsw = WINDOW(swin);
  ans = make_xgcontext(xsw->display, xsw->screen_number,
		       XCreateGC(xsw->dpy, xsw->p.drbl, 0L, &v), 0);
  xgc = GCONTEXT(ans);
  mask = args2xgcvalues(ans, &v, args);
  XChangeGC(xgc->dpy, xgc->gc, mask, &v);
  return ans;
}
SCM x_gc_set(args)
     SCM args;
{
  SCM sgc;
  struct xs_GContext *xgc;
  XGCValues v;
  unsigned long mask;

  ASRTER(NIMP(args), args, WNA, s_x_gc_set);
  sgc = CAR(args); args = CDR(args);
  ASRTER(NIMP(sgc) && GCONTEXTP(sgc), sgc, ARG1, s_x_gc_set);
  xgc = GCONTEXT(sgc);
  mask = args2xgcvalues(sgc, &v, args);
  XChangeGC(xgc->dpy, xgc->gc, mask, &v);
  return UNSPECIFIED;
}
SCM x_copy_gc(dst, src, args)
     SCM dst;
     SCM src;
     SCM args;
{
  struct xs_GContext *dgc, *sgc;
  unsigned long mask;

  ASRTER(NIMP(dst) && GCONTEXTP(dst), dst, ARG1, s_x_copy_gc);
  ASRTER(NIMP(src) && GCONTEXTP(src), src, ARG2, s_x_copy_gc);
  dgc = GCONTEXT(dst);
  sgc = GCONTEXT(src);
  mask = args2valmask(args, s_gc);
  XCopyGC(dgc->dpy, sgc->gc, mask, dgc->gc);
  return UNSPECIFIED;
}
SCM x_gc_ref(oargs)
     SCM oargs;
{
  SCM sgc, args = oargs, sval = BOOL_F;
  SCM vals = cons(BOOL_T, EOL), valend = vals;
  struct xs_GContext *xgc;
  unsigned long valuemask;
  XGCValues vlu;
  int attr, len = ilength(args);
/*    (void)memset((char *)&vlu, 0, sizeof(XGCValues)); */
  ASRTER(len > 0, oargs, WNA, s_x_gc_ref);
  if (1==len--) return EOL;
  sgc = CAR(args); args = CDR(args);
  ASRTER(NIMP(sgc) && GCONTEXTP(sgc), sgc, ARG1, s_x_gc_ref);
  xgc = GCONTEXT(sgc);
  valuemask = args2valmask(args, s_gc);
/*    printf("valuemask = %lx\n", valuemask); */
  valuemask &= (GCFunction | GCPlaneMask | GCForeground | GCBackground |
		GCLineWidth | GCLineStyle | GCCapStyle | GCJoinStyle |
		GCFillStyle | GCFillRule |
		GCTileStipXOrigin | GCTileStipYOrigin |
		GCSubwindowMode | GCGraphicsExposures |
		GCClipXOrigin | GCClipYOrigin | GCDashOffset | GCArcMode);
  if (!XGetGCValues(xgc->dpy, xgc->gc, valuemask, &vlu)) return BOOL_F;
  while (len) {
    attr = theint(CAR(args), s_gc); args = CDR(args);
    switch (attr) {

    case GCFunction:	sval = MAKINUM(vlu.function  ); break;
    case GCPlaneMask:	sval = MAKINUM(vlu.plane_mask); break;
    case GCForeground:	sval = MAKINUM(vlu.foreground); break;
    case GCBackground:	sval = MAKINUM(vlu.background); break;
    case GCLineWidth:	sval = MAKINUM(vlu.line_width); break;
    case GCLineStyle:	sval = MAKINUM(vlu.line_style); break;
    case GCCapStyle:	sval = MAKINUM(vlu.cap_style ); break;
    case GCJoinStyle:	sval = MAKINUM(vlu.join_style); break;
    case GCFillStyle:	sval = MAKINUM(vlu.fill_style); break;
    case GCFillRule:	sval = MAKINUM(vlu.fill_rule ); break;
    case GCTile:	sval = xgc->tile; break;
    case GCStipple:	sval = xgc->stipple; break;
    case GCTileStipXOrigin: sval = MAKINUM(vlu.ts_x_origin); break;
    case GCTileStipYOrigin: sval = MAKINUM(vlu.ts_y_origin); break;
    case (GCTileStipXOrigin | GCTileStipYOrigin):
      sval = cons2(MAKINUM(vlu.ts_x_origin), MAKINUM(vlu.ts_y_origin), EOL);
      break;
    case GCFont:	sval = xgc->font; break;
    case GCSubwindowMode: sval = MAKINUM(vlu.subwindow_mode); break;
    case GCGraphicsExposures:
      sval = x_make_bool(vlu.graphics_exposures); break;
    case GCClipXOrigin:	sval = MAKINUM(vlu.clip_x_origin); break;
    case GCClipYOrigin:	sval = MAKINUM(vlu.clip_y_origin); break;
    case (GCClipXOrigin | GCClipYOrigin):
      sval = cons2(MAKINUM(vlu.clip_x_origin),
		    MAKINUM(vlu.clip_y_origin), EOL);
      break;
    case GCClipMask:	sval = xgc->clipmask; break;
    case GCDashOffset:	sval = MAKINUM(vlu.dash_offset); break;
    case GCDashList:	sval = MAKINUM(vlu.dashes); break;
    case GCArcMode:	sval = MAKINUM(vlu.arc_mode); break;

    default: ASRTER(0, MAKINUM(attr), ARGn, s_x_gc_ref);
    }
    CAR(valend) = sval;
    CDR(valend) = cons(BOOL_T, EOL);
    len -= 1;
    if (len) valend = CDR(valend);
    else CDR(valend) = EOL;
  }
  return vals;
}

SCM x_create_cursor(sdpy, scsr, sargs)
     SCM sdpy, scsr, sargs;
{
  Cursor cursor;

  switch (ilength(sargs)) {
  default: ASRTER(0, sargs, WNA, s_x_create_cursor);
  case 0: {
    SCM shape;
    ASRTER(NIMP(sdpy) && DISPLAYP(sdpy), sdpy, ARG1, s_x_create_cursor);
    shape = thevalue(scsr);
    ASRTER(INUMP(shape) && 0 <= INUM(shape), scsr, ARG2, s_x_create_cursor);
    cursor = XCreateFontCursor(XDISPLAY(sdpy), INUM(shape));
    return make_xcursor(sdpy, cursor);
  }
  case 3: {
    XColor foreground_color, background_color;
    XPoint origin;
    int sts;
    ASRTER(NIMP(sdpy) && WINDOWP(sdpy), sdpy, ARG1, s_x_create_cursor);
    ASRTER(FALSEP(scsr) || (NIMP(scsr) && WINDOWP(scsr)), scsr, ARG2,
	   s_x_create_cursor);
    sts = scm2XColor(CAR(sargs), &foreground_color);
    ASRTER(sts, CAR(sargs), ARG3, s_x_create_cursor);
    sargs = CDR(sargs);
    sts = scm2XColor(CAR(sargs), &background_color);
    ASRTER(sts, CAR(sargs), ARG4, s_x_create_cursor);
    sargs = CDR(sargs);
    scm2XPoint(0, CAR(sargs), &origin, (char*)ARG5, s_x_create_cursor);
    cursor = XCreatePixmapCursor(XWINDISPLAY(sdpy), XWINDOW(sdpy),
				 FALSEP(scsr) ? 0L : XWINDOW(scsr),
				 &foreground_color, &background_color,
				 origin.x, origin.y);
    return make_xcursor(WINDOW(sdpy)->display, cursor);
  }
  case 4: {
    XColor foreground_color, background_color;
    Font source_font, mask_font = 0;
    unsigned int source_char, mask_char = 0;
    int sts;
    source_font = thefont(sdpy, s_x_create_cursor);
    GET_NEXT_INT(source_char, sargs, ARG2, s_x_create_cursor);
    if (FALSEP(CAR(sargs))) {
      sargs = CDR(sargs);
      ASRTER(FALSEP(CAR(sargs)), sargs, ARG4, s_x_create_cursor);
      sargs = CDR(sargs);
    } else {
      mask_font = thefont(CAR(sargs), s_x_create_cursor);
      sargs = CDR(sargs);
      GET_NEXT_INT(mask_char, sargs, ARG4, s_x_create_cursor);
    }
    sts = scm2XColor(CAR(sargs), &foreground_color);
    ASRTER(sts, CAR(sargs), ARG5, s_x_create_cursor);
    sargs = CDR(sargs);
    sts = scm2XColor(CAR(sargs), &background_color);
    ASRTER(sts, CAR(sargs), ARGn, s_x_create_cursor);
    cursor = XCreateGlyphCursor(XWINDISPLAY(sdpy),
				source_font, mask_font, source_char, mask_char,
				&foreground_color, &background_color);
    return make_xcursor(FONT(sdpy)->display, cursor);
  }}
}

SCM x_load_font(sdpy, fntnam)
     SCM sdpy, fntnam;
{
  Font font;

  ASRTER(NIMP(sdpy) && DISPLAYP(sdpy), sdpy, ARG1, s_x_load_font);
  ASRTER(NIMP(fntnam) && STRINGP(fntnam), fntnam, ARG2, s_x_load_font);
  font = XLoadFont(XDISPLAY(sdpy), CHARS(fntnam));
  return make_xfont(sdpy, font, fntnam);
}

		  /* Xlib information functions. */

SCM x_protocol_version(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_protocol_version);
  return cons(MAKINUM(ProtocolVersion(dspscn.dpy)),
	      MAKINUM(ProtocolRevision(dspscn.dpy)));
}
SCM x_server_vendor(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_server_vendor);
  return makfrom0str(ServerVendor(dspscn.dpy));
}
SCM x_vendor_release(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_vendor_release);
  return MAKINUM(VendorRelease(dspscn.dpy));
}
int x_scm_error_handler(display, xee)
     Display *display;
     XErrorEvent *xee;
{
  char buffer_return[1024];
  fflush(stdout);
  XGetErrorText(display, xee->error_code, buffer_return, sizeof buffer_return);
  *loc_errobj = MAKINUM((xee->request_code<<8) + xee->minor_code);
  fputs(buffer_return, stderr);
  fputc('\n', stderr);
  fflush(stderr);
  return 0;
}
SCM x_q_length(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_q_length);
  return MAKINUM(QLength(dspscn.dpy));
}
SCM x_pending(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_pending);
  return MAKINUM(XPending(dspscn.dpy));
}
SCM x_events_queued(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_events_queued);
  return MAKINUM(XEventsQueued(dspscn.dpy, QueuedAfterReading));
}
SCM x_next_event(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  XEvent event_return;
  scm2display_screen(sd, si, &dspscn, s_x_next_event);
  XNextEvent(dspscn.dpy, &event_return);
  return make_xevent(&event_return);
}
SCM x_peek_event(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  XEvent event_return;
  scm2display_screen(sd, si, &dspscn, s_x_peek_event);
  XPeekEvent(dspscn.dpy, &event_return);
  return make_xevent(&event_return);
}
		  /* Screen information functions */

SCM x_screen_count(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_screen_count);
  return MAKINUM(ScreenCount(dspscn.dpy));
}
SCM x_screen_cells(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_screen_cells);
  return MAKINUM(DisplayCells(dspscn.dpy, dspscn.screen_number));
}
SCM x_screen_depth(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  if (UNBNDP(si) && NIMP(sd) && VISUALP(sd))
    return MAKINUM(XVISUALINFO(sd)->depth);
  scm2display_screen(sd, si, &dspscn, s_x_screen_depth);
  return MAKINUM(DisplayPlanes(dspscn.dpy, dspscn.screen_number));
}
SCM x_screen_depths(sd, si)
     SCM sd, si;
{
  int count_return = 0;
  int *depths;
  SCM depra;
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_screen_depths);
  depths = XListDepths(dspscn.dpy, dspscn.screen_number, &count_return);
  if (!depths) return BOOL_F;
  depra = make_uve(count_return, MAKINUM(32L)); /* Uniform vector of long */
  for (;count_return--;) VELTS(depra)[count_return] = depths[count_return];
  XFree(depths);
  return depra;
}
SCM x_screen_size(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_screen_size);
  return cons2(MAKINUM(DisplayWidth(dspscn.dpy, dspscn.screen_number)),
	       MAKINUM(DisplayHeight(dspscn.dpy, dspscn.screen_number)),
	       EOL);
}
SCM x_screen_dimm(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  scm2display_screen(sd, si, &dspscn, s_x_screen_dimm);
  return cons2(MAKINUM(DisplayWidthMM(dspscn.dpy, dspscn.screen_number)),
	       MAKINUM(DisplayHeightMM(dspscn.dpy, dspscn.screen_number)),
	       EOL);
}
SCM x_screen_black(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  Screen *scn;
  scm2display_screen(sd, si, &dspscn, s_x_screen_black);
  scn = ScreenOfDisplay(dspscn.dpy, dspscn.screen_number);
  return ulong2num(BlackPixelOfScreen(scn));
}
SCM x_screen_white(sd, si)
     SCM sd, si;
{
  struct display_screen dspscn;
  Screen *scn;
  scm2display_screen(sd, si, &dspscn, s_x_screen_white);
  scn = ScreenOfDisplay(dspscn.dpy, dspscn.screen_number);
  return ulong2num(WhitePixelOfScreen(scn));
}

XVisualInfo *visual2visualinfo(dsp, vis)
     Display *dsp;
     Visual *vis;
{
  int nitems_return;
  XVisualInfo vinfo_template;
  XVisualInfo *vislst;
  vinfo_template.visualid = XVisualIDFromVisual(vis);
  vislst = XGetVisualInfo(dsp, VisualIDMask, &vinfo_template, &nitems_return);
  if (1 != nitems_return) {
    if (vislst) XFree(vislst);
    wta(MAKINUM(nitems_return), (char *)WNA, s_visual);
  }
  return vislst;
}
SCM x_make_visual(sd, sdepth, sclass)
     SCM sd, sdepth, sclass;
{
  int nitems_return;
  struct display_screen dspscn;
  XVisualInfo vinfo_template;
  XVisualInfo *vislst;
  scm2display_screen(sd, UNDEFINED, &dspscn, s_x_make_visual);
  vinfo_template.screen = dspscn.screen_number;
  vinfo_template.depth = theuint(sdepth, s_x_make_visual);
  vinfo_template.class = theuint(sclass, s_x_make_visual);
  vislst =
    XGetVisualInfo(dspscn.dpy,
		   VisualScreenMask | VisualDepthMask | VisualClassMask,
		   &vinfo_template,
		   &nitems_return);
  if (0==nitems_return) return BOOL_F;
  return make_xvisual(vislst);
}
static sizet free_visual(ptr)
     CELLPTR ptr;
{
  XFree(XVISUALINFO(ptr));
  return 0;
}
SCM x_visual_geometry(svsl)
     SCM svsl;
{
  XVisualInfo *vsl;
  ASRTER(NIMP(svsl) && VISUALP(svsl), svsl, ARG1, s_x_visual_geometry);
  vsl = XVISUALINFO(svsl);
  return cons2(MAKINUM(vsl->red_mask), MAKINUM(vsl->green_mask),
	       cons2(MAKINUM(vsl->blue_mask), MAKINUM(vsl->colormap_size),
		     EOL));
}
SCM x_visual_class(svsl)
     SCM svsl;
{
  XVisualInfo *vsl;
  ASRTER(NIMP(svsl) && VISUALP(svsl), svsl, ARG1, s_x_visual_class);
  vsl = XVISUALINFO(svsl);
  return MAKINUM(vsl->class);
}

SCM x_root_window(sdpy, sscr)
     SCM sdpy, sscr;
{
  struct display_screen dspscn;
  struct xs_Display *xsd;
  struct xs_screen *scrns;
  scm2display_screen(sdpy, sscr, &dspscn, s_x_root_window);
  xsd = DISPLAY(dspscn.display);
  scrns = (struct xs_screen *)(xsd + 1);
  return scrns[dspscn.screen_number].root_window;
}
SCM x_default_colormap(sdpy, sscr)
     SCM sdpy, sscr;
{
  struct display_screen dspscn;
  struct xs_Display *xsd;
  struct xs_screen *scrns;
  scm2display_screen(sdpy, sscr, &dspscn, s_x_default_colormap);
  xsd = DISPLAY(dspscn.display);
  scrns = (struct xs_screen *)(xsd + 1);
  return scrns[dspscn.screen_number].default_colormap;
}
SCM x_default_gcontext(sdpy, sscr)
     SCM sdpy, sscr;
{
  struct display_screen dspscn;
  struct xs_Display *xsd;
  struct xs_screen *scrns;
  scm2display_screen(sdpy, sscr, &dspscn, s_x_default_gcontext);
  xsd = DISPLAY(dspscn.display);
  scrns = (struct xs_screen *)(xsd + 1);
  return scrns[dspscn.screen_number].default_gcontext;
}
SCM x_default_visual(sdpy, sscr)
     SCM sdpy, sscr;
{
  struct display_screen dspscn;
  struct xs_Display *xsd;
  struct xs_screen *scrns;
  scm2display_screen(sdpy, sscr, &dspscn, s_x_default_visual);
  xsd = DISPLAY(dspscn.display);
  scrns = (struct xs_screen *)(xsd + 1);
  return scrns[dspscn.screen_number].default_visual;
}
SCM x_default_ccc(sdpy, sscr)
     SCM sdpy, sscr;
{
  struct display_screen dspscn;
  XcmsCCC ccc;
  if (NIMP(sdpy) && COLORMAPP(sdpy) && UNBNDP(sscr)) {
    struct xs_Colormap *cmp = COLORMAP(sdpy);
    ccc = XcmsCCCOfColormap(cmp->dpy, cmp->cm);
  }
  else {
    scm2display_screen(sdpy, sscr, &dspscn, s_x_default_ccc);
    ccc = XcmsDefaultCCC(dspscn.dpy, dspscn.screen_number);
  }
  return CCC2SCM(ccc);
}
/*
SCM x_ccc_screen_info(sccc, sfmt)
     SCM sccc;
     SCM sfmt;
{
  XcmsCCC xccc;
  XcmsPerScrnInfo *pPerScrnInfo;
  ASRTER(NIMP(sccc) && CCCP(sccc), sccc, ARG1, s_x_ccc_screen_info);
  ASRTER(NIMP(sfmt) && STRINGP(sfmt), sfmt, ARG2, s_x_ccc_screen_info);
  xccc = XCCC(sccc);
  pPerScrnInfo = (XcmsFunctionSet *)xccc->pPerScrnInfo;
  return ;
}
*/
		       /* Window Information */

SCM x_propdata2scm(type, format, nitems, data)
     Atom type;
     int format;
     unsigned long nitems;
     unsigned char* data;
{
  SCM datum = EOL;
  SCM lst = EOL;
  int cnt;
  for (cnt = nitems; cnt--;) {
    switch (type) {
    case XA_ATOM:
    case XA_VISUALID:
    case XA_CARDINAL:
      switch (format) {
      case 8: datum = MAKINUM(((unsigned char *)data)[cnt]); break;
      case 16: datum = MAKINUM(((unsigned short *)data)[cnt]); break;
      case 32: datum = ulong2num(((unsigned long *)data)[cnt]); break;
      default: return MAKINUM(format);
      } break;
    case XA_INTEGER:
      switch (format) {
      case 8: datum = MAKINUM(((char *)data)[cnt]); break;
      case 16: datum = MAKINUM(((short *)data)[cnt]); break;
      case 32: datum = long2num(((long *)data)[cnt]); break;
      default: return MAKINUM(format);
      } break;
    case XA_STRING:
      switch (format) {
      case 8: return makfrom0str(data);
      default: return MAKINUM(format);
      } break;
    case XA_ARC:
    case XA_BITMAP:
    case XA_COLORMAP:
    case XA_CURSOR:
    case XA_DRAWABLE:
    case XA_FONT:
    case XA_PIXMAP:
    case XA_POINT:
    case XA_RECTANGLE:
    case XA_RGB_COLOR_MAP:
    case XA_WINDOW:
    case XA_WM_HINTS:
    case XA_WM_SIZE_HINTS:
    default:
      /* datum = BOOL_F; */
      return MAKINUM(-type);
    }
    lst = cons(datum, lst);
  }
  return lst;
}
SCM x_get_window_property(swin, sprop, sargs)
     SCM swin, sprop, sargs;
{
  struct xs_Window *xwn;
  Atom property;
  Atom actual_type_return;
  int actual_format_return;
  unsigned long nitems_return;
  unsigned long bytes_after_return;
  unsigned char *prop_return;
  int sarglen = ilength(sargs);
  ASRTER(IMP(sprop) ? INUMP(sprop) : STRINGP(sprop),
	 sprop, ARG2, s_x_get_window_property);
  ASRTER(sarglen >= 0 && sarglen < 2, sargs, WNA, s_x_get_window_property);
  if (1 == sarglen) {
    ASRTER(NFALSEP(booleanp(CAR(sargs))), sargs, ARG3, s_x_get_window_property);
  }
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_map_window);
  xwn = WINDOW(swin);
  if (INUMP(sprop))
    property = INUM(sprop);
  else
    property = XInternAtom(xwn->dpy, CHARS(sprop), !0);

  if (None == property) return BOOL_F;
  if (XGetWindowProperty(xwn->dpy, xwn->p.win, property, 0L, 65536L,
			 (1 == sarglen) && NFALSEP(CAR(sargs)), AnyPropertyType,
			 &actual_type_return, &actual_format_return,
			 &nitems_return, &bytes_after_return,
			 &prop_return)
      != Success)
    return BOOL_F;
  {
    SCM ans = x_propdata2scm(actual_type_return, actual_format_return,
			     nitems_return, prop_return);
    XFree(prop_return);
    return ans;
  }
}
SCM x_list_properties(swin)
     SCM swin;
{
  struct xs_Window *xwn;
  Atom *atoms;
  int num_prop_return;
  SCM lst;
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_map_window);
  xwn = WINDOW(swin);
  atoms = XListProperties(xwn->dpy, xwn->p.win, &num_prop_return);
  {
    int i = num_prop_return;
    lst = EOL;
    while (i--) {
      char *name = XGetAtomName(xwn->dpy, atoms[i]);
      lst = cons(makfrom0str(name), lst);
      XFree(name);
    }
  }
  XFree(atoms);
  return lst;
}

			   /* Rendering */

SCM x_clear_area(swin, spos, sargs)
     SCM swin, spos, sargs;
{
  XPoint position, size;
  ASRTER(2==ilength(sargs), sargs, WNA, s_x_clear_area);
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_clear_area);
  scm2XPoint(!0, spos, &position, (char *)ARG2, s_x_clear_area);
  scm2XPoint(0, CAR(sargs), &size, (char *)ARG3, s_x_clear_area);
  sargs = CDR(sargs);
  XClearArea(XWINDISPLAY(swin), XWINDOW(swin),
	     position.x, position.y, size.x, size.y,
	     NFALSEP(CAR(sargs)));
  return UNSPECIFIED;
}
SCM x_fill_rectangle(swin, sgc, sargs)
     SCM swin, sgc, sargs;
{
  XPoint position, size;
  ASRTER(2==ilength(sargs), sargs, WNA, s_x_fill_rectangle);
  ASRTER(NIMP(swin) && WINDOWP(swin), swin, ARG1, s_x_fill_rectangle);
  ASRTER(NIMP(sgc) && GCONTEXTP(sgc), sgc, ARG2, s_x_fill_rectangle);
  scm2XPoint(!0, CAR(sargs), &position, (char *)ARG3, s_x_fill_rectangle);
  sargs = CDR(sargs);
  scm2XPoint(0, CAR(sargs), &size, (char *)ARG4, s_x_fill_rectangle);
  XFillRectangle(XWINDISPLAY(swin), XWINDOW(swin), XGCONTEXT(sgc),
		 position.x, position.y, size.x, size.y);
  return UNSPECIFIED;
}

void xldraw_string(sdbl, sgc, sargs, proc, s_caller)
     SCM sdbl, sgc, sargs;
     int (*proc)();
     char *s_caller;
{
  XPoint position;
  ASRTER(2==ilength(sargs), sargs, WNA, s_caller);
  ASRTER(NIMP(sdbl) && WINDOWP(sdbl), sdbl, ARG1, s_caller);
  ASRTER(NIMP(sgc) && GCONTEXTP(sgc), sgc, ARG2, s_caller);
  scm2XPoint(!0, CAR(sargs), &position, (char *)ARG3, s_caller);
  sargs = CDR(sargs);
  sargs = CAR(sargs);
  ASRTER(NIMP(sargs) && STRINGP(sargs), sargs, ARG4, s_caller);
  proc(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
       position.x, position.y, CHARS(sargs), LENGTH(sargs));
}
SCM x_draw_string(sdbl, sgc, sargs)
     SCM sdbl, sgc, sargs;
{
  xldraw_string(sdbl, sgc, sargs, &XDrawString, s_x_draw_string);
  return UNSPECIFIED;
}
SCM x_image_string(sdbl, sgc, sargs)
     SCM sdbl, sgc, sargs;
{
  xldraw_string(sdbl, sgc, sargs, &XDrawImageString, s_x_image_string);
  return UNSPECIFIED;
}

SCM x_draw_points(sdbl, sgc, sargs)
     SCM sdbl, sgc, sargs;
{
  XPoint pos[1];
  int len;
  SCM sarg;
  ASRTER(NIMP(sdbl) && WINDOWP(sdbl), sdbl, ARG1, s_x_draw_points);
  ASRTER(NIMP(sgc) && GCONTEXTP(sgc), sgc, ARG2, s_x_draw_points);
 loop:
  if (NULLP(sargs)) return UNSPECIFIED;
  sarg = CAR(sargs); sargs = CDR(sargs);
  if (INUMP(sarg)) {
    ASRTER(NNULLP(sargs), sargs, WNA, s_x_draw_points);
    pos[0].x = INUM(sarg);
    GET_NEXT_INT(pos[0].y, sargs, ARGn, s_x_draw_points);
    goto drawshort;
  }
  len = scm2xpointslen(sarg, s_x_draw_points);
  if (len < 0) {
    scm2XPoint(!0, sarg, &(pos[0]), (char *)ARG3, s_x_draw_points);
  drawshort:
    XDrawPoints(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
		&(pos[0]), 1, CoordModeOrigin);
    goto loop;
  } else {
    ASRTER(NULLP(sargs), sargs, WNA, s_x_draw_points);
    XDrawPoints(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
		(XPoint *)scm_base_addr(sarg, s_x_draw_points), len,
		CoordModeOrigin);
    return UNSPECIFIED;
  }
}
SCM xldraw_lines(sdbl, sgc, sargs, funcod, s_caller)
     SCM sdbl, sgc, sargs;
     int funcod;
     char *s_caller;
{
  XPoint pos[2];
  int len;
  SCM sarg;
  ASRTER(NIMP(sdbl) && WINDOWP(sdbl), sdbl, ARG1, s_caller);
  ASRTER(NIMP(sgc) && GCONTEXTP(sgc), sgc, ARG2, s_caller);
 loop:
  if (NULLP(sargs)) return UNSPECIFIED;
  sarg = CAR(sargs); sargs = CDR(sargs);
  if (INUMP(sarg)) {
    ASRTER(NNULLP(sargs), sargs, WNA, s_caller);
    pos[0].x = INUM(sarg);
    GET_NEXT_INT(pos[0].y, sargs, ARGn, s_caller);
    GET_NEXT_INT(pos[1].x, sargs, ARGn, s_caller);
    GET_NEXT_INT(pos[1].y, sargs, ARGn, s_caller);
    goto drawshort;
  }
  len = scm2xpointslen(sarg, s_caller);
  if (len < 0) {
    scm2XPoint(!0, sarg, &(pos[0]), (char *)ARG3, s_caller);
    scm2XPoint(!0, sarg, &(pos[1]), (char *)ARG4, s_caller);
  drawshort:
    switch (funcod) {
    default: wna: wta(sargs, (char *)WNA, s_caller);
    case 0:
      XDrawSegments(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
		    (XSegment *) &(pos[0]), 1);
      goto loop;
    case 1:
      XDrawLines(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
		 &(pos[0]), 2, CoordModeOrigin);
      goto loop;
    }
  } else {
    void* rabase;
    ASRTGO(NULLP(sargs), wna);
    rabase = scm_base_addr(sarg, s_caller);
    switch (funcod) {
    default: goto wna;
    case 0:
      XDrawSegments(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
		    (XSegment *)rabase, len/2);
      return UNSPECIFIED;
    case 1:
      XDrawLines(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
		 (XPoint *)rabase, len, CoordModeOrigin);
      return UNSPECIFIED;
    case 2:
      XFillPolygon(XWINDISPLAY(sdbl), XWINDOW(sdbl), XGCONTEXT(sgc),
		   (XPoint *)rabase, len, Complex, CoordModeOrigin);
      return UNSPECIFIED;
    }
  }
}

SCM x_draw_segments(sdbl, sgc, sargs)
     SCM sdbl, sgc, sargs;
{
  return xldraw_lines(sdbl, sgc, sargs, 0, s_x_draw_segments);
}
SCM x_draw_lines(sdbl, sgc, sargs)
     SCM sdbl, sgc, sargs;
{
  return xldraw_lines(sdbl, sgc, sargs, 1, s_x_draw_lines);
}
SCM x_fill_poly(sdbl, sgc, sargs)
     SCM sdbl, sgc, sargs;
{
  return xldraw_lines(sdbl, sgc, sargs, 2, s_x_fill_poly);
}

static char s_x_read_bitmap_file[]      = "x:read-bitmap-file";
SCM x_read_bitmap_file(sdbl, sfname)
     SCM sdbl, sfname;
{
  unsigned int w, h;
  int x, y;
  Pixmap pxmp;
  ASRTER(NIMP(sdbl) && WINDOWP(sdbl), sdbl, ARG1, s_x_read_bitmap_file);
  if (XReadBitmapFile(XWINDISPLAY(sdbl),
		      WINDOW(sdbl)->p.pm,
		      CHARS(sfname),
		      &w, &h, &pxmp, &x, &y) == BitmapSuccess)
    return make_xwindow(WINDOW(sdbl)->display,
			WINDOW(sdbl)->screen_number,
			pxmp, (char) 1, (char) 0);
  else return BOOL_F;
}

			    /* XEvents */

/* x_make_bool() is used in xevent.h */
SCM x_make_bool(f)
     Bool f;
{
  return f ? BOOL_F : BOOL_T;
}

SCM x_event_ref(sevent, sfield)
     SCM sevent, sfield;
{
  void *x;
  ASRTER(NIMP(sevent) && XEVENTP(sevent), sevent, ARG1, s_x_event_ref);
  ASRTER(INUMP(sfield), sfield, ARG2, s_x_event_ref);
  x = (void *) CHARS(sevent);
  switch (((((XEvent*)x)->type)<<8)+INUM(sfield)) {
    default: wta(sevent, "Incompatible field for", s_x_event_ref);
#define SCM_EVENT_FIELDS
#include "xevent.h"
  }
}

static struct {
  int type;
  char *name;
} event_names[] = {
#undef SCM_EVENT_FIELDS
#include "xevent.h"
};

static char *x__event_name(type)
     int type;
{
  int i;
  for (i = 0; i < sizeof(event_names) / sizeof(event_names[0]); i++)
    if (type==event_names[i].type) return event_names[i].name;
  return "unknown";
}

SCM x_event_keysym(sevent)
     SCM sevent;
{
  XKeyEvent *ev;
  KeySym ans;
  ASRTGO(NIMP(sevent) && XEVENTP(sevent), badarg);
  ev = (XKeyEvent *)CHARS(sevent);
  switch (((XEvent*)ev)->type) {
  badarg:
  default: wta(sevent, (char *)ARG1, s_x_event_keysym);
  case KeyPress:
  case KeyRelease:
    ;
  }
  ans = XLookupKeysym(ev, ev->state);
  if (ans) return MAKINUM(ans);
  else return BOOL_F;
}

		      /* SMOB print routines */

static int print_xevent(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  lputs("#<X event: ", f);
  lputs(x__event_name(XEVENT(exp)->type), f);
  lputc('>', f);
  return 1;
}
static int print_xdisplay(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  if (CLOSEDP(exp)) lputs("#<closed-X display>", f);
  else {
    lputs("#<X display \"", f);
    lputs(DisplayString(XDISPLAY(exp)), f);
    lputs("\">", f);
  }
  return 1;
}
static int print_xwindow(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  lputs(CLOSEDP(exp) ? "#<closed-X " : "#<X ", f);
  lputs((CAR(exp) & PXMP) ? "pixmap #x" : "window #x", f);
  scm_intprint((long) XWINDOW(exp), 16, f);
  lputc('>', f);
  return 1;
}
static int print_xcursor(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  lputs("#<X cursor #x", f);
  scm_intprint((long) XCURSOR(exp), 16, f);
  lputc('>', f);
  return 1;
}
static int print_xfont(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  lputs("#<X font \"", f);
  lputs(CHARS((FONT(exp))->name), f);
  lputs("\">", f);
  return 1;
}
static int print_xcolormap(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  lputs("#<X colormap ID #x", f);
  scm_intprint((long) XCOLORMAP(exp), 16, f);
  lputc('>', f);
  return 1;
}
static int print_xgcontext(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  lputs("#<X graphics context, ID #x", f);
  /*  scm_intprint((long) GCONTEXT(exp)->gid, 16, f); skimu */
  scm_intprint((long) XGContextFromGC(XGCONTEXT(exp)), 16, f);
  lputc('>', f);
  return 1;
}

char *xvisualclass2name(class)
     int class;
{
  switch (class) {
  case StaticGray: return "StaticGray";
  case GrayScale: return "GrayScale";
  case StaticColor: return "StaticColor";
  case PseudoColor: return "PseudoColor";
  case TrueColor: return "TrueColor";
  case DirectColor: return "DirectColor";
  default: return "??";
  }
}

static int print_xvisual(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  XVisualInfo *xvi = XVISUALINFO(exp);
  lputs("#<X visual #x", f);
  scm_intprint((long) xvi->visualid, 16, f);
  lputs(" ", f);
  lputs(xvisualclass2name(xvi->class), f);
  lputc(' ', f);
  scm_intprint((long) xvi->depth, 10, f);
  lputc('x', f);
  scm_intprint((long) xvi->colormap_size, 10, f);
  lputc('>', f);
  return 1;
}
static int print_xccc(exp, f, writing)
     SCM exp;
     SCM f;
     int writing;
{
  XcmsColorSpace **papColorSpaces;
  XcmsCCC xccc = XCCC(exp);
  lputs("#<X CCC", f);
  papColorSpaces =
    ((XcmsFunctionSet *)xccc->pPerScrnInfo->functionSet)->DDColorSpaces;
  if (papColorSpaces != NULL) {
    while (*papColorSpaces != NULL) {
      lputs(" ", f);
      lputs((*papColorSpaces)->prefix, f);
      papColorSpaces++;
    }
  }

  lputc('>', f);
  return 1;
}

static smobfuns smob_xdisplay	= {mark_xdisplay, free_xdisplay, print_xdisplay, 0};
static smobfuns smob_xwindow	= {mark_xwindow, free_xwindow, print_xwindow, 0};
static smobfuns smob_xcursor	= {mark_xcursor, free_xcursor, print_xcursor, 0};
static smobfuns smob_xfont	= {mark_xfont, free_xfont, print_xfont, 0};
static smobfuns smob_xgcontext	= {mark_xgcontext, free_xgcontext, print_xgcontext, 0};
static smobfuns smob_xcolormap	= {mark_xcolormap, free_xcolormap, print_xcolormap, 0};
static smobfuns smob_xvisual	= {mark0, free_visual, print_xvisual, 0};
static smobfuns smob_xccc	= {mark0, free_xccc, print_xccc, 0};
static smobfuns smob_xevent	= {mark0, x_free_xevent, print_xevent, 0};

static iproc x_subr3s[] = {
  {s_x_make_visual,		x_make_visual},
  {s_x_create_pixmap,		x_create_pixmap},
  {s_x_create_colormap,		x_create_colormap},
  {s_x_color_set,		x_color_set},
  {0, 0}
};

static iproc x_lsubr2s[] = {
  {s_x_create_window,		x_create_window},
  {s_x_create_cursor,		x_create_cursor},
  {s_x_alloc_color_cells,	x_alloc_color_cells},
  {s_x_free_color_cells,	x_free_color_cells},
  {s_x_get_window_property,	x_get_window_property},
  {s_x_clear_area,		x_clear_area},
  {s_x_fill_rectangle,		x_fill_rectangle},
  {s_x_draw_string,		x_draw_string},
  {s_x_image_string,		x_image_string},
  {s_x_draw_points,		x_draw_points},
  {s_x_draw_segments,		x_draw_segments},
  {s_x_draw_lines,		x_draw_lines},
  {s_x_fill_poly,		x_fill_poly},
  {0, 0}
};

static iproc x_lsubrs[] = {
  {s_x_create_gc,		x_create_gc},
  {s_x_gc_set,			x_gc_set},
  {s_x_gc_ref,			x_gc_ref},
  {s_x_copy_gc,			x_copy_gc},
  {s_x_window_set,		x_window_set},
  {s_x_window_geometry_set,	x_window_geometry_set},
  {s_x_window_ref,		x_window_ref},
  {0, 0}
};

static iproc x_subr2s[] = {
  {s_x_event_ref,		x_event_ref},
  {s_x_find_color,		x_find_color},
  {s_x_color_ref,		x_color_ref},
  {s_x_load_font,		x_load_font},
  {s_x_read_bitmap_file,        x_read_bitmap_file},
  {0, 0}
};

static iproc x_subr2os[] = {
  {s_x_display_debug,		x_display_debug},
  {s_x_screen_cells,		x_screen_cells},
  {s_x_screen_depth,		x_screen_depth},
  {s_x_screen_depths,		x_screen_depths},
  {s_x_screen_size,		x_screen_size},
  {s_x_screen_dimm,		x_screen_dimm},
  {s_x_screen_black,		x_screen_black},
  {s_x_screen_white,		x_screen_white},
  {s_x_protocol_version,	x_protocol_version},
  {s_x_vendor_release,		x_vendor_release},
  {s_x_server_vendor,		x_server_vendor},
  {s_x_screen_count,		x_screen_count},
  {s_x_events_queued,		x_events_queued},
  {s_x_next_event,		x_next_event},
  {s_x_peek_event,		x_peek_event},
  {s_x_pending,			x_pending},
  {s_x_q_length,		x_q_length},
  {s_x_root_window,		x_root_window},
  {s_x_default_gcontext,	x_default_gcontext},
  {s_x_default_visual,		x_default_visual},
  {s_x_default_colormap,	x_default_colormap},
  {s_x_install_colormap,	x_install_colormap},
  {s_x_default_ccc,		x_default_ccc},
  {s_x_flush,			x_flush},
  {0, 0}
};

static iproc x_subr1s[] = {
  {s_x_open_display,		x_open_display},
  {s_x_close,			x_close},
  {s_x_default_screen,		x_default_screen},
  {s_x_window_geometry,		x_window_geometry},
  {s_x_list_properties,		x_list_properties},
  {s_x_map_window,		x_map_window},
  {s_x_map_subwindows,		x_map_subwindows},
  {s_x_unmap_window,		x_unmap_window},
  {s_x_unmap_subwindows,	x_unmap_subwindows},
  {s_x_recreate_colormap,	x_recreate_colormap},
  {s_x_visual_geometry,		x_visual_geometry},
  {s_x_visual_class,		x_visual_class},
  {s_x_event_keysym,		x_event_keysym},
/*    {s_x_colormap_basis,		x_colormap_basis}, */
/*    {s_x_colormap_limits,		x_colormap_limits}, */
  {0, 0}
};

int (*x_scm_prev_error_handler)() = 0;
void x_scm_final()
{
  if (x_scm_prev_error_handler) XSetErrorHandler(x_scm_prev_error_handler);
  x_scm_prev_error_handler = 0;
}

void init_x()
{
  init_iprocs(x_subr3s, tc7_subr_3);
  init_iprocs(x_lsubr2s, tc7_lsubr_2);
  init_iprocs(x_lsubrs, tc7_lsubr);
  init_iprocs(x_subr2s, tc7_subr_2);
  init_iprocs(x_subr2os, tc7_subr_2o);
  init_iprocs(x_subr1s, tc7_subr_1);

  tc16_xdisplay = newsmob(&smob_xdisplay);
  tc16_xwindow = newsmob(&smob_xwindow);
  tc16_xcursor = newsmob(&smob_xcursor);
  tc16_xfont = newsmob(&smob_xfont);
  tc16_xcolormap = newsmob(&smob_xcolormap);
  tc16_xgcontext = newsmob(&smob_xgcontext);
  tc16_xvisual = newsmob(&smob_xvisual);
  tc16_xevent = newsmob(&smob_xevent);
  tc16_xccc = newsmob(&smob_xccc);
  xtc_ccc = XUniqueContext();
  xtc_cmp = XUniqueContext();

  scm_ldprog("x11.scm");
  scm_ldprog("xevent.scm");
				/* Redefines STRING */
/*    scm_ldprog("xatoms.scm"); */
  scm_ldstr("\
(define x:ccc x:default-ccc)\n\
(define x:GC-Clip-Origin (logior x:GC-Clip-X-Origin x:GC-Clip-Y-Origin))\n\
(define x:GC-Tile-Stip-Origin \n\
        (logior x:GC-Tile-Stip-X-Origin x:GC-Tile-Stip-Y-Origin))\n\
");
  add_feature("xlib");

  add_final(x_scm_final);
  XSetErrorHandler(x_scm_error_handler);
}
