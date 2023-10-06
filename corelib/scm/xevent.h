/* xgen.scm extracted typedef structs from /usr/include/X11/Xlib.h */
#ifdef SCM_EVENT_FIELDS
  case (KeyPress<<8)+0x10: case (KeyRelease<<8)+0x10: return MAKINUM(((XKeyEvent *) x)->type);
  case (KeyPress<<8)+0x11: case (KeyRelease<<8)+0x11: return MAKINUM(((XKeyEvent *) x)->serial);
  case (KeyPress<<8)+0x12: case (KeyRelease<<8)+0x12: return x_make_bool(((XKeyEvent *) x)->send_event);
  case (KeyPress<<8)+0x13: case (KeyRelease<<8)+0x13: return ulong2num(((XKeyEvent *) x)->time);
  case (KeyPress<<8)+0x14: case (KeyRelease<<8)+0x14: return MAKINUM(((XKeyEvent *) x)->x);
  case (KeyPress<<8)+0x15: case (KeyRelease<<8)+0x15: return MAKINUM(((XKeyEvent *) x)->y);
  case (KeyPress<<8)+0x16: case (KeyRelease<<8)+0x16: return MAKINUM(((XKeyEvent *) x)->x_root);
  case (KeyPress<<8)+0x17: case (KeyRelease<<8)+0x17: return MAKINUM(((XKeyEvent *) x)->y_root);
  case (KeyPress<<8)+0x18: case (KeyRelease<<8)+0x18: return MAKINUM(((XKeyEvent *) x)->state);
  case (KeyPress<<8)+0x19: case (KeyRelease<<8)+0x19: return MAKINUM(((XKeyEvent *) x)->keycode);
  case (KeyPress<<8)+0x1a: case (KeyRelease<<8)+0x1a: return x_make_bool(((XKeyEvent *) x)->same_screen);
  case (ButtonPress<<8)+0x10: case (ButtonRelease<<8)+0x10: return MAKINUM(((XButtonEvent *) x)->type);
  case (ButtonPress<<8)+0x11: case (ButtonRelease<<8)+0x11: return MAKINUM(((XButtonEvent *) x)->serial);
  case (ButtonPress<<8)+0x12: case (ButtonRelease<<8)+0x12: return x_make_bool(((XButtonEvent *) x)->send_event);
  case (ButtonPress<<8)+0x13: case (ButtonRelease<<8)+0x13: return ulong2num(((XButtonEvent *) x)->time);
  case (ButtonPress<<8)+0x14: case (ButtonRelease<<8)+0x14: return MAKINUM(((XButtonEvent *) x)->x);
  case (ButtonPress<<8)+0x15: case (ButtonRelease<<8)+0x15: return MAKINUM(((XButtonEvent *) x)->y);
  case (ButtonPress<<8)+0x16: case (ButtonRelease<<8)+0x16: return MAKINUM(((XButtonEvent *) x)->x_root);
  case (ButtonPress<<8)+0x17: case (ButtonRelease<<8)+0x17: return MAKINUM(((XButtonEvent *) x)->y_root);
  case (ButtonPress<<8)+0x18: case (ButtonRelease<<8)+0x18: return MAKINUM(((XButtonEvent *) x)->state);
  case (ButtonPress<<8)+0x1b: case (ButtonRelease<<8)+0x1b: return MAKINUM(((XButtonEvent *) x)->button);
  case (ButtonPress<<8)+0x1a: case (ButtonRelease<<8)+0x1a: return x_make_bool(((XButtonEvent *) x)->same_screen);
  case (MotionNotify<<8)+0x10: return MAKINUM(((XMotionEvent *) x)->type);
  case (MotionNotify<<8)+0x11: return MAKINUM(((XMotionEvent *) x)->serial);
  case (MotionNotify<<8)+0x12: return x_make_bool(((XMotionEvent *) x)->send_event);
  case (MotionNotify<<8)+0x13: return ulong2num(((XMotionEvent *) x)->time);
  case (MotionNotify<<8)+0x14: return MAKINUM(((XMotionEvent *) x)->x);
  case (MotionNotify<<8)+0x15: return MAKINUM(((XMotionEvent *) x)->y);
  case (MotionNotify<<8)+0x16: return MAKINUM(((XMotionEvent *) x)->x_root);
  case (MotionNotify<<8)+0x17: return MAKINUM(((XMotionEvent *) x)->y_root);
  case (MotionNotify<<8)+0x18: return MAKINUM(((XMotionEvent *) x)->state);
  case (MotionNotify<<8)+0x1c: return MAKINUM(((XMotionEvent *) x)->is_hint);
  case (MotionNotify<<8)+0x1a: return x_make_bool(((XMotionEvent *) x)->same_screen);
  case (EnterNotify<<8)+0x10: case (LeaveNotify<<8)+0x10: return MAKINUM(((XCrossingEvent *) x)->type);
  case (EnterNotify<<8)+0x11: case (LeaveNotify<<8)+0x11: return MAKINUM(((XCrossingEvent *) x)->serial);
  case (EnterNotify<<8)+0x12: case (LeaveNotify<<8)+0x12: return x_make_bool(((XCrossingEvent *) x)->send_event);
  case (EnterNotify<<8)+0x13: case (LeaveNotify<<8)+0x13: return ulong2num(((XCrossingEvent *) x)->time);
  case (EnterNotify<<8)+0x14: case (LeaveNotify<<8)+0x14: return MAKINUM(((XCrossingEvent *) x)->x);
  case (EnterNotify<<8)+0x15: case (LeaveNotify<<8)+0x15: return MAKINUM(((XCrossingEvent *) x)->y);
  case (EnterNotify<<8)+0x16: case (LeaveNotify<<8)+0x16: return MAKINUM(((XCrossingEvent *) x)->x_root);
  case (EnterNotify<<8)+0x17: case (LeaveNotify<<8)+0x17: return MAKINUM(((XCrossingEvent *) x)->y_root);
  case (EnterNotify<<8)+0x1d: case (LeaveNotify<<8)+0x1d: return MAKINUM(((XCrossingEvent *) x)->mode);
  case (EnterNotify<<8)+0x1e: case (LeaveNotify<<8)+0x1e: return MAKINUM(((XCrossingEvent *) x)->detail);
  case (EnterNotify<<8)+0x1a: case (LeaveNotify<<8)+0x1a: return x_make_bool(((XCrossingEvent *) x)->same_screen);
  case (EnterNotify<<8)+0x1f: case (LeaveNotify<<8)+0x1f: return x_make_bool(((XCrossingEvent *) x)->focus);
  case (EnterNotify<<8)+0x18: case (LeaveNotify<<8)+0x18: return MAKINUM(((XCrossingEvent *) x)->state);
  case (FocusIn<<8)+0x10: case (FocusOut<<8)+0x10: return MAKINUM(((XFocusChangeEvent *) x)->type);
  case (FocusIn<<8)+0x11: case (FocusOut<<8)+0x11: return MAKINUM(((XFocusChangeEvent *) x)->serial);
  case (FocusIn<<8)+0x12: case (FocusOut<<8)+0x12: return x_make_bool(((XFocusChangeEvent *) x)->send_event);
  case (FocusIn<<8)+0x1d: case (FocusOut<<8)+0x1d: return MAKINUM(((XFocusChangeEvent *) x)->mode);
  case (FocusIn<<8)+0x1e: case (FocusOut<<8)+0x1e: return MAKINUM(((XFocusChangeEvent *) x)->detail);
  case (KeymapNotify<<8)+0x10: return MAKINUM(((XKeymapEvent *) x)->type);
  case (KeymapNotify<<8)+0x11: return MAKINUM(((XKeymapEvent *) x)->serial);
  case (KeymapNotify<<8)+0x12: return x_make_bool(((XKeymapEvent *) x)->send_event);
  case (Expose<<8)+0x10: return MAKINUM(((XExposeEvent *) x)->type);
  case (Expose<<8)+0x11: return MAKINUM(((XExposeEvent *) x)->serial);
  case (Expose<<8)+0x12: return x_make_bool(((XExposeEvent *) x)->send_event);
  case (Expose<<8)+0x14: return MAKINUM(((XExposeEvent *) x)->x);
  case (Expose<<8)+0x15: return MAKINUM(((XExposeEvent *) x)->y);
  case (Expose<<8)+0x20: return MAKINUM(((XExposeEvent *) x)->width);
  case (Expose<<8)+0x21: return MAKINUM(((XExposeEvent *) x)->height);
  case (Expose<<8)+0x22: return MAKINUM(((XExposeEvent *) x)->count);
  case (GraphicsExpose<<8)+0x10: return MAKINUM(((XGraphicsExposeEvent *) x)->type);
  case (GraphicsExpose<<8)+0x11: return MAKINUM(((XGraphicsExposeEvent *) x)->serial);
  case (GraphicsExpose<<8)+0x12: return x_make_bool(((XGraphicsExposeEvent *) x)->send_event);
  case (GraphicsExpose<<8)+0x14: return MAKINUM(((XGraphicsExposeEvent *) x)->x);
  case (GraphicsExpose<<8)+0x15: return MAKINUM(((XGraphicsExposeEvent *) x)->y);
  case (GraphicsExpose<<8)+0x20: return MAKINUM(((XGraphicsExposeEvent *) x)->width);
  case (GraphicsExpose<<8)+0x21: return MAKINUM(((XGraphicsExposeEvent *) x)->height);
  case (GraphicsExpose<<8)+0x22: return MAKINUM(((XGraphicsExposeEvent *) x)->count);
  case (GraphicsExpose<<8)+0x23: return MAKINUM(((XGraphicsExposeEvent *) x)->major_code);
  case (GraphicsExpose<<8)+0x24: return MAKINUM(((XGraphicsExposeEvent *) x)->minor_code);
  case (NoExpose<<8)+0x10: return MAKINUM(((XNoExposeEvent *) x)->type);
  case (NoExpose<<8)+0x11: return MAKINUM(((XNoExposeEvent *) x)->serial);
  case (NoExpose<<8)+0x12: return x_make_bool(((XNoExposeEvent *) x)->send_event);
  case (NoExpose<<8)+0x23: return MAKINUM(((XNoExposeEvent *) x)->major_code);
  case (NoExpose<<8)+0x24: return MAKINUM(((XNoExposeEvent *) x)->minor_code);
  case (VisibilityNotify<<8)+0x10: return MAKINUM(((XVisibilityEvent *) x)->type);
  case (VisibilityNotify<<8)+0x11: return MAKINUM(((XVisibilityEvent *) x)->serial);
  case (VisibilityNotify<<8)+0x12: return x_make_bool(((XVisibilityEvent *) x)->send_event);
  case (VisibilityNotify<<8)+0x18: return MAKINUM(((XVisibilityEvent *) x)->state);
  case (CreateNotify<<8)+0x10: return MAKINUM(((XCreateWindowEvent *) x)->type);
  case (CreateNotify<<8)+0x11: return MAKINUM(((XCreateWindowEvent *) x)->serial);
  case (CreateNotify<<8)+0x12: return x_make_bool(((XCreateWindowEvent *) x)->send_event);
  case (CreateNotify<<8)+0x14: return MAKINUM(((XCreateWindowEvent *) x)->x);
  case (CreateNotify<<8)+0x15: return MAKINUM(((XCreateWindowEvent *) x)->y);
  case (CreateNotify<<8)+0x20: return MAKINUM(((XCreateWindowEvent *) x)->width);
  case (CreateNotify<<8)+0x21: return MAKINUM(((XCreateWindowEvent *) x)->height);
  case (CreateNotify<<8)+0x25: return MAKINUM(((XCreateWindowEvent *) x)->border_width);
  case (CreateNotify<<8)+0x26: return x_make_bool(((XCreateWindowEvent *) x)->override_redirect);
  case (DestroyNotify<<8)+0x10: return MAKINUM(((XDestroyWindowEvent *) x)->type);
  case (DestroyNotify<<8)+0x11: return MAKINUM(((XDestroyWindowEvent *) x)->serial);
  case (DestroyNotify<<8)+0x12: return x_make_bool(((XDestroyWindowEvent *) x)->send_event);
  case (UnmapNotify<<8)+0x10: return MAKINUM(((XUnmapEvent *) x)->type);
  case (UnmapNotify<<8)+0x11: return MAKINUM(((XUnmapEvent *) x)->serial);
  case (UnmapNotify<<8)+0x12: return x_make_bool(((XUnmapEvent *) x)->send_event);
  case (UnmapNotify<<8)+0x27: return x_make_bool(((XUnmapEvent *) x)->from_configure);
  case (MapNotify<<8)+0x10: return MAKINUM(((XMapEvent *) x)->type);
  case (MapNotify<<8)+0x11: return MAKINUM(((XMapEvent *) x)->serial);
  case (MapNotify<<8)+0x12: return x_make_bool(((XMapEvent *) x)->send_event);
  case (MapNotify<<8)+0x26: return x_make_bool(((XMapEvent *) x)->override_redirect);
  case (MapRequest<<8)+0x10: return MAKINUM(((XMapRequestEvent *) x)->type);
  case (MapRequest<<8)+0x11: return MAKINUM(((XMapRequestEvent *) x)->serial);
  case (MapRequest<<8)+0x12: return x_make_bool(((XMapRequestEvent *) x)->send_event);
  case (ReparentNotify<<8)+0x10: return MAKINUM(((XReparentEvent *) x)->type);
  case (ReparentNotify<<8)+0x11: return MAKINUM(((XReparentEvent *) x)->serial);
  case (ReparentNotify<<8)+0x12: return x_make_bool(((XReparentEvent *) x)->send_event);
  case (ReparentNotify<<8)+0x14: return MAKINUM(((XReparentEvent *) x)->x);
  case (ReparentNotify<<8)+0x15: return MAKINUM(((XReparentEvent *) x)->y);
  case (ReparentNotify<<8)+0x26: return x_make_bool(((XReparentEvent *) x)->override_redirect);
  case (ConfigureNotify<<8)+0x10: return MAKINUM(((XConfigureEvent *) x)->type);
  case (ConfigureNotify<<8)+0x11: return MAKINUM(((XConfigureEvent *) x)->serial);
  case (ConfigureNotify<<8)+0x12: return x_make_bool(((XConfigureEvent *) x)->send_event);
  case (ConfigureNotify<<8)+0x14: return MAKINUM(((XConfigureEvent *) x)->x);
  case (ConfigureNotify<<8)+0x15: return MAKINUM(((XConfigureEvent *) x)->y);
  case (ConfigureNotify<<8)+0x20: return MAKINUM(((XConfigureEvent *) x)->width);
  case (ConfigureNotify<<8)+0x21: return MAKINUM(((XConfigureEvent *) x)->height);
  case (ConfigureNotify<<8)+0x25: return MAKINUM(((XConfigureEvent *) x)->border_width);
  case (ConfigureNotify<<8)+0x26: return x_make_bool(((XConfigureEvent *) x)->override_redirect);
  case (GravityNotify<<8)+0x10: return MAKINUM(((XGravityEvent *) x)->type);
  case (GravityNotify<<8)+0x11: return MAKINUM(((XGravityEvent *) x)->serial);
  case (GravityNotify<<8)+0x12: return x_make_bool(((XGravityEvent *) x)->send_event);
  case (GravityNotify<<8)+0x14: return MAKINUM(((XGravityEvent *) x)->x);
  case (GravityNotify<<8)+0x15: return MAKINUM(((XGravityEvent *) x)->y);
  case (ResizeRequest<<8)+0x10: return MAKINUM(((XResizeRequestEvent *) x)->type);
  case (ResizeRequest<<8)+0x11: return MAKINUM(((XResizeRequestEvent *) x)->serial);
  case (ResizeRequest<<8)+0x12: return x_make_bool(((XResizeRequestEvent *) x)->send_event);
  case (ResizeRequest<<8)+0x20: return MAKINUM(((XResizeRequestEvent *) x)->width);
  case (ResizeRequest<<8)+0x21: return MAKINUM(((XResizeRequestEvent *) x)->height);
  case (ConfigureRequest<<8)+0x10: return MAKINUM(((XConfigureRequestEvent *) x)->type);
  case (ConfigureRequest<<8)+0x11: return MAKINUM(((XConfigureRequestEvent *) x)->serial);
  case (ConfigureRequest<<8)+0x12: return x_make_bool(((XConfigureRequestEvent *) x)->send_event);
  case (ConfigureRequest<<8)+0x14: return MAKINUM(((XConfigureRequestEvent *) x)->x);
  case (ConfigureRequest<<8)+0x15: return MAKINUM(((XConfigureRequestEvent *) x)->y);
  case (ConfigureRequest<<8)+0x20: return MAKINUM(((XConfigureRequestEvent *) x)->width);
  case (ConfigureRequest<<8)+0x21: return MAKINUM(((XConfigureRequestEvent *) x)->height);
  case (ConfigureRequest<<8)+0x25: return MAKINUM(((XConfigureRequestEvent *) x)->border_width);
  case (ConfigureRequest<<8)+0x1e: return MAKINUM(((XConfigureRequestEvent *) x)->detail);
  case (ConfigureRequest<<8)+0x28: return MAKINUM(((XConfigureRequestEvent *) x)->value_mask);
  case (CirculateNotify<<8)+0x10: return MAKINUM(((XCirculateEvent *) x)->type);
  case (CirculateNotify<<8)+0x11: return MAKINUM(((XCirculateEvent *) x)->serial);
  case (CirculateNotify<<8)+0x12: return x_make_bool(((XCirculateEvent *) x)->send_event);
  case (CirculateNotify<<8)+0x29: return MAKINUM(((XCirculateEvent *) x)->place);
  case (CirculateRequest<<8)+0x10: return MAKINUM(((XCirculateRequestEvent *) x)->type);
  case (CirculateRequest<<8)+0x11: return MAKINUM(((XCirculateRequestEvent *) x)->serial);
  case (CirculateRequest<<8)+0x12: return x_make_bool(((XCirculateRequestEvent *) x)->send_event);
  case (CirculateRequest<<8)+0x29: return MAKINUM(((XCirculateRequestEvent *) x)->place);
  case (PropertyNotify<<8)+0x10: return MAKINUM(((XPropertyEvent *) x)->type);
  case (PropertyNotify<<8)+0x11: return MAKINUM(((XPropertyEvent *) x)->serial);
  case (PropertyNotify<<8)+0x12: return x_make_bool(((XPropertyEvent *) x)->send_event);
  case (PropertyNotify<<8)+0x13: return ulong2num(((XPropertyEvent *) x)->time);
  case (PropertyNotify<<8)+0x18: return MAKINUM(((XPropertyEvent *) x)->state);
  case (SelectionClear<<8)+0x10: return MAKINUM(((XSelectionClearEvent *) x)->type);
  case (SelectionClear<<8)+0x11: return MAKINUM(((XSelectionClearEvent *) x)->serial);
  case (SelectionClear<<8)+0x12: return x_make_bool(((XSelectionClearEvent *) x)->send_event);
  case (SelectionClear<<8)+0x13: return ulong2num(((XSelectionClearEvent *) x)->time);
  case (SelectionRequest<<8)+0x10: return MAKINUM(((XSelectionRequestEvent *) x)->type);
  case (SelectionRequest<<8)+0x11: return MAKINUM(((XSelectionRequestEvent *) x)->serial);
  case (SelectionRequest<<8)+0x12: return x_make_bool(((XSelectionRequestEvent *) x)->send_event);
  case (SelectionRequest<<8)+0x13: return ulong2num(((XSelectionRequestEvent *) x)->time);
  case (SelectionNotify<<8)+0x10: return MAKINUM(((XSelectionEvent *) x)->type);
  case (SelectionNotify<<8)+0x11: return MAKINUM(((XSelectionEvent *) x)->serial);
  case (SelectionNotify<<8)+0x12: return x_make_bool(((XSelectionEvent *) x)->send_event);
  case (SelectionNotify<<8)+0x13: return ulong2num(((XSelectionEvent *) x)->time);
  case (ColormapNotify<<8)+0x10: return MAKINUM(((XColormapEvent *) x)->type);
  case (ColormapNotify<<8)+0x11: return MAKINUM(((XColormapEvent *) x)->serial);
  case (ColormapNotify<<8)+0x12: return x_make_bool(((XColormapEvent *) x)->send_event);
  case (ColormapNotify<<8)+0x2a: return x_make_bool(((XColormapEvent *) x)->new);
  case (ColormapNotify<<8)+0x18: return MAKINUM(((XColormapEvent *) x)->state);
  case (ClientMessage<<8)+0x10: return MAKINUM(((XClientMessageEvent *) x)->type);
  case (ClientMessage<<8)+0x11: return MAKINUM(((XClientMessageEvent *) x)->serial);
  case (ClientMessage<<8)+0x12: return x_make_bool(((XClientMessageEvent *) x)->send_event);
  case (ClientMessage<<8)+0x2b: return MAKINUM(((XClientMessageEvent *) x)->format);
  case (MappingNotify<<8)+0x10: return MAKINUM(((XMappingEvent *) x)->type);
  case (MappingNotify<<8)+0x11: return MAKINUM(((XMappingEvent *) x)->serial);
  case (MappingNotify<<8)+0x12: return x_make_bool(((XMappingEvent *) x)->send_event);
  case (MappingNotify<<8)+0x2c: return MAKINUM(((XMappingEvent *) x)->request);
  case (MappingNotify<<8)+0x2d: return MAKINUM(((XMappingEvent *) x)->first_keycode);
  case (MappingNotify<<8)+0x22: return MAKINUM(((XMappingEvent *) x)->count);
#else
  {MotionNotify,        "MotionNotify"},
  {KeyPress,            "KeyPress"},
  {KeyRelease,          "KeyRelease"},
  {ButtonPress,         "ButtonPress"},
  {ButtonRelease,       "ButtonRelease"},
  {MotionNotify,        "MotionNotify"},
  {EnterNotify,         "EnterNotify"},
  {LeaveNotify,         "LeaveNotify"},
  {FocusIn,             "FocusIn"},
  {FocusOut,            "FocusOut"},
  {KeymapNotify,        "KeymapNotify"},
  {Expose,              "Expose"},
  {GraphicsExpose,      "GraphicsExpose"},
  {NoExpose,            "NoExpose"},
  {VisibilityNotify,    "VisibilityNotify"},
  {CreateNotify,        "CreateNotify"},
  {DestroyNotify,       "DestroyNotify"},
  {UnmapNotify,         "UnmapNotify"},
  {MapNotify,           "MapNotify"},
  {MapRequest,          "MapRequest"},
  {ReparentNotify,      "ReparentNotify"},
  {ConfigureNotify,     "ConfigureNotify"},
  {ConfigureRequest,    "ConfigureRequest"},
  {GravityNotify,       "GravityNotify"},
  {ResizeRequest,       "ResizeRequest"},
  {CirculateNotify,     "CirculateNotify"},
  {CirculateRequest,    "CirculateRequest"},
  {PropertyNotify,      "PropertyNotify"},
  {SelectionClear,      "SelectionClear"},
  {SelectionRequest,    "SelectionRequest"},
  {SelectionNotify,     "SelectionNotify"},
  {ColormapNotify,      "ColormapNotify"},
  {ClientMessage,       "ClientMessage"},
  {MappingNotify,       "MappingNotify"},
#endif
