/* "socket.c" internet stream socket support for client/server in SCM
 * Copyright (C) 1994, 1995, 2006 Free Software Foundation, Inc.
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

/* Author: Aubrey Jaffer.
 * Thanks to Hallvard.Tretteberg@si.sintef.no
 * who credits NCSA httpd software by Rob McCool 3/21/93
 */

#include "scm.h"

#ifdef macintosh
# define SOCKETDEFS
# include "macsocket.h"
#endif

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <arpa/inet.h>
/* added by Denys Duchier: for bzero */
#ifdef sun
# include <strings.h>
#endif

#ifndef STDC_HEADERS
	int close P((int fd));
#else /* added by Denys Duchier */
# ifdef SVR4
#  include <unistd.h>
# endif
# ifdef __OpenBSD__
#  include <unistd.h>
# endif
# ifdef __NetBSD__
#  include <unistd.h>
# endif
#endif /* STDC_HEADERS */

static char s_inetaddr[] = "inet:string->address";
SCM l_inetaddr (host)
     SCM host;
{
  struct in_addr soka;
  ASRTER(NIMP(host) && STRINGP(host), host, ARG1, s_inetaddr);
  soka.s_addr = inet_addr(CHARS(host));
  if (-1==soka.s_addr) {
    struct hostent *entry;
    DEFER_INTS;
    SYSCALL(entry = gethostbyname(CHARS(host)););
    ALLOW_INTS;
    if (!entry) return BOOL_F;
    return ulong2num(ntohl(((struct in_addr *)entry->h_addr)->s_addr));
  }
  return ulong2num(ntohl(soka.s_addr));
}

static char s_inetstr[] = "inet:address->string";
SCM l_inetstr (inetid)
     SCM inetid;
{
  struct in_addr addr;
  char *ans;
  addr.s_addr = htonl(num2ulong(inetid, (char *)ARG1, s_inetstr));
  SYSCALL(ans = inet_ntoa(addr););
  return makfrom0str(ans);
}

static char s_network[] = "inet:network";
SCM l_network (host)
     SCM host;
{
  struct in_addr addr;
  addr.s_addr = htonl(num2ulong(host, (char *)ARG1, s_network));
  return ulong2num(0L+inet_netof(addr));
}

#ifndef __CYGWIN__
static char s_lna[] = "inet:local-network-address";
SCM l_lna (host)
     SCM host;
{
  struct in_addr addr;
  addr.s_addr = htonl(num2ulong(host, (char *)ARG1, s_lna));
  return ulong2num(0L+inet_lnaof(addr));
}
#endif

static char s_makaddr[] = "inet:make-address";
SCM l_makaddr (net, lna)
     SCM net, lna;
{
  struct in_addr addr;
  unsigned long netnum = num2ulong(net, (char *)ARG1, s_makaddr);
  unsigned long lnanum = num2ulong(lna, (char *)ARG2, s_makaddr);
  addr = inet_makeaddr(netnum, lnanum);
  return ulong2num(ntohl(addr.s_addr));
}

#ifndef __CYGWIN__
static char s_hostinfo[] = "gethost";
SCM l_hostinfo(name)
     SCM name;
{
  SCM ans = make_vector(MAKINUM(5), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  SCM lst = EOL;
  struct hostent *entry;
  struct in_addr inad;
  const char **argv;
  int i = 0;
# ifndef linux
  if (UNBNDP(name)) {
    DEFER_INTS;
    SYSCALL(entry = gethostent(););
  }
  else
# endif
    if (NIMP(name) && STRINGP(name)) {
    DEFER_INTS;
    SYSCALL(entry = gethostbyname(CHARS(name)););
  }
  else {
    inad.s_addr = htonl(num2ulong(name, (char *)ARG1, s_hostinfo));
    DEFER_INTS;
    SYSCALL(entry = gethostbyaddr((char *)&inad , sizeof(inad), AF_INET););
  }
  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->h_name);
  ve[ 1] = makfromstrs(-1, entry->h_aliases);
  ve[ 2] = MAKINUM(entry->h_addrtype + 0L);
  ve[ 3] = MAKINUM(entry->h_length + 0L);
  if (sizeof(struct in_addr) != entry->h_length)
    {ve[ 4] = BOOL_F; return ans;}
  for (argv = entry->h_addr_list; argv[i]; i++);
  while (i--) {
    inad = *(struct in_addr *)argv[i];
    lst = cons(ulong2num(ntohl(inad.s_addr)), lst);
  }
  ve[ 4] = lst;
  return ans;
}
static char s_netinfo[] = "getnet";
SCM l_netinfo(name)
     SCM name;
{
  SCM ans = make_vector(MAKINUM(4), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  struct netent *entry;
  if (UNBNDP(name)) {
    DEFER_INTS;
    SYSCALL(entry = getnetent(););
  }
  else if (NIMP(name) && STRINGP(name)) {
    DEFER_INTS;
    SYSCALL(entry = getnetbyname(CHARS(name)););
  }
  else {
    unsigned long netnum;
    netnum = num2ulong(name, (char *)ARG1, s_netinfo);
    DEFER_INTS;
    SYSCALL(entry = getnetbyaddr(netnum, AF_INET););
  }
  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->n_name);
  ve[ 1] = makfromstrs(-1, entry->n_aliases);
  ve[ 2] = MAKINUM(entry->n_addrtype + 0L);
  ve[ 3] = ulong2num(entry->n_net + 0L);
  return ans;
}
#endif
static char s_protoinfo[] = "getproto";
SCM l_protoinfo(name)
     SCM name;
{
  SCM ans = make_vector(MAKINUM(3), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  struct protoent *entry;
  if (UNBNDP(name)) {
    DEFER_INTS;
    SYSCALL(entry = getprotoent(););
  }
  else if (NIMP(name) && STRINGP(name)) {
    DEFER_INTS;
    SYSCALL(entry = getprotobyname(CHARS(name)););
  }
  else {
    unsigned long protonum;
    protonum = num2ulong(name, (char *)ARG1, s_protoinfo);
    DEFER_INTS;
    SYSCALL(entry = getprotobynumber(protonum););
  }
  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->p_name);
  ve[ 1] = makfromstrs(-1, entry->p_aliases);
  ve[ 2] = MAKINUM(entry->p_proto + 0L);
  return ans;
}
static char s_servinfo[] = "getserv";
SCM l_servinfo(args)
     SCM args;
{
  SCM ans = make_vector(MAKINUM(4), UNSPECIFIED);
  SCM *ve = VELTS(ans);
  SCM name, proto;
  struct servent *entry;
  if (NULLP(args)) {
    DEFER_INTS;
    SYSCALL(entry = getservent(););
    goto comlab;
  }
  name = CAR(args);
  proto = CDR(args);
  ASRTER(NIMP(proto) && CONSP(proto), args, WNA, s_servinfo);
  proto = CAR(proto);
  ASRTER(NIMP(proto) && STRINGP(proto), args, ARG2, s_servinfo);
  DEFER_INTS;
  if (NIMP(name) && STRINGP(name)) {
    SYSCALL(entry = getservbyname(CHARS(name), CHARS(proto)););
  }
  else {
    ASRTER(INUMP(proto), proto, ARG1, s_servinfo);
    SYSCALL(entry = getservbyport(INUM(proto), CHARS(proto)););
  }
 comlab:  ALLOW_INTS;
  if (!entry) return BOOL_F;
  ve[ 0] = makfrom0str(entry->s_name);
  ve[ 1] = makfromstrs(-1, entry->s_aliases);
  ve[ 2] = MAKINUM(ntohs(entry->s_port) + 0L);
  ve[ 3] = makfrom0str(entry->s_proto);
  return ans;
}

SCM l_sethost(arg)
     SCM arg;
{
  if (UNBNDP(arg)) endhostent();
  else sethostent(NFALSEP(arg));
  return UNSPECIFIED;
}
#ifndef __CYGWIN__
SCM l_setnet(arg)
     SCM arg;
{
  if (UNBNDP(arg)) endnetent();
  else setnetent(NFALSEP(arg));
  return UNSPECIFIED;
}
#endif
SCM l_setproto(arg)
     SCM arg;
{
  if (UNBNDP(arg)) endprotoent();
  else setprotoent(NFALSEP(arg));
  return UNSPECIFIED;
}
SCM l_setserv(arg)
     SCM arg;
{
  if (UNBNDP(arg)) endservent();
  else setservent(NFALSEP(arg));
  return UNSPECIFIED;
}

static char s_socket[] = "make-stream-socket";
SCM l_socket(fam, proto)
     SCM fam, proto;
{
  int sd, j, tp = INUM(fam);
  FILE* f;
  SCM port;
  ASRTER(INUMP(fam), fam, ARG1, s_socket);
  if (UNBNDP(proto)) proto = INUM0;
  else ASRTER(INUMP(proto), proto, ARG2, s_socket);
  NEWCELL(port);
  DEFER_INTS;
  SYSCALL(sd = socket(tp, SOCK_STREAM, INUM(proto)););
  if (-1==sd) wta(UNDEFINED, (char *)NALLOC, s_socket);
  SYSCALL(f = fdopen(sd, "r+"););
  /*  SCM_OPENCALL(f = fdopen(sd, "r+")); */
  if (!f) {
    close(sd);
    wta(MAKINUM(sd), (char *)NALLOC, s_port_type);
  }
  port = scm_port_entry(f, tc_socket, BUF0);
  SCM_PORTDATA(port) = fam;
  i_setbuf0(port);
  ALLOW_INTS;
  if (AF_INET==tp) {
#ifdef macintosh
    sd = setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, (char *)&j, sizeof(j));
#else
    sd = setsockopt(sd, SOL_SOCKET, SO_REUSEADDR, &j, sizeof(j));
#endif
    ASRTER(!sd, port, "could not set socket option", s_socket);
  }
  return port;
}
static char s_socketpair[] = "make-stream-socketpair";
SCM l_socketpair(fam, proto)
     SCM fam, proto;
{
  int sts, tp = INUM(fam);
  int sv[2];
  FILE* f[2];
  SCM port[2];
  ASRTER(INUMP(fam), fam, ARG1, s_socketpair);
  if (UNBNDP(proto)) proto = INUM0;
  else ASRTER(INUMP(proto), proto, ARG2, s_socketpair);
  NEWCELL(port[0]); NEWCELL(port[1]);
  DEFER_INTS;
  SYSCALL(sts = socketpair(tp, SOCK_STREAM, INUM(proto), sv););
  if (-1==sts) wta(UNDEFINED, (char *)NALLOC, s_socketpair);
  SCM_OPENCALL(f[0] = fdopen(sv[0], "r+"));
  if (!f[0]) {
    close(sv[0]);
    wta(MAKINUM(sv[0]), (char *)NALLOC, s_port_type);
  }
  SCM_OPENCALL(f[1] = fdopen(sv[1], "r+"));
  if (!f[1]) {
    fclose(f[0]);
    close(sv[1]);
    wta(MAKINUM(sv[1]), (char *)NALLOC, s_port_type);
  }
  port[0] = scm_port_entry(f[0], tc16_fport, mode_bits("r+0", (char *)0));
  CAR(port[1]) = scm_port_entry(f[1], tc16_fport, mode_bits("r+0", (char *)0));
  i_setbuf0(port[0]); i_setbuf0(port[1]);
  ALLOW_INTS;
  return cons(port[0], port[1]);
}

static char s_shutdown[] = "socket:shutdown";
SCM l_shutdown(port, how)
     SCM port, how;
{
  int sts;
  ASRTER(NIMP(port) && OPFPORTP(port), port, ARG1, s_shutdown);
  ASRTER(INUMP(how) && 0 <= INUM(how) && 2 >= INUM(how),
	 how, ARG2, s_shutdown);
  SYSCALL(sts = shutdown(fileno(STREAM(port)), INUM(how)););
  if (sts) return BOOL_F;
  switch (INUM(how)) {
  case 0: CAR(port) &= ~RDNG;
    break;
  case 1: CAR(port) &= ~WRTNG;
    break;
  case 2: CAR(port) &= ~(RDNG | WRTNG);
  }
  if (SOCKP(port)) close_port(port); /* can't read or write */
  return port;
}
static char s_unkfam[] = "unknown-family";
static char s_connect[] = "socket:connect";
SCM l_connect (sockpt, address, arg)
     SCM sockpt, address, arg;
{
  long flags;
  int sts;
  ASRTER(NIMP(sockpt) && SOCKP(sockpt), sockpt, ARG1, s_connect);
  switch SOCKTYP(sockpt) {
  default:
    ASRTER(0, sockpt, s_unkfam, s_connect);
  case AF_INET:
    ASRTER(NIMP(arg) && CONSP(arg) && NULLP(CDR(arg)), arg, WNA, s_connect);
    arg = CAR(arg);
    ASRTER(INUMP(arg), arg, ARG3, s_connect);
    {
      struct sockaddr_in soka;
      soka.sin_addr.s_addr =
	htonl(num2ulong(address, (char *)ARG2, s_connect));
      soka.sin_family = AF_INET;
      soka.sin_port = htons(INUM(arg));
      SYSCALL(sts = connect(fileno(STREAM(sockpt)),
			    (struct sockaddr*)&soka, sizeof(soka)););
    }
    break;
  case AF_UNIX:
    ASRTER(NULLP(arg), arg, WNA, s_connect);
    ASRTER(NIMP(address) && STRINGP(address), address, ARG2, s_connect);
    {
      struct sockaddr_un soka;
      soka.sun_family = AF_UNIX;
      memcpy(&soka.sun_path, CHARS(address), 1+LENGTH(address));
      SYSCALL(sts = connect(fileno(STREAM(sockpt)),
			    (struct sockaddr*)&soka, sizeof(soka)););
    }
    break;
  }
  if (sts) return BOOL_F;
  flags = tc16_fport | mode_bits("r+0", (char *)0);
  SCM_PORTFLAGS(sockpt) = flags;
  SCM_SETFLAGS(sockpt, flags);
  SCM_PORTDATA(sockpt) = cons(address, arg);
  return sockpt;
}

static char s_bind[] = "socket:bind";
SCM l_bind(sockpt, address)
     SCM sockpt, address;
{
  int sts;
  ASRTER(NIMP(sockpt) && SOCKP(sockpt), sockpt, ARG1, s_bind);
  switch SOCKTYP(sockpt) {
  default:
    ASRTER(0, sockpt, s_unkfam, s_bind);
  case AF_UNIX:
    ASRTER(NIMP(address) && STRINGP(address), address, ARG2, s_bind);
    {
      struct sockaddr_un sa_server;
      bzero((char *) &sa_server, sizeof(sa_server));
      sa_server.sun_family = AF_UNIX;
      memcpy(&sa_server.sun_path, CHARS(address), 1+LENGTH(address));
      SYSCALL(sts = bind(fileno(STREAM(sockpt)),
			 (struct sockaddr *)&sa_server, sizeof(sa_server)););
    }
    break;
  case AF_INET:
    ASRTER(INUMP(address), address, ARG2, s_bind);
    {
      struct sockaddr_in sa_server;
      bzero((char *) &sa_server, sizeof(sa_server));
      sa_server.sin_family = AF_INET;
      sa_server.sin_addr.s_addr = htonl(INADDR_ANY);
      sa_server.sin_port = htons(INUM(address));
      SYSCALL(sts = bind(fileno(STREAM(sockpt)),
			 (struct sockaddr *)&sa_server, sizeof(sa_server)););
    }
    break;
  }
  return sts ? BOOL_F : sockpt;
}

static char s_listen[] = "socket:listen";
SCM l_listen(port, backlog)
     SCM port, backlog;
{
  long flags;
  int sts;
  ASRTER(NIMP(port) && SOCKP(port), port, ARG1, s_listen);
  ASRTER(INUMP(backlog), backlog, ARG2, s_listen);
  SYSCALL(sts = listen(fileno(STREAM(port)), INUM(backlog)););
  if (sts) return BOOL_F;
  DEFER_INTS;
  flags  = tc16_fport | mode_bits("r0", (char *)0);
  SCM_PORTFLAGS(port) = flags;
  SCM_SETFLAGS(port, flags);
  ALLOW_INTS;
  return port;
}

static char s_accept[] = "socket:accept";
SCM l_accept(sockpt)
     SCM sockpt;
{
  int newsd, sadlen;
  struct sockaddr sad;
  FILE *newfd;
  SCM newpt;
  NEWCELL(newpt);
  ASRTER(NIMP(sockpt) && OPINPORTP(sockpt), sockpt, ARG1, s_accept);
  sadlen=sizeof(sad);
  SYSCALL(newsd = accept(fileno(STREAM(sockpt)), &sad, &sadlen););
  if (-1==newsd) {
#ifndef macintosh
    if (EWOULDBLOCK != errno) return BOOL_F;
    else
#endif
        wta(sockpt, "couldn't", s_accept);
  }
  DEFER_INTS;
  SCM_OPENCALL(newfd = fdopen(newsd, "r+"));
  if (!newfd) {
    close(newsd);
    wta(MAKINUM(newsd), (char *)NALLOC, s_port_type);
  }
  newpt = scm_port_entry(newfd, tc16_fport, mode_bits("r+0", (char *)0));
  i_setbuf0(newpt);
  ALLOW_INTS;
  return newpt;
}

int sknm_print(exp, port, writing)
     SCM exp; SCM port; int writing;
{
  lputs("#<", port);
  switch (((struct sockaddr *)CDR(exp))->sa_family) {
  case AF_UNIX:
    lputs("unix-addr ", port);
    lputs(((struct sockaddr_un *)CDR(exp))->sun_path, port);
    break;
  case AF_INET:
    lputs("inet-addr ", port);
    lputs(inet_ntoa(((struct sockaddr_in *)CDR(exp))->sin_addr), port);
    lputc(':', port);
    scm_intprint(0L + ntohs(((struct sockaddr_in *)CDR(exp))->sin_port), 10, port);
    break;
  default: lputs(s_unkfam, port);
    lputc(' ', port);
    scm_intprint(0L+((struct sockaddr *)CDR(exp))->sa_family, 10, port);
  }
  lputc('>', port);
  return !0;
}
sizet sknm_free(p)
     CELLPTR p;
{
  must_free(CHARS((SCM)p), sizeof(struct sockaddr));
  return 0;
}
long tc16_sknm;
static smobfuns sknm_smob = {mark0, sknm_free, sknm_print, 0};

char s_sknm_family[] = "socket-name:family";
SCM l_sknm_family(snm)
     SCM snm;
{
  ASRTER(NIMP(snm) && TYP16(snm)==tc16_sknm, snm, ARG1, s_sknm_family);
  return MAKINUM(((struct sockaddr *)CDR(snm))->sa_family + 0L);
}
char s_sknm_port_num[] = "socket-name:port-number";
SCM l_sknm_port_num(snm)
     SCM snm;
{
  ASRTGO(NIMP(snm) && TYP16(snm)==tc16_sknm, err1);
  switch (((struct sockaddr *)CDR(snm))->sa_family) {
  default:
  err1:
    wta(snm, (char *)ARG1, s_sknm_port_num);
  case AF_INET:
    return MAKINUM(ntohs(((struct sockaddr_in *)CDR(snm))->sin_port) + 0L);
  }
}
char s_sknm_addr[] = "socket-name:address";
SCM l_sknm_addr(snm)
     SCM snm;
{
  ASRTGO(NIMP(snm) && TYP16(snm)==tc16_sknm, err1);
  switch (((struct sockaddr *)CDR(snm))->sa_family) {
  default:
  err1:
    wta(snm, (char *)ARG1, s_sknm_addr);
  case AF_INET:
    return ulong2num(ntohl(((struct sockaddr_in *)CDR(snm))->sin_addr.s_addr));
  case AF_UNIX:			/* the manual says this won't work anyway */
    return makfrom0str(((struct sockaddr_un *)CDR(snm))->sun_path);
  }
}

SCM maksknm(sad)
     struct sockaddr *sad;
{
  SCM sknm;
  struct sockaddr *msknm;
  DEFER_INTS;
  sknm = must_malloc_cell(0L+sizeof(struct sockaddr), (SCM)tc16_sknm, "sknm");
  msknm = (struct sockaddr *)CDR(sknm);
  *msknm = *sad;
  ALLOW_INTS;
  return sknm;
}

static char s_getpeername[] = "getpeername";
SCM l_getpeername(sockpt)
     SCM sockpt;
{
  struct sockaddr_in sad;
  int sts, sadlen = sizeof(sad);
  bzero((char *) &sad, sizeof(sad));
  ASRTER(NIMP(sockpt) && OPFPORTP(sockpt), sockpt, ARG1, s_getpeername);
  SYSCALL(sts = getpeername(fileno(STREAM(sockpt)),
			    (struct sockaddr*)&sad, &sadlen););
  if (sts || sizeof(sad) != sadlen) return BOOL_F;
/*  ASRTER(sad.sin_family==AF_INET, sockpt, "non-internet", s_getpeername); */
  return maksknm(&sad);
}
static char s_getsockname[] = "getsockname";
SCM l_getsockname(sockpt)
     SCM sockpt;
{
  struct sockaddr_in sad;
  int sts, sadlen = sizeof(sad);
  bzero((char *) &sad, sizeof(sad));
  ASRTER(NIMP(sockpt) && OPFPORTP(sockpt), sockpt, ARG1, s_getsockname);
  SYSCALL(sts = getsockname(fileno(STREAM(sockpt)),
			    (struct sockaddr*)&sad, &sadlen););
  if (sts || sizeof(sad) != sadlen) return BOOL_F;
  return maksknm(&sad);
}
static iproc subr1s[] = {
	{s_inetaddr, l_inetaddr},
	{s_inetstr, l_inetstr},
	{s_network, l_network},
#ifndef __CYGWIN__
	{s_lna, l_lna},
#endif
	{s_accept, l_accept},
	{s_sknm_family, l_sknm_family},
	{s_sknm_port_num, l_sknm_port_num},
	{s_sknm_addr, l_sknm_addr},
	{s_getpeername, l_getpeername},
	{s_getsockname, l_getsockname},
	{0, 0}};

static iproc subr1os[] = {
	{s_protoinfo, l_protoinfo},
#ifndef __CYGWIN__
	{s_hostinfo, l_hostinfo},
	{s_netinfo, l_netinfo},
	{"setnetent", l_setnet},
#endif
	{"sethostent", l_sethost},
	{"setprotoent", l_setproto},
	{"setservent", l_setserv},
	{0, 0}};

static iproc subr2s[] = {
	{s_shutdown, l_shutdown},
	{s_bind, l_bind},
	{s_listen, l_listen},
	{s_makaddr, l_makaddr},
	{0, 0}};

void init_socket()
{
  sysintern("af_unix", MAKINUM(AF_UNIX));
  sysintern("af_inet", MAKINUM(AF_INET));
  init_iprocs(subr1s, tc7_subr_1);
  init_iprocs(subr1os, tc7_subr_1o);
  init_iprocs(subr2s, tc7_subr_2);
  make_subr(s_servinfo, tc7_lsubr, l_servinfo);
  make_subr(s_socket, tc7_subr_2o, l_socket);
  make_subr(s_socketpair, tc7_subr_2o, l_socketpair);
  make_subr(s_connect, tc7_lsubr_2, l_connect);
  tc16_sknm = newsmob(&sknm_smob);
  add_feature("socket");
}
