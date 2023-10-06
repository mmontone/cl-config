
#if 0 /* SCMVERSION is a string for the version specifier.  The leading
 #  number is the major version number, the letter is the revision ("a"
 #  for alpha release, "b" for beta release, "c", and so on), and the
 #  trailing number is the patchlevel. */
 #  /* This next line sets VERSION when included from the Makefile */
VERSION=5f3
#endif

#ifndef SCMVERSION
# define SCMVERSION "5f3"
#endif
#ifdef nosve
# define INIT_FILE_NAME "Init"SCMVERSION"_scm";
#else
# define INIT_FILE_NAME "Init"SCMVERSION".scm"
#endif
