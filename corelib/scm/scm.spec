%define name scm
%define version 5f3
%define release 1
%define implpath %{prefix}/lib/scm
%define slibpath %{prefix}/lib/slib
%define dumparch setarch %{_target_cpu}
# rpm seems to require all on one line, bleah.
%define features cautious bignums arrays inexact dynamic-linking macro engineering-notation

Name:         %{name}
Release:      %{release}
Version:      %{version}
Packager:     Aubrey Jaffer <agj @ alum.mit.edu>

License:      GPL
Vendor:       Aubrey Jaffer <agj @ alum.mit.edu>
Group:        Development/Languages
Provides:     scm
Requires:     slib

Summary:      SCM Scheme implementation
Source:       http://groups.csail.mit.edu/mac/ftpdir/scm/scm-%{version}.zip
URL:          http://people.csail.mit.edu/jaffer/SCM
BuildRoot:    %{_tmppath}/%{name}-%{version}
Prefix:       /usr

%description
Scm conforms to Revised^5 Report on the Algorithmic Language Scheme and
the IEEE P1178 specification.  Scm provides a machine independent
platform for JACAL, a symbolic algebra system.

This distribution requires libdl.so from the glibc-devel package and the
slib Scheme library package.  If your machine lacks XFree86 or readline,
install with --nodeps.

# % define __os_install_post /usr/lib/rpm/brp-compress

%prep
rm -rf /var/tmp/%{name}-%{version}
%setup -n scm -c -T
cd ..
unzip $RPM_SOURCE_DIR/scm-%{version}.zip

%build
# SLIB is required to build SCM.
if [ -n "$SCHEME_LIBRARY_PATH" ]; then
  echo using SLIB $SCHEME_LIBRARY_PATH
elif [ -d %{slibpath} ]; then
  export SCHEME_LIBRARY_PATH=%{slibpath}/
elif [ -d %{prefix}/share/slib ]; then
  export SCHEME_LIBRARY_PATH=%{prefix}/share/slib/
fi
make scmlit
make clean
export PATH=.:$PATH   # to get scmlit in the path.

# Build the executable.
./build -h system -o scm --compiler-options="-O2" -l debug -s %{implpath} -F %{features}
# echo "(quit)" | ./udscm5 -no-init-file -r5 -o scm
# make check

# Build dlls
# ./build -h system -t dll -F curses --compiler-options="-O2"
# ./build -h system -t dll -F edit-line --compiler-options="-O2"
make x.so
./build -h system -t dll -c differ.c --compiler-options="-O2"
./build -h system -t dll -c sc2.c --compiler-options="-O2"
./build -h system -t dll -c rgx.c --compiler-options="-O2"
./build -h system -t dll -c record.c --compiler-options="-O2"
./build -h system -t dll -c gsubr.c --compiler-options="-O2"
./build -h system -t dll -c ioext.c --compiler-options="-O2"
./build -h system -t dll -c posix.c --compiler-options="-O2"
./build -h system -t dll -c unix.c --compiler-options="-O2"
./build -h system -t dll -c socket.c --compiler-options="-O2"
./build -h system -t dll -c ramap.c --compiler-options="-O2"
./build -h system -t dll -c byte.c --compiler-options="-O2"
./build -h system -t dll -F x --compiler-options="-O2"

# Build libscm.a static library
./build -h system -F cautious bignums arrays inexact dynamic-linking -t lib \
	--compiler-options="-O2"

%install
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/bin
mkdir -p ${RPM_BUILD_ROOT}%{prefix}/lib/scm
mkdir -p ${RPM_BUILD_ROOT}%{_mandir}/man1
make prefix=${RPM_BUILD_ROOT}%{prefix}/ \
     mandir=${RPM_BUILD_ROOT}%{_mandir}/ \
     infodir=${RPM_BUILD_ROOT}%{_infodir}/ \
     install-for-rpm
rm -f ${RPM_BUILD_ROOT}%{prefix}/bin/scm
cp scm ${RPM_BUILD_ROOT}%{prefix}/bin/

# Assume SLIB is in %{prefix}/lib/slib, as installed by the slib rpm.
cat > ${RPM_BUILD_ROOT}%{prefix}/lib/scm/require.scm <<EOF
(set! library-vicinity (lambda () "%{prefix}/lib/slib/"))
EOF

%clean
rm -rf $RPM_BUILD_ROOT

# echo "(quit)" | % {dumparch} % {prefix}/bin/udscm5 -no-init-file -r5 -o % {prefix}/bin/scm
%post
ln -s %{prefix}/bin/scm %{prefix}/local/bin/
ln -s %{prefix}/lib/scm %{prefix}/local/lib/
%{prefix}/bin/scm -br new-catalog

%verifyscript
cp -f %{prefix}/lib/scm/r4rstest.scm /tmp/
(cd /tmp
    %{prefix}/bin/scm -fr4rstest.scm -e'(test-sc4)(test-cont)(test-delay)(gc)')

%preun
rm -f %{prefix}/lib/scm/slibcat %{prefix}/lib/scm/implcat
if [ -L %{prefix}/local/bin/scm ] && \
    ls -l %{prefix}/local/bin/scm | grep -q "> %{prefix}/bin/scm"; then
  rm -f %{prefix}/local/bin/scm
fi
if [ -L %{prefix}/local/lib/scm ] && \
    ls -l %{prefix}/local/lib/scm | grep -q "> %{prefix}/lib/scm"; then
  rm -f %{prefix}/local/lib/scm
fi
rm -f %{prefix}/bin/scm

%files
%defattr(-, root, root)
%{prefix}/bin/scmlit
%{prefix}/bin/scm
%dir %{prefix}/lib/scm
# No wildcards here because we need to segregate files by package.
# % {prefix}/lib/scm/crs.so
%{prefix}/lib/scm/gsubr.so
%{prefix}/lib/scm/posix.so
%{prefix}/lib/scm/record.so
%{prefix}/lib/scm/sc2.so
%{prefix}/lib/scm/unix.so
%{prefix}/lib/scm/ioext.so
%{prefix}/lib/scm/ramap.so
%{prefix}/lib/scm/socket.so
%{prefix}/lib/scm/rgx.so
%{prefix}/lib/scm/Init%{version}.scm
%{prefix}/lib/scm/require.scm
%{prefix}/lib/scm/Iedline.scm
%{prefix}/lib/scm/Macexp.scm
%{prefix}/lib/scm/Macro.scm
%{prefix}/lib/scm/Tscript.scm
%{prefix}/lib/scm/Transcen.scm
%{prefix}/lib/scm/mkimpcat.scm
%{prefix}/lib/scm/Link.scm
%{prefix}/lib/scm/compile.scm
%{prefix}/lib/scm/hobbit.scm
%{prefix}/lib/scm/scmhob.scm
%{prefix}/lib/scm/scmhob.h
%{prefix}/lib/scm/patchlvl.h
%{prefix}/lib/scm/build.scm
%{prefix}/lib/scm/build
%{prefix}/lib/scm/Idiffer.scm
%{prefix}/lib/scm/differ.so
%{prefix}/lib/scm/x.so
%{prefix}/lib/scm/xevent.scm
%{prefix}/lib/scm/xatoms.scm
%{prefix}/lib/scm/x11.scm
%{prefix}/lib/scm/keysymdef.scm
%{prefix}/lib/scm/r4rstest.scm
%{prefix}/lib/scm/byte.so
%{prefix}/lib/scm/COPYING
%{prefix}/lib/scm/COPYING.LESSER
%{_infodir}/Xlibscm.info.gz
%{_infodir}/hobbit.info.gz
%{_infodir}/scm.info.gz
%{_infodir}/dir
%{_mandir}/man1/scm.1.gz

%{prefix}/lib/libscm.a
%{prefix}/include/scm.h
%{prefix}/include/scmfig.h
%{prefix}/include/scmflags.h

%doc ANNOUNCE COPYING COPYING.LESSER QUICKREF README ChangeLog

%changelog
