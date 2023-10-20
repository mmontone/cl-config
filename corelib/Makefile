all: libcfgtest

libtest:
	gcc -I ./scm  -o libtest libtest.c example.o ./scm/libscm.a  -ldl -lm -lc

libtest2:
	gcc -L . -I ./scm  -o libtest libtest.c ./scm/libscm.a  -ldl -lm -lc -lcfg

util.c:
	scm -r compile  -e "(compile-file \"util.scm\")"

util.o: util.c
	gcc -I ./scm -fPIC -c util.c

cfg.c:
	scm -r compile -e "(compile-file \"cfg.expanded.scm\")"
	mv cfg.expanded.c cfg.c

cfgcli.c:
	scm -r compile -e "(compile-file \"cfgcli.scm\")"

cfg.o: cfg.c
	gcc -I ./scm -fPIC -c cfg.c

cfgcli.o: cfgcli.c
	gcc -I ./scm -fpic -c cfgcli.c

cfgall.scm:
	cat util.scm record.scm define-record-type.scm cfg.scm > cfgall.scm

cfgall.c: cfgall.scm
	scm -r compile -e "(compile-file \"cfgall.scm\")"

cfgall.o: cfgall.c
	gcc -I ./scm -fpic -c cfgall.c

libcfg.o:
	gcc -I ./scm -c -fPIC libcfg.c

# needs compilation of libscm.a using -fPIC
# (dlll linux "-fPIC -DSUN_DL" "-ldl" #f () ()) in scm/build.scm
# also add -F record to libscm.opt
libcfg.so: cfgall.o libcfg.o 
	gcc -shared -o libcfg.so libcfg.o cfgall.o -Wl,--whole-archive ./scm/libscm.a -Wl,--no-whole-archive -ldl -lm -lc

libcfgtest: libcfg.so
	export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
	gcc -L. libcfgtest.c -lcfg -o libcfgtest

clean:
	rm -f *.so
	rm -f *.o
	rm -f cfg.c cfgcli.c util.c cfgall.c
	rm -f libcfgtest
	rm -f cfgall.scm
	rm -f libcfg.v.c libcfg.v.so

rebuild: clean all

test:
	scm -r define-record-type -r macro -f tests.scm

libcfg.v.c:
	v -o libcfg.v.c libcfg.v

libcfg.v.so: clean libcfg.v.c
	gcc -shared -fpic -I ~/src/v/thirdparty/cJSON/ libcfg.v.c ~/src/v/thirdparty/cJSON/cJSON.c -lgc -o libcfg.v.so
	nm libcfg.v.so | grep cfg


libcfgtest.v.exe: libcfg.v.so
	export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
	gcc -L. libcfgtest.c -lcfg.v -o libcfgtest.v.exe
