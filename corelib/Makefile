all: libcfgtest

libtest:
	gcc -I ./scm  -o libtest libtest.c example.o ./scm/libscm.a  -ldl -lm -lc

libtest2:
	gcc -L . -I ./scm  -o libtest libtest.c ./scm/libscm.a  -ldl -lm -lc -lcfg

cfg.c:
	scm -r compile -e "(compile-file \"cfg.scm\")"

cfgcli.c:
	scm -r compile -e "(compile-file \"cfgcli.scm\")"

cfg.o: cfg.c
	gcc -I ./scm -fPIC -c cfg.c

cfgcli.o: cfgcli.c
	gcc -I ./scm -fpic -c cfgcli.c

libcfg.o:
	gcc -I ./scm -c -fPIC libcfg.c

# needs compilation of libscm.a using -fPIC
# (dlll linux "-fPIC -DSUN_DL" "-ldl" #f () ()) in scm/build.scm
libcfg.so: libcfg.o cfg.o cfgcli.o
	gcc -shared -o libcfg.so libcfg.o cfg.o cfgcli.o -Wl,--whole-archive ./scm/libscm.a -Wl,--no-whole-archive

libcfgtest: libcfg.so
	export LD_LIBRARY_PATH=$(pwd):$LD_LIBRARY_PATH
	gcc -L. libcfgtest.c -lcfg -ldl -lm -lc -o libcfgtest

clean:
	rm *.so
	rm *.o
	rm cfg.c

rebuild: clean all

test:
	scm -r define-record-type -r macro -f tests.scm
