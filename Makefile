BUILD=Production

all:
	gprbuild -m -p -Psrc/conts -j0 -XBUILD=${BUILD}

test:
	cd tests; python ./testsuite.py
perfs:
	cd tests: python ./testsuite.py perfs

valgrind:
	valgrind --leak-check=full --max-stackframe=3800000 --show-reachable=yes ./tests/obj/perf

clean:
	gprclean -Ptests -r -q
