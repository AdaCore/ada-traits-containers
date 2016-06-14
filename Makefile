BUILD=Production

all:
	gprbuild -m -p -Psrc/conts -j0 -XBUILD=${BUILD}

test:
	cd tests; ./run.sh
perfs:
	cd tests: ./run.sh perfs

valgrind:
	valgrind --leak-check=full --max-stackframe=3800000 --show-reachable=yes ./tests/obj/perf

clean:
	gprclean -Ptests -r -q
