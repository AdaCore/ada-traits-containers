BUILD=Production

all:
	gprbuild -m -p -Psrc/conts -j0 -XBUILD=${BUILD}

# Run all tests, except manual ones
test:
	cd tests; python ./testsuite.py

# Run manual tests
perfs spark:
	cd tests; python ./testsuite.py $@

# Create all project files, for use with GPS
projects:
	cd tests; python ./testsuite.py -c


valgrind:
	valgrind --leak-check=full --max-stackframe=3800000 --show-reachable=yes ./tests/obj/perf

clean:
	gprclean -Ptests -r -q
