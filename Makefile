BUILD=Production

all:
	gprbuild -m -p -Psrc/conts -j0 -XBUILD=${BUILD}

# Run all tests, except manual ones
test:
	cd tests; python ./testsuite.py -j0 --enable-color

# Run all tests with assertions, except manual ones
test_with_assert:
	cd tests; BUILD=Debug python ./testsuite.py -j0 --enable-color

# Run all tests with valgrind
test_with_valgrind:
	cd tests; python ./testsuite.py -j0 --enable-color --valgrind

# Verify memory leaks in tests
test_with_leaks:
	cd tests; python ./testsuite.py -j0 --enable-color --leaks

# Run manual tests
perfs spark:
	cd tests; python ./testsuite.py -j0 --enable-color $@

# Create all project files, for use with GPS
projects:
	cd tests; python ./testsuite.py -c

clean:
	gprclean -Proot -r -q
	-rm -f tests/*/auto_*.gpr
	-rm -rf tests/*/obj/

