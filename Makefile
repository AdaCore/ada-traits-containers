BUILD=Production

# In our automatic nightly builds, we want to consider the source
# directory as read-only, and build in another directory.
ifeq (${SOURCE_DIR},)
GPR_CONTS=src/conts.gpr
RBD=
else
GPR_CONTS=$(SOURCE_DIR)/src/conts.gpr
RBD=--relocate-build-tree
endif

# Add support for passing extra switches to gprbuild, like -d
GPRBUILD_OPTIONS=

GPRBUILD=gprbuild ${RBD} -p -m -j0 ${GPRBUILD_OPTIONS}

all:
	${GPRBUILD} -P${GPR_CONTS} -XBUILD=${BUILD}

# Run all tests, except manual ones
test:
	${GPRBUILD} -XBUILD=Debug
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
perfs:
	${GPRBUILD} -Psrc/conts -XBUILD=Production
	cd tests; python ./testsuite.py -j0 --enable-color $@
spark:
	${GPRBUILD} -Psrc/conts -XBUILD=Debug
	cd tests; python ./testsuite.py -j0 --enable-color $@

# Create all project files, for use with GPS
projects:
	cd tests; python ./testsuite.py -c

clean:
	gprclean -Proot -r -q
	-rm -f tests/*/auto_*.gpr
	-rm -rf tests/*/obj/

