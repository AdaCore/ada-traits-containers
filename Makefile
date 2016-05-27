all:
	@mkdir -p tests/generated
	python ./generate_test.py
	gprbuild -m -p -Ptests -j0

debug:
	@mkdir -p tests/generated
	python ./generate_test.py
	gprbuild -m -p -Ptests -j0 -XBUILD=Debug

run_test:
	./tests/obj/perf

# Check that our files are compatible with SPARK mode
check_spark:
	gnatprove -Pspark/spark.gpr -U --mode=check

# Prove our files
# Use Debug mode, so that optimization switches like Disable Overflow
# are not used for gnatprove
prove:
	BUILD=Debug gnatprove -Pspark/spark.gpr -u use_sets.adb use_lists.adb use_maps.adb use_ordered_sets.adb use_vectors.adb --dbg-proof-only -j0 -f --level=2
	# gnatprove -P spark/spark.gpr -U -j0 -f --level=2


valgrind:
	valgrind --leak-check=full --max-stackframe=3800000 --show-reachable=yes ./tests/obj/perf

clean:
	gprclean -Ptests -r -q
