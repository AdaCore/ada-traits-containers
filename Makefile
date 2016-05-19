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
prove:
	gnatprove -Pspark/spark.gpr -u use_sets.adb use_lists.adb use_maps.adb use_ordered_sets.adb use_vectors.adb --dbg-proof-only --prover=z3,cvc4,alt-ergo -j0
	# gnatprove -P spark/spark.gpr -U --prover=z3,cvc4,alt-ergo -j0


valgrind:
	valgrind --leak-check=full --max-stackframe=3800000 --show-reachable=yes ./tests/obj/perf

clean:
	gprclean -Ptests -r -q
