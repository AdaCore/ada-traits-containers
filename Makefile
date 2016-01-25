all:
	@mkdir -p tests/generated
	python ./generate_test.py
	gprbuild -m -p -Ptests -j0

run_test:
	./tests/obj/perf

# Check that our files are compatible with SPARK mode
check_spark:
	gnatprove -Pspark/spark.gpr -U --mode=check

# Prove our files
prove:
	gnatprove -Pspark/spark.gpr -u use_sets.adb use_lists.adb use_maps.adb use_ordered_sets.adb --dbg-proof-only --level=1 --steps=300
	# gnatprove -P spark/spark.gpr -U --level=1 --steps=300


valgrind:
	valgrind --leak-check=full --max-stackframe=3800000 --show-reachable=yes ./tests/obj/perf

clean:
	gprclean -Ptests -r -q
