all:
	python ./generate_test.py
	gprbuild -p -Ptests
	./tests/obj/perf

valgrind:
	valgrind --leak-check=full --max-stackframe=3800000 --show-reachable=yes ./tests/obj/perf

clean:
	gprclean -Ptests -r -q
