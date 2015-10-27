all:
	python ./generate_test.py
	gprbuild -Ptests
	./tests/obj/perf

clean:
	gprclean -Ptests -r -q
