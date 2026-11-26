# C simulation without Vitis HLS (any laptop): builds and runs the testbench.
CFLAGS = -std=c99 -Wall -Wno-unknown-pragmas -Wno-unused-label -fsanitize=undefined
test:
	cc $(CFLAGS) -o tb_plain muladd.c muladd_tb.c && ./tb_plain
	cc $(CFLAGS) -o tb_pipe muladd_pipe.c muladd_tb.c && ./tb_pipe
	cc $(CFLAGS) -o tb_nopipe muladd_nopipe.c muladd_tb.c && ./tb_nopipe
	rm -f tb_plain tb_pipe tb_nopipe
