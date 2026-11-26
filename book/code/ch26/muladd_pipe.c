// muladd with optimization pragmas (Chapter 26). Several pragmas on one
// function or loop are allowed. A C compiler ignores them.
#define SIZE 16

int muladd(int a[SIZE], int b[SIZE])
{
#pragma HLS ARRAY_PARTITION variable=a type=cyclic factor=2
#pragma HLS ARRAY_PARTITION variable=b type=cyclic factor=2
    int temp = 0;

    muladd_loop: for (int i = 0; i < SIZE; i++) {
#pragma HLS PIPELINE II=1
#pragma HLS UNROLL factor=4
        temp += a[i] * b[i];
    }

    return temp;
}
