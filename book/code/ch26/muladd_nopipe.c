// muladd with loop pipelining switched off, to compare with the default (Chapter 26).
// Recent Vitis HLS releases pipeline simple loops automatically; PIPELINE off undoes that.
#define SIZE 16

int muladd(int a[SIZE], int b[SIZE])
{
    int temp = 0;

    muladd_loop: for (int i = 0; i < SIZE; i++) {
#pragma HLS PIPELINE off
        temp += a[i] * b[i];
    }

    return temp;
}
