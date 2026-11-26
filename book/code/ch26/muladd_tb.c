// Testbench for muladd. Used by csim_design and cosim_design: it must
// return 0 when the result is correct.
// Inputs are 12-bit (like ADC samples) so that the int sum cannot overflow:
// with 16-bit inputs, as in the 2026 version, a*b overflows int (undefined in C).
#include <stdio.h>
#include <stdlib.h>
#define SIZE 16

int muladd(int a[SIZE], int b[SIZE]);

int main(void)
{
    int a[SIZE], b[SIZE];
    int expected = 0;

    for (int i = 0; i < SIZE; i++) {
        a[i] = rand() & 0xfff;   // 0..4095
        b[i] = rand() & 0xfff;
        expected += a[i] * b[i]; // < 2^31
    }

    int result = muladd(a, b);
    printf("result = %d, expected = %d\n", result, expected);
    if (result == expected) {
        printf("PASS\n");
        return 0;
    }
    printf("FAIL\n");
    return 1;
}
