#define SIZE 16

int muladd(int a[SIZE], int b[SIZE])
{
    int temp=0;

    muladd_loop:for (int i=0; i<SIZE; i++) {
        temp += a[i] * b[i];
    }

return temp;

}
