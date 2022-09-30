#include <stdio.h>


int main()
{
    int N = 12;
    for(int i=0;i<N;i++)
    {
        printf("%d\n",(N-i));
        if((N-i)%5==0)
            printf("Beep\n");
    }
    return 0;
}

