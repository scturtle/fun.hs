//#include <stdbool.h>
#include <math.h>
#define bool unsigned char
#ifdef DEBUG
  #include <stdio.h>
  #define MAX 100000000
  bool isprime[MAX];
#endif

void sieve(int n, bool *isprime){
    int i, j, m = sqrt(n);
    for(i = 0; i < n; i++)
        isprime[i] = 1;
    isprime[0] = isprime[1] = 0;
    for(int i = 2; i < m; i++){
        if(isprime[i]){
            for(j = i*i; j < n; j += i)
                isprime[j] = 0;
        }
    }
}

#ifdef DEBUG
int main(){
    sieve(MAX, isprime);
    int i, c = 0;
    for(i = 0; i < MAX; i++)
        if(isprime[i]) ++c;
    printf("%d", c);
}
#endif
