#include <stdio.h>

int main() {
    int n, fibn0, fibn1, i, aux;
   fibn0 = 1;
   fibn1 = 1;
   aux = 0;
   i = 2;
   printf("Ingrese el número");
   scanf("%d",&n);
   while (i<n || i==n){
     aux = fibn1;
    fibn1 = fibn1 + fibn0;
    fibn0 = aux;
    i = i + 1;
   }
    printf("Fib(%d) = %d\n", n, fibn1);
    return 0;
}


