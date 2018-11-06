#include <stdio.h>
int main() {
    int n = 0;
    printf("Ingrese un número para calcular la suma hasta éste: ");
    scanf("%d",&n);
    int suma_hasta(int n){
      int i = 0;
      int aux = 0;
      while(i <= n){ // bueno le hice hasta inclusive.
        aux = aux + i;
        i = i + 1; 
      }
      return aux;
    }

    if(n < 0){
      printf("Error, sólo se puede calcular con números positivos.");
    }else{
     printf("La suma hasta %d es %d \n", n, suma_hasta(n));
    }    
    return 0;
}
