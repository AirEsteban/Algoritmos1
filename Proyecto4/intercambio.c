#include <stdio.h>

int main(){
 int x,y,z;
 z=0;
 printf("Ingrese primero x y luego y: ");
 scanf("%d",&x);
 scanf("%d",&y);
 
 z = x;
 x = y;
 y = z;

 printf("Ahora el valor de x es %d, ",x);
 printf("Y el valor de y es %d.",y);

 return 0;
}

