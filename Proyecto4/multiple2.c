#include <stdio.h>

int main(){
 int x,y,z,w,t;
 w = 0;
 t = 0;

 printf("Ingrese tres números para la asignación múltiple: ");
 scanf("%d",&x);
 scanf("%d",&y);
 scanf("%d",&z);

 w = x;
 t = y;

 x = t;
 y = w + y + z;
 z =  t + w;

 printf("x vale ahora %d, ",x);
 printf("y vale ahora %d, ",y);
 printf("z vale ahora %d.",z);
}
