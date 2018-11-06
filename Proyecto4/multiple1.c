#include <stdio.h>

int main(){
 int x,y,z;
 z = 0;
 printf("Ingrese dos valores para para asignarle, x + 1 y x+y respectivamente.");
 scanf("%d",&x);
 scanf("%d",&y);

 z = x;
 x = x + 1;
 y = z + y;

 printf("El valor del primer número es ahora %d ,",x);
 printf(" el del segundo número es ahora %d",y);
}
