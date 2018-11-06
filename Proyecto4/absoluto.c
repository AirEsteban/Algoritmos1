#include <stdio.h>

int main(){
  int x;
  x = 0;
  printf("Por favor, ingrese el número para calcular su valor absoluto ");
  scanf("%d",&x);
  
  if(x<0){
   x = x * (-1);
  }else{
   x = x;
  }

 printf("El valor absoluto de su número es: %d",x);
  
 return 0;
}
