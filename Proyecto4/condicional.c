#include <stdio.h>

/* Para leer del teclado booleanos, no usar scanf, sino
   directamente hacer la asignación correspondiente.
*/

int main() {
    int x, y, z, m;
    x = 0;
    y = 0;
    z = 0;
    m = 0;
    
    printf("Inserte tres números enteros separados por espacios:\n");
    scanf("%d %d %d", &x, &y, &z);

    //completar
    if(x < y){
      m = x;
    }else{
      m = y;
    }

    if(m < z){
      m = m;
    }else{
     m = z;
    }
    printf("El mínimo entre los tres es %d \n", m);
    return 0;
} 
