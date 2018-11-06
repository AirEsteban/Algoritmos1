#include <stdio.h>
#include <stdbool.h>

int main() {
    int x, i;
    bool res = true;
    i=2;
    printf("Ingrese el número a validar: ");
    scanf("%d",&x);
    while(i<x && res){
      res = res && !(x % i == 0);
      i = i + 1;
    }

    if (res && x > 1) {
	printf("El número es primo. \n");
    } else {
	printf("El número no es primo. \n");
    }

    return 0;
}
