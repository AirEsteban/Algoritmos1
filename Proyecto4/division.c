#include <stdio.h>

struct div_t {
    int cociente;
    int resto;
};

struct div_t division(int x, int y) {
    int cociente = x/y;
    int resto = x % y;
}

int main() {
    int x = 0;
    int y = 1;
 
    printf("Ingrese dividendo y divisor, respectivamente: ");
    scanf("%d",&x);
    scanf("%d",&y);
    if(y ==0){
      printf("Error, no se puede efectuar la división por 0.");
    }else{
      struct div_t res = division(x, y);
      printf("El cociente es %d y el resto %d\n", res.cociente, res.resto);
    }
    return 0;
}
