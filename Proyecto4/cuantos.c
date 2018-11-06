#include <stdio.h>
struct comp_t {
int menores;
int iguales;
int mayores;
};
struct comp_t cuantos(int a[], int tam, int elem){
  int i = 0;
  int menores = 0;
  int mayores = 0;
  int iguales = 0;
  while(i < tam){
    if(a[i] == elem){
       iguales = iguales + 1;
    }
    if(a[i] < elem){
       menores = menores + 1;
    }
    if(a[i] > elem){
       mayores = mayores + 1;
    }
   i = i + 1;
  }
}
int main(){
    int n;
    printf("Indique la cantidad de elementos del arreglo");
    scanf("%d",&n);
    int elem;
    printf("Ingrese el número con el cual usted desea comparar:");
    scanf("%d",&elem);
    int a[n];
    int i = 0;
    while (i < n) {
	printf("Inserte el elemento de la posición %d del arreglo: ", i);
	scanf("%d", &a[i]);
	i++;
    }
    printf("Usted insertó el siguiente arreglo: ");
    i = 0;
    while (i < n) {
	printf("%d ", a[i]);
	i++;
    }
   struct comp_t res = cuantos(a, n, elem);
   printf("La cantidad de elementos menores, iguales y mayores a %d es: %d, %d,%d respectivamente.",elem,res.menores, res.iguales,res.mayores);
   return 0;
}
