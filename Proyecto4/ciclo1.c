#include <stdio.h>

int main() {
    int x, y, i;
    x = 0;
    y = 0;
    i = 0;
    
    printf("Inserte el dividendo: ");
    scanf("%d", &x);
    printf("Inserte el divisor: ");
    scanf("%d", &y);

    i = 0;
    while (y <= x) {
	 x=x-y;
         i=i+1;
    }
    return 0;
}
