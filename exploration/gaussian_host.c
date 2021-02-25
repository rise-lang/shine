#include <stdio.h>
#include "gaussian.c"


void printM(double* mat, int n, int m){
    for(int i=0; i<n; i++){
        for(int j=0; j<m; j++){
            printf("%f\t", mat[i*m + j]);
        }
        printf("\n");
    }
}

int main(void){
    double in[] = {
        1,2,3,4,5,6,7,8,
        2,3,4,5,6,7,8,9,
        3,4,5,6,7,8,9,10,
        4,5,6,7,8,9,10,11,
        5,6,7,8,9,10,11,12,
        6,7,8,9,10,11,12,13,
        7,8,9,10,11,12,13,14,
        8,9,10,11,12,13,14,15,
        
    };
    double weights[] = {
        2,4,5,4,2,
        4,9,12,9,4,
        5,12,15,12,5,
        4,9,12,9,4,
        2,4,5,4,2
        
    };
    double out[8*8];
    
    gaussian(out, in, weights);
    printM(out, 8, 8);
}
