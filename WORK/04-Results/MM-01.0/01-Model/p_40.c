#include <R.h>
 #include <math.h>
 void p_40_d7t5o0xq ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0])/exp(p[1]) ;
y[1+i**l] = 40.0-exp(p[2]) ;
y[4+i**l] = exp(p[0]) ;
y[5+i**l] = exp(p[1]) ;
y[6+i**l] = exp(p[3]) ;
y[7+i**l] = exp(p[4]) ;
y[8+i**l] = exp(p[5]) ;
y[9+i**l] = exp(p[6]) ;
y[10+i**l] = exp(p[7]) ;
y[11+i**l] = exp(p[8]) ;
y[12+i**l] = exp(p[9]) ; 
}
}