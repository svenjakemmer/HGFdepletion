#include <R.h>
 #include <math.h>
 void p_40_deriv_tw262u8z ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0])/exp(p[1]) ;
y[3+i**l] = exp(p[0]) ;
y[11+i**l] = -(exp(p[0])*exp(p[1])/pow(exp(p[1]),2.0)) ;
y[15+i**l] = exp(p[1]) ;
y[23+i**l] = -exp(p[2]) ;
y[38+i**l] = exp(p[3]) ;
y[50+i**l] = exp(p[4]) ;
y[62+i**l] = exp(p[5]) ;
y[74+i**l] = exp(p[6]) ;
y[86+i**l] = exp(p[7]) ;
y[98+i**l] = exp(p[8]) ; 
}
}