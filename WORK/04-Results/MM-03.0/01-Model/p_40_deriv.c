#include <R.h>
 #include <math.h>
 void p_40_deriv_3u3e4cx5 ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0])/exp(p[1]) ;
y[3+i**l] = exp(p[0]) ;
y[10+i**l] = -(exp(p[0])*exp(p[1])/pow(exp(p[1]),2.0)) ;
y[14+i**l] = exp(p[1]) ;
y[25+i**l] = exp(p[2]) ;
y[36+i**l] = exp(p[3]) ;
y[47+i**l] = exp(p[4]) ;
y[58+i**l] = exp(p[5]) ;
y[69+i**l] = exp(p[6]) ; 
}
}