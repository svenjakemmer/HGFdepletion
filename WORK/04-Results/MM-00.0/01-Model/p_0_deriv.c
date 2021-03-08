#include <R.h>
 #include <math.h>
 void p_0_deriv_7rroui1o ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0])/exp(p[1]) ;
y[7+i**l] = exp(p[0]) ;
y[20+i**l] = -(exp(p[0])*exp(p[1])/pow(exp(p[1]),2.0)) ;
y[28+i**l] = exp(p[1]) ;
y[49+i**l] = exp(p[2]) ;
y[70+i**l] = exp(p[3]) ;
y[91+i**l] = exp(p[4]) ;
y[112+i**l] = exp(p[5]) ;
y[133+i**l] = exp(p[6]) ;
y[154+i**l] = exp(p[7]) ;
y[175+i**l] = exp(p[8]) ;
y[196+i**l] = exp(p[9]) ;
y[217+i**l] = exp(p[10]) ;
y[238+i**l] = exp(p[11]) ;
y[259+i**l] = exp(p[12]) ; 
}
}