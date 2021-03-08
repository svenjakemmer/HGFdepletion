#include <R.h>
 #include <math.h>
 void p_40_deriv_hztuqi1t ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0])/exp(p[1]) ;
y[7+i**l] = exp(p[0]) ;
y[22+i**l] = -(exp(p[0])*exp(p[1])/pow(exp(p[1]),2.0)) ;
y[30+i**l] = exp(p[1]) ;
y[53+i**l] = exp(p[2]) ;
y[76+i**l] = exp(p[3]) ;
y[99+i**l] = exp(p[4]) ;
y[122+i**l] = exp(p[5]) ;
y[145+i**l] = exp(p[6]) ;
y[168+i**l] = exp(p[7]) ;
y[191+i**l] = exp(p[8]) ;
y[214+i**l] = exp(p[9]) ;
y[237+i**l] = exp(p[10]) ;
y[260+i**l] = exp(p[11]) ;
y[283+i**l] = exp(p[12]) ;
y[306+i**l] = exp(p[13]) ;
y[329+i**l] = exp(p[14]) ; 
}
}