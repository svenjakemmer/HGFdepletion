#include <R.h>
 #include <math.h>
 void p_40_deriv_3es1dmj8 ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0])/exp(p[1]) ;
y[7+i**l] = exp(p[0]) ;
y[20+i**l] = -(exp(p[0])*exp(p[1])/pow(exp(p[1]),2.0)) ;
y[28+i**l] = exp(p[1]) ;
y[41+i**l] = -exp(p[2]) ;
y[69+i**l] = exp(p[3]) ;
y[90+i**l] = exp(p[4]) ;
y[111+i**l] = exp(p[5]) ;
y[132+i**l] = exp(p[6]) ;
y[153+i**l] = exp(p[7]) ;
y[174+i**l] = exp(p[8]) ;
y[195+i**l] = exp(p[9]) ;
y[216+i**l] = exp(p[10]) ;
y[237+i**l] = exp(p[11]) ;
y[258+i**l] = exp(p[12]) ;
y[279+i**l] = exp(p[13]) ; 
}
}