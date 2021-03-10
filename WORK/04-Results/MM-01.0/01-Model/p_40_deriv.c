#include <R.h>
 #include <math.h>
 void p_40_deriv_4al4dtuv ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = exp(p[0])/exp(p[1]) ;
y[4+i**l] = exp(p[0]) ;
y[13+i**l] = -(exp(p[0])*exp(p[1])/pow(exp(p[1]),2.0)) ;
y[18+i**l] = exp(p[1]) ;
y[27+i**l] = -exp(p[2]) ;
y[45+i**l] = exp(p[3]) ;
y[59+i**l] = exp(p[4]) ;
y[73+i**l] = exp(p[5]) ;
y[87+i**l] = exp(p[6]) ;
y[101+i**l] = exp(p[7]) ;
y[115+i**l] = exp(p[8]) ;
y[129+i**l] = exp(p[9]) ; 
}
}