#include <R.h>
 #include <math.h>
 void g_zjbuquke ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = log10(p[0]*(x[2+i**k])+p[1]) ;
y[1+i**l] = log10(p[2]*(x[0+i**k]+x[2+i**k])) ;
y[2+i**l] = log10(x[1+i**k]) ; 
}
}