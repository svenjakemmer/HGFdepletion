#include <R.h>
 #include <math.h>
 void g_deriv_spqktgli ( double * x, double * y, double * p, int * n, int * k, int * l ) {
 for(int i = 0; i< *n; i++) {
 y[0+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[6+i**k]) ;
y[1+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[4+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[6+i**k]) ;
y[2+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[5+i**k]) ;
y[3+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[10+i**k]) ;
y[4+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[8+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[10+i**k]) ;
y[5+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[9+i**k]) ;
y[6+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[14+i**k]) ;
y[7+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[12+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[14+i**k]) ;
y[8+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[13+i**k]) ;
y[9+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[18+i**k]) ;
y[10+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[16+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[18+i**k]) ;
y[11+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[17+i**k]) ;
y[12+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[22+i**k])+(x[2+i**k])/((p[0]*(x[2+i**k])+p[1])*log(10.0)) ;
y[13+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[20+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[22+i**k]) ;
y[14+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[21+i**k]) ;
y[15+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[26+i**k])+1.0/((p[0]*(x[2+i**k])+p[1])*log(10.0)) ;
y[16+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[24+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[26+i**k]) ;
y[17+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[25+i**k]) ;
y[18+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[30+i**k]) ;
y[19+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[28+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[30+i**k])+(x[0+i**k]+x[2+i**k])/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)) ;
y[20+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[29+i**k]) ;
y[21+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[34+i**k]) ;
y[22+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[32+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[34+i**k]) ;
y[23+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[33+i**k]) ;
y[24+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[38+i**k]) ;
y[25+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[36+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[38+i**k]) ;
y[26+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[37+i**k]) ;
y[27+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[42+i**k]) ;
y[28+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[40+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[42+i**k]) ;
y[29+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[41+i**k]) ;
y[30+i**l] = (p[0]/((p[0]*(x[2+i**k])+p[1])*log(10.0)))*(x[46+i**k]) ;
y[31+i**l] = (p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[44+i**k])+(p[2]/(p[2]*(x[0+i**k]+x[2+i**k])*log(10.0)))*(x[46+i**k]) ;
y[32+i**l] = (1.0/(x[1+i**k]*log(10.0)))*(x[45+i**k]) ; 
}
}