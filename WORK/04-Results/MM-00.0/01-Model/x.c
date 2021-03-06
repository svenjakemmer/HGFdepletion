/** Code auto-generated by cOde 1.1.0 **/
#include <R.h> 
 #include <math.h> 

static double parms[16];
static double forc[0];
static double cons[0];
static double range[2];

#define nGridpoints 2 
#define nSplines 0 
#define precision 1e-05 

#define k_prod parms[0] 
 #define k_deg parms[1] 
 #define k_ass parms[2] 
 #define k_rec parms[3] 
 #define k_act parms[4] 
 #define k_int parms[5] 
 #define k_diss parms[6] 
 #define k_degint parms[7] 
 #define k_degHGF parms[8] 
 #define y0_0 parms[9] 
 #define y1_0 parms[10] 
 #define y2_0 parms[11] 
 #define y3_0 parms[12] 
 #define y4_0 parms[13] 
 #define y5_0 parms[14] 
 #define y6_0 parms[15] 
#define tmin range[0]
#define tmax range[1]


void x_initmod(void (* odeparms)(int *, double *)) {
	 int N=16;
	 odeparms(&N, parms);
}

void x_initforc(void (* odeforcs)(int *, double *)) {
	 int N=0;
	 odeforcs(&N, forc);
}

/** Derivatives (ODE system) **/
void x_derivs (int *n, double *t, double *y, double *ydot, double *RPAR, int *IPAR) {

	 double time = *t;

	 ydot[0] = 1.0*(k_prod)-1.0*(k_deg*y[0])-1.0*(k_ass*y[0]*y[1])+1.0*(k_rec*y[5]);
 	 ydot[1] = -1.0*(k_ass*y[0]*y[1]);
 	 ydot[2] = 1.0*(k_ass*y[0]*y[1])-2.0*(k_act*y[2]*y[2]);
 	 ydot[3] = 2.0*(k_act*y[2]*y[2])-1.0*(k_int*y[3]);
 	 ydot[4] = 1.0*(k_int*y[3])-1.0*(k_diss*y[4]);
 	 ydot[5] = 1.0*(k_diss*y[4])-1.0*(k_rec*y[5])-1.0*(k_degint*y[5]);
 	 ydot[6] = 1.0*(k_diss*y[4])-1.0*(k_degHGF*y[6]);

}

