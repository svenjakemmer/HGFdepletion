/** Code auto-generated by cOde 1.1.0 **/
#include <R.h> 
 #include <math.h> 

static double parms[35];
static double forc[0];
static double cons[0];
static double range[2];

#define nGridpoints 2 
#define nSplines 0 
#define precision 1e-05 

#define k_prod parms[0] 
 #define k_deg parms[1] 
 #define k_ass parms[2] 
 #define k_act parms[3] 
 #define k_degact parms[4] 
 #define y0_0 parms[5] 
 #define y1_0 parms[6] 
 #define y2_0 parms[7] 
 #define y3_0 parms[8] 
 #define y4_0 parms[9] 
 #define y5_0 parms[10] 
 #define y6_0 parms[11] 
 #define y7_0 parms[12] 
 #define y8_0 parms[13] 
 #define y9_0 parms[14] 
 #define y10_0 parms[15] 
 #define y11_0 parms[16] 
 #define y12_0 parms[17] 
 #define y13_0 parms[18] 
 #define y14_0 parms[19] 
 #define y15_0 parms[20] 
 #define y16_0 parms[21] 
 #define y17_0 parms[22] 
 #define y18_0 parms[23] 
 #define y19_0 parms[24] 
 #define y20_0 parms[25] 
 #define y21_0 parms[26] 
 #define y22_0 parms[27] 
 #define y23_0 parms[28] 
 #define y24_0 parms[29] 
 #define y25_0 parms[30] 
 #define y26_0 parms[31] 
 #define y27_0 parms[32] 
 #define y28_0 parms[33] 
 #define y29_0 parms[34] 
#define tmin range[0]
#define tmax range[1]


void x_s_initmod(void (* odeparms)(int *, double *)) {
	 int N=35;
	 odeparms(&N, parms);
}

void x_s_initforc(void (* odeforcs)(int *, double *)) {
	 int N=0;
	 odeforcs(&N, forc);
}

/** Derivatives (ODE system) **/
void x_s_derivs (int *n, double *t, double *y, double *ydot, double *RPAR, int *IPAR) {

	 double time = *t;

	 ydot[0] = 1.0*(k_prod)-1.0*(k_deg*y[0])-1.0*(k_ass*y[0]*y[1]);
 	 ydot[1] = -1.0*(k_ass*y[0]*y[1]);
 	 ydot[2] = 1.0*(k_ass*y[0]*y[1])-2.0*(k_act*y[2]*y[2]);
 	 ydot[3] = 2.0*(k_act*y[2]*y[2])-1.0*(k_degact*y[3]);
 	 ydot[4] = (-(k_deg+k_ass*y[1]))*(y[4])+(-(k_ass*y[0]))*(y[5]);
 	 ydot[5] = (-(k_ass*y[1]))*(y[4])+(-(k_ass*y[0]))*(y[5]);
 	 ydot[6] = (k_ass*y[1])*(y[4])+(k_ass*y[0])*(y[5])+(-(2.0*(k_act*y[2]+k_act*y[2])))*(y[6]);
 	 ydot[7] = (2.0*(k_act*y[2]+k_act*y[2]))*(y[6])+(-k_degact)*(y[7]);
 	 ydot[8] = (-(k_deg+k_ass*y[1]))*(y[8])+(-(k_ass*y[0]))*(y[9]);
 	 ydot[9] = (-(k_ass*y[1]))*(y[8])+(-(k_ass*y[0]))*(y[9]);
 	 ydot[10] = (k_ass*y[1])*(y[8])+(k_ass*y[0])*(y[9])+(-(2.0*(k_act*y[2]+k_act*y[2])))*(y[10]);
 	 ydot[11] = (2.0*(k_act*y[2]+k_act*y[2]))*(y[10])+(-k_degact)*(y[11]);
 	 ydot[12] = (k_ass*y[1])*(0.0)+(k_ass*y[0])*(0.0)+(-(2.0*(k_act*y[2]+k_act*y[2])))*(y[12]);
 	 ydot[13] = (2.0*(k_act*y[2]+k_act*y[2]))*(y[12])+(-k_degact)*(y[13]);
 	 ydot[14] = (2.0*(k_act*y[2]+k_act*y[2]))*(0.0)+(-k_degact)*(y[14]);
 	 ydot[15] = (-(k_deg+k_ass*y[1]))*(y[15])+(-(k_ass*y[0]))*(y[16])+1.0;
 	 ydot[16] = (-(k_ass*y[1]))*(y[15])+(-(k_ass*y[0]))*(y[16]);
 	 ydot[17] = (k_ass*y[1])*(y[15])+(k_ass*y[0])*(y[16])+(-(2.0*(k_act*y[2]+k_act*y[2])))*(y[17]);
 	 ydot[18] = (2.0*(k_act*y[2]+k_act*y[2]))*(y[17])+(-k_degact)*(y[18]);
 	 ydot[19] = (-(k_deg+k_ass*y[1]))*(y[19])+(-(k_ass*y[0]))*(y[20])-y[0];
 	 ydot[20] = (-(k_ass*y[1]))*(y[19])+(-(k_ass*y[0]))*(y[20]);
 	 ydot[21] = (k_ass*y[1])*(y[19])+(k_ass*y[0])*(y[20])+(-(2.0*(k_act*y[2]+k_act*y[2])))*(y[21]);
 	 ydot[22] = (2.0*(k_act*y[2]+k_act*y[2]))*(y[21])+(-k_degact)*(y[22]);
 	 ydot[23] = (-(k_deg+k_ass*y[1]))*(y[23])+(-(k_ass*y[0]))*(y[24])-(y[0]*y[1]);
 	 ydot[24] = (-(k_ass*y[1]))*(y[23])+(-(k_ass*y[0]))*(y[24])-(y[0]*y[1]);
 	 ydot[25] = (k_ass*y[1])*(y[23])+(k_ass*y[0])*(y[24])+(-(2.0*(k_act*y[2]+k_act*y[2])))*(y[25])+y[0]*y[1];
 	 ydot[26] = (2.0*(k_act*y[2]+k_act*y[2]))*(y[25])+(-k_degact)*(y[26]);
 	 ydot[27] = (k_ass*y[1])*(0.0)+(k_ass*y[0])*(0.0)+(-(2.0*(k_act*y[2]+k_act*y[2])))*(y[27])-(2.0*(y[2]*y[2]));
 	 ydot[28] = (2.0*(k_act*y[2]+k_act*y[2]))*(y[27])+(-k_degact)*(y[28])+2.0*(y[2]*y[2]);
 	 ydot[29] = (2.0*(k_act*y[2]+k_act*y[2]))*(0.0)+(-k_degact)*(y[29])-y[3];

	 for(int i=  0 ; i <  10 ; ++i) RPAR[i] = 0;
}

