#ifndef __FLOAT64_EXTENSION__ 
#define __FLOAT64_EXTENSION__ 
#if defined(cl_khr_fp64)  // Khronos extension available?
#pragma OPENCL EXTENSION cl_khr_fp64 : enable
#elif defined(cl_amd_fp64)  // AMD extension available?
#pragma OPENCL EXTENSION cl_amd_fp64 : enable
#endif
double spoc_dadd ( double a, double b );
double spoc_dminus ( double a, double b );
double spoc_dmul ( double a, double b );
double spoc_ddiv ( double a, double b );
double spoc_dadd ( double a, double b ) { return (a + b);}
double spoc_dminus ( double a, double b ) { return (a - b);}
double spoc_dmul ( double a, double b ) { return (a * b);}
double spoc_ddiv ( double a, double b ) { return (a / b);}
#endif
float spoc_fadd ( float a, float b );
float spoc_fminus ( float a, float b );
float spoc_fmul ( float a, float b );
float spoc_fdiv ( float a, float b );
int logical_and (int, int);
int spoc_powint (int, int);
int spoc_xor (int, int);
float spoc_fadd ( float a, float b ) { return (a + b);}
float spoc_fminus ( float a, float b ) { return (a - b);}
float spoc_fmul ( float a, float b ) { return (a * b);}
float spoc_fdiv ( float a, float b ) { return (a / b);}
int logical_and (int a, int b ) { return (a & b);}
int spoc_powint (int a, int b ) { return ((int) pow (((float) a), ((float) b)));}
int spoc_xor (int a, int b ) { return (a^b);}
/************* CUSTOM TYPES ******************/


/************* FUNCTION PROTOTYPES ******************/
/************* FUNCTION DEFINITIONS ******************/
__kernel void spoc_dummy ( __global double* a, __global double* b, int n, __global double* c ) {
  int x;
  x = get_global_id(0); 
  if (x < n) {
    c[x] = spoc_dmul (a[x],b[x]) ;;
  }  ;
  
}