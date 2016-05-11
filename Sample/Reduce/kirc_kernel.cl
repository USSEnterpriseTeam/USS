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
__kernel void spoc_dummy ( __global float* a, __global float* n ) {
  int idx;
  int n2;
  int n3;
  int pos;
  int b;
  idx = get_global_id(0) ;
  n2 = n ;
  n3 = 0 ;
  pos = 0 ;
  for (int i = 0; i <= 4; i++){
    n3 = n2 / 2 ;
    if (n2 % 2 == 0){
      n2 = n3 + 1
    }
    else{
      n2 = n3
    }
     ;
    if (idx < n2){
      if (n2 < n3 || n2 > n3 && idx > 0){
        pos = idx + n3 ;
        b = a[idx] + 1.f
      }
      else{
        pos = idx + n2 ;
        b = a[idx] + 1.f
      }
      
    }    ;}
  
  
  
  
  
}