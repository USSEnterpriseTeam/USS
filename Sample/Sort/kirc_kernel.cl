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
__kernel void spoc_dummy ( __global float* a, int j, int k, int n ) {
  int id_x;
  int ixj;
  float tmp;
  int test;
  id_x = get_global_id(0); 
  if (id_x < n){
    ixj = id_x^j; 
    if ((ixj >= id_x && ixj < n)){
      if (a[id_x] > a[ixj]){
        test = 0.f;
      }
      else{
        test = 1.f;
      }
      ; 
      if (((id_x&k) == (0) && test)){
        tmp = a[id_x]; 
        a[id_x] = a[ixj]; 
        a[ixj] = tmp;;;
      }      ; 
      if ((!((id_x&k) == (0)) && !(test))){
        tmp = a[id_x]; 
        a[id_x] = a[ixj]; 
        a[ixj] = tmp;;;
      }      ;;;
    }    ;;
  }  ;
  
  
  
  ;
  
  
}