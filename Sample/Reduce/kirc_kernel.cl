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
__kernel void spoc_dummy ( __global float* a, int n_i, __global float* b ) {
  __local float spoc_var0[8192];
  int idx;
  int idl;
  int n;
  float n2;
  float n3;
  int pos;
  idx = get_global_id(0); 
  idl = get_local_id(0); 
  spoc_var0[idl] = a[idx]; 
  barrier(CLK_GLOBAL_MEM_FENCE); 
  n = 8192; 
  n2 = n; 
  n3 = n; 
  pos = 0; 
  while (n3 > 0){
    n3 = floor (n2 / 2) ; 
    if ((int)n2 % 2 == 0){
      n2 = n3;
    }
    else{
      n2 = n3 + 1;
    }
    ; 
    if (idl < n2){
      if (((n2 < n3 || n2 > n3) && idl > 0)){
        pos = idl + n3; 
        spoc_var0[idl] = spoc_var0[idl] + spoc_var0[pos];;
      }
      else{
        if (n2 == n3){
          pos = idl + n2; 
          spoc_var0[idl] = spoc_var0[idl] + spoc_var0[pos];
        }        ;
      }
      
    }    ;}
  
  
  
  
  
  
  ;
  barrier(CLK_GLOBAL_MEM_FENCE); 
  if (idx == 0){
    b[0] = spoc_var0[0]; 
    
  }  ; 
  if ((idl == 0 && idx > 0)){
    b[0] = b[0] + spoc_var0[0];; 
    
  }  ; 
  
  
}