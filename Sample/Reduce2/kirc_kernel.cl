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
__kernel void spoc_dummy ( __global float* a, int n, __global float* b ) {
  __local float spoc_var0[8192];
  int start;
  int idl;
  int stride; 
  start = 2 * get_group_id(0) * get_local_size(0); 
  stride = get_local_size(0); 
  idl = get_local_id(0); 
  if (start + idl < n){
    spoc_var0[idl] = a[idl + start]; 
    ;
  }
  else{
    spoc_var0[idl] = 0; 
    ;
  }
  ; 
  if (start + idl + get_local_size(0) < n){
    spoc_var0[idl + get_local_size(0)] = a[start + idl + get_local_size(0)]; 
    ;
  }
  else{
    spoc_var0[idl + get_local_size(0)] = 0; 
    ;
  }
  ; 
  while (stride > 0){
    barrier(CLK_GLOBAL_MEM_FENCE); 
    if (idl < stride){
      spoc_var0[idl] = spoc_var0[idl] + spoc_var0[idl + stride];; 
      
    }    ; 
    stride = stride >> 1;}; 
  if (idl == 0){
    b[get_group_id(0)] = spoc_var0[0]; 
    
  }  ; 
  
  
  
  ;
  
  
}