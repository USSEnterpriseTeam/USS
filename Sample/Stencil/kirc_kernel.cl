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
__kernel void spoc_dummy ( __global float* a, int ray, int n, __global float* b ) {
  __local float spoc_var0[21];
  int s_index;
  int id_x;
  int result;
  int id_l;
  int i;
  id_l = get_local_id(0); 
  s_index = ray + get_local_id(0); 
  id_x = get_global_id(0); 
  if (id_x < n){
    if (id_l < ray){
      if (id_x - ray > 0){
        spoc_var0[s_index - ray] = a[id_x - ray];
      }
      else{
        spoc_var0[s_index - ray] = a[n - ray + id_l];
      }
      ; 
      if (id_x + get_local_size(0) >= n){
        spoc_var0[s_index + get_local_size(0)] = a[id_l];
      }
      else{
        spoc_var0[s_index + get_local_size(0)] = a[id_x + get_local_size(0)];
      }
      
    }    ; 
    spoc_var0[s_index] = a[id_x]; 
    barrier(CLK_GLOBAL_MEM_FENCE); 
    result = 0; 
    i = 0 - ray; 
    while (i <= ray){
      result = result + spoc_var0[s_index + i]; 
      i = i + 1;}; 
    b[id_x] = result; 
    ;
  }
  else{
    barrier(CLK_GLOBAL_MEM_FENCE);
  }
  
  
  
  
  
  
  ;
  
  
}