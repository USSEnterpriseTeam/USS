__device__ float spoc_fadd ( float a, float b ) { return (a + b);}
__device__ float spoc_fminus ( float a, float b ) { return (a - b);}
__device__ float spoc_fmul ( float a, float b ) { return (a * b);}
__device__ float spoc_fdiv ( float a, float b ) { return (a / b);}
__device__ int logical_and (int a, int b ) { return (a & b);}
__device__ int spoc_powint (int a, int b ) { return ((int) pow (((double) a), ((double) b)));}
__device__ int spoc_xor (int a, int b ) { return (a^b);}


#ifdef __cplusplus
extern "C" {
#endif

__global__ void spoc_dummy (  int* a,  int* b, int n ) {
  int x;
  x = blockIdx.x*blockDim.x+threadIdx.x ;
  if (x < n){
    b[x] = a[x] + 1;
  }  
  
}
#ifdef __cplusplus
}
#endif