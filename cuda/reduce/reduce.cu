#include <iostream>
#include <stdio.h>

using namespace std;

__global__ void reduce (  float* a, int n, float* b ) {
  __shared__ float spoc_var0[10];
  int idx;
  int n2;
  int n3;
  int pos;
  idx = blockIdx.x*blockDim.x+threadIdx.x ;
  spoc_var0[idx] = a[idx] ;
  n2 = n ;
  n3 = n ;
  pos = 0 ;
  while (n3 > 0) {
    n3 = n2 / 2 ;
    if (n2 % 2 == 0){
      n2 = n3;
    }
    else{
      n2 = n3 + 1;
    }
     
    if (idx < n2){
      if (((n2 < n3 || n2 > n3) && idx > 0)){
        pos = idx + n3 ;
        spoc_var0[idx] = spoc_var0[idx] + spoc_var0[pos];;
      }
      else if(n2==n3){
        pos = idx + n2 ;
        spoc_var0[idx] = spoc_var0[idx] + spoc_var0[pos];;
      }
      
    }    ;}
  
  
  
  
  ;
  b[0] = spoc_var0[0] ;
  
  
}

/*

__global__ void reduce(int * a, const int n) {
  int idx = blockDim.x * blockIdx.x + threadIdx.x;
  __shared__ int tmp[10];
  tmp[idx] = a[idx];
  //  __syncthreads();
  int N = n;
  for (int i = 0; i < 4; i++) {
    float N2 = (float)N / 2.0f;
    if (N%2!=0) {
      N = N2 + 1;
    } else {
      N = N2;
    }

    if (idx < N) {
      if (((N < N2 || N > N2) && idx != 0)) {
	tmp[idx] += tmp[idx + (int)N2];
      } else if(N == N2) {
	tmp[idx] += tmp[idx + N];
      }
    }
  }
  //  __syncthreads();
  a[0] = tmp[0];
}
*/
int main()
{
  const int n = 11;
  int bytes = n * sizeof(float);

  float h_a[n];
  for (int i = 0; i < n; i++) { h_a[i] = 1; }

  float * d_a;
  float * b;
  cudaMalloc((void**)&d_a, bytes);
  cudaMalloc((void**)&b, 4);

  cudaMemcpy(d_a, h_a, bytes, cudaMemcpyHostToDevice);

  reduce << < 1, n >> > (d_a, n, b);

  cudaMemcpy(h_a, b, 4, cudaMemcpyDeviceToHost);

  cout << "Res : " << h_a[0] << endl;

  cudaFree(d_a);

  return 0;
}
