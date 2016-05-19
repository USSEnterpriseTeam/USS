#include <stdio.h>
#include <stdlib.h>

__global__ void total(float * input, float * output, int len) {
  __shared__ float partialSum[2 * 1024];
  unsigned int t = threadIdx.x, start = 2 * blockIdx.x * blockDim.x;
    
    if (start + t < len)
       partialSum[t] = input[start + t];
    else
      partialSum[t] = 0;
    if (start + blockDim.x + t < len)
      partialSum[blockDim.x + t] = input[start + blockDim.x + t];
    else
       partialSum[blockDim.x + t] = 0;

    for (unsigned int stride = blockDim.x; stride >= 1; stride >>= 1) {
       __syncthreads();
       if (t < stride)
          partialSum[t] += partialSum[t+stride];
    }

    if (t == 0)
       output[blockIdx.x] = partialSum[0];
}


int main() {
  int N = 2000;
  float * a = (float*) malloc(sizeof(int) * N);
  float * b = (float*) malloc(sizeof(int) * N);
  float * d_b, *d_a;

  cudaMalloc((void**)&d_a, sizeof(int) * N);
  cudaMalloc((void**)&d_b, sizeof(int) * N);
  for(int i = 0; i < N; i++) { a[i] = 1.0f; b[i] = 0.0f; }
  cudaMemcpy(d_a, a, sizeof(float) * N, cudaMemcpyHostToDevice);
  cudaMemcpy(d_b, b, sizeof(float) * N, cudaMemcpyHostToDevice);

  double d = log(N - 1) / log(2);
  
  int threads = min((int)pow(2, (int)d + 1), 1024);
  printf("%d", threads);
  int blocks = max((int)pow(2, (int)d + 1) / threads, 1);
  printf("\n%d", blocks);
  total<<<blocks, threads>>> (d_a, d_b, N);

  
  cudaMemcpy(a, d_b, sizeof(float) * blocks, cudaMemcpyDeviceToHost);
  for(int i = 0; i < blocks; i++)
    printf("\n %f \n", a[i]);
}
