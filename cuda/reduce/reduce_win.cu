#include <iostream>
#include <stdio.h>

using namespace std;

__global__ void reduce(int * a, const int n) {
  int pos = blockDim.x * blockIdx.x + threadIdx.x;
  int N = n;
  for (int i = 0; i < 4; i++) {
    float N2 = (float)N / 2.0f;
    if (N2 != (float)((int)N2)) {
      N = N2 + 1;
    } else {
      N = N2;
    }

    if (pos < N) {
      if (N != N2 && pos != 0) {
	a[pos] += a[pos + (int)N2];
      } else if(N == N2) {
	a[pos] += a[pos + N];
      }
    }
  }

}

int main()
{
  const int n = 10;
  int bytes = n * sizeof(int);

  int h_a[n];
  for (int i = 0; i < n; i++) { h_a[i] = 1; }

  int * d_a;
  cudaMalloc((void**)&d_a, bytes);

  cudaMemcpy(d_a, h_a, bytes, cudaMemcpyHostToDevice);

  reduce << < 1, n >> > (d_a, n);

  cudaMemcpy(h_a, d_a, bytes, cudaMemcpyDeviceToHost);

  for (int i = 0; i < n; i++) { cout << h_a[i] << ", "; } cout << endl;
  cout << "Res : " << h_a[0] << endl;

  cudaFree(d_a);

  return 0;
}
