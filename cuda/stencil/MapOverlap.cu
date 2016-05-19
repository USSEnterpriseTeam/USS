#ifndef __CUDACC__ 
#define __CUDACC__
#endif
#include "cuda_runtime.h"
#include "device_launch_parameters.h"
#include <cuda.h>
#include <device_functions.h>
#include <cuda_runtime_api.h>



#include <stdio.h>

__global__ void mapOverlap1D(int * in, int * out, int ray, int size) {
	__shared__ int tmp[9];

	int index = blockIdx.x * blockDim.x + threadIdx.x;
	int sIndex = threadIdx.x + ray;	
	if (threadIdx.x < ray) {
		if (index - ray > 0){
			tmp[sIndex - ray] = in[index - ray];
		}
		else{
			tmp[sIndex - ray] = in[size - ray + threadIdx.x];
		}
		if (index + blockDim.x >= size)
			tmp[sIndex + blockDim.x - ray] = in[threadIdx.x];
		else
			tmp[sIndex + blockDim.x - ray] = in[index + blockDim.x];
	}
	tmp[sIndex] = in[index];
	
	__syncthreads();

	int result = 0;
	for (int i=-ray; i <= ray; i++) {
		result += tmp[sIndex + i];
	}

	out[index] = result;
}

int main()
{
	int * host_in = NULL;
	int * host_out = NULL;

	int * kernel_in = NULL;
	int * kernel_out = NULL;

	int ray = 2;
	int N = 10;
	host_in = (int*)malloc(N*sizeof(int));
	host_out = (int*)malloc(N*sizeof(int));

	cudaMalloc(&kernel_in, N*sizeof(int));
	cudaMalloc(&kernel_out, N*sizeof(int));

	for (int i = 0; i < N; i++) { host_in[i] = 1+i; }

	cudaMemcpy(kernel_in, host_in, N*sizeof(int), cudaMemcpyHostToDevice);

	mapOverlap1D << < 2, 5 >> > (kernel_in, kernel_out, ray, 10);

	cudaMemcpy(host_out, kernel_out, N*sizeof(int), cudaMemcpyDeviceToHost);

	int res = 0;
	for (int i = 0; i < N; i++) {
		printf("%i ", host_out[i]);
		res += host_out[i];
	}
	printf("\n");
	for (int i = 0; i < N; i++) {
		printf("%i ", host_in[i]);
		res += host_in[i];
	}
	printf("\n");

	printf("Res = %i\n", res);

	system("PAUSE");
	return 0;
}