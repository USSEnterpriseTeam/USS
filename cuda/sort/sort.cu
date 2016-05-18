#include <stdio.h>
#include <math.h>
#include <stdlib.h>

#define MAX_LEVEL 300

__global__ void (int * values, int n) {
    int pivot, L, R;
    int idx = threadIdx.x + blockIdx.x * blockDim.x;
    int start[MAX_LEVEL];
    int end[MAX_LEVEL];
    start[idx] = idx;
    end[idx] = N - 1;
    while(idx >= 0) {
	L = start[idx];
	R = end[idx];
	if(L < R) {
	    pivot = values[L];
	    while(L < R) {
		while(values[R] >= pivot && L < R) R--;
		if(L < R) values[L++] = values[R];
		while (values[L] < pivot && L < R) L++;
		if(L < R) values[R--] = values[L];
	    }
	    values[L] = pivot;
	    start[idx + 1] = L + 1;
	    end[idx + 1] = end[idx];
	    end[idx++] = L;
	    if(end[idx] - start[idx] > end[idx - 1] - start[idx - 1]) {
		int tmp = start[idx];
		start[idx] = start[idx - 1];
		start[idx - 1] = tmp;
		tmp = end[idx];
		end[idx] = end[idx - 1];
		end[idx - 1] = tmp;
	    }
	} else idx --;		
    }    
}



int main() {
    int * h_values = (int*)malloc(10 * sizeof(int));
    int * d_values;
    printf("[");
    for(int i = 0; i < 10; i++) {
	h_values[i] = rand()%10;
	printf("%d, ", h_values[i]);
    }
    printf("]\n");
    
    cudaMalloc((void**)&d_values, 10 * sizeof(int));
    cudaMemcpy(d_values, h_values, 10 * sizeof(int), cudaMemcpyHostToDevice);
    quicksort<<<10, 1>>>(d_values, 10);
    cudaMemcpy(h_values, d_values, 10 * sizeof(int), cudaMemcpyDeviceToHost);
    printf("[");
    for(int i = 0; i < 10; i++) {
	printf("%d, ", h_values[i]);
    }
    printf("]\n");
    return 0;
}
