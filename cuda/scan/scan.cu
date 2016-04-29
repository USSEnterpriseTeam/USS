#include <iostream>

__global__ void scan(float *g_odata, float *g_idata, int n)  {  
    extern __shared__ float temp[]; 

    int thid = threadIdx.x;
    int offset = 1;
    temp[2*thid] = g_idata[2*thid]; 
    temp[2*thid+1] = g_idata[2*thid+1];  
	
    for (int d = n >> 1; d > 0; d >>= 1) {
	
	__syncthreads();  
	if (thid < d) {

	    int ai = offset*(2*thid+1)-1;  
	    int bi = offset*(2*thid+2)-1;  	    
	    temp[bi] += temp[ai];  
	}	
	offset *= 2;
    }


    if (thid == 0) { temp[n - 1 + CONFLICT_FREE_OFFSET(n - 1)] = 0;}  
 
    for (int d = 1; d < n; d *= 2) {  
	offset >>= 1;  

	__syncthreads();  
	if (thid < d) {
	    int ai = offset*(2*thid+1)-1;  
	    int bi = offset*(2*thid+2)-1;  
	    float t = temp[ai];  
	    temp[ai] = temp[bi];  
	    temp[bi] += t;   
	}
	
    }
    
    __syncthreads();
    
    g_odata[2*thid] = temp[2*thid]; 
    g_odata[2*thid+1] = temp[2*thid+1];  
}


int main() {
    const int n = 10;
    int bytes = n * sizeof(float);
    float *h_a = malloc(bytes), *h_b = malloc(bytes);
    
    for(int i = 0; i < n; i++) {h_a[i] = 1;}
    float * d_a;
    float * d_b;

    cudaMalloc((void**)&d_a, bytes);
    cudaMalloc((void**)&d_b, bytes);

    cudaMemcpy(d_a, h_a, bytes, cudaMemcpyHostToDevice);
    scan<<<1, n, n>>>(d_a, d_b, n);
    cudaMemcpy(h_b, d_b, bytes, cudaMemcpyDeviceToHost);


    for(int i = 0; i < n; i++)
	std::cout << h_a[i] << " ";
    std::cout << std::endl;
    for(int i = 0; i < n; i++)
	std::cout << h_b[i] << " ";
    std::cout << std::endl;
    
    cudaFree(d_a);
    cudaFree(d_b);
    free(h_a);
    free(h_b);   
}

