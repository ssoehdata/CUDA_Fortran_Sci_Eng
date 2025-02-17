/*Thrust is a C++ template library that provides a set of parallel primitives
  such as scan, reduce, and sort.
  The first step is to create C wrapper functions that CUDA Fortran can interface with.
  Here are wrapper functions for the sort algorithm.
*/

#include <thrust/device_vector.h> 
#include <thrust/sort.h> 


extern "C" {
    void sort_int_wrapper(int *data, int N)
    {
        // C ptr to device_vector container
        thrust::device_ptr <int> dev_ptr(data);
        // Use device_ptr in Thrust sort algorithm
        thrust::sort(dev_ptr, dev_ptr+N);
    }

    // sort for float arrays 
    void sort_float_wrapper(float *data, int N)
    {
        thrust::device_ptr <float> dev_ptr(data);
        thrust::sort(dev_ptr, dev_ptr+N);
    }

    //Sort for double arrays 
    void sort_double_wrapper(double *data, int N)
    {
        thrust::device_ptr <double> dev_ptr(data);
        thrust::sort(dev_ptr, dev_ptr+N);
    }
}


