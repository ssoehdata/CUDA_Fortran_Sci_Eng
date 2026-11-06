### Some basic terms and definitions:

filename.cuf - indicates a free-format CUDA Fortran program. 
             - to compile a fixed format program add the  line option -Mfixed
             - Fortran extensions (.F90 etc) can be compiled by adding the  -cuda 
                command line option when compiling. 

             

host   - CPU
device - GPU
kernel - A subroutine that executes on the device but is called from the host.

        call kernel<<<grid,block[,bytes][,streamid]>>>(arg1, arg2, ... ) 
<li>Cuda Stream</li>
  - a sequence of operations performed in order on the device. 
  
<li>attribute(global)</li>
  - indicatest that the code is to run on the device but is called from the host. 
  
<li>threadIdx</li>
  - built in variable for thread identification as an index of the array, allowing
            for concurrent execution on the GPU in CUDA Fortran.
            Thread indexes for each dimension start at one in CUDA Fortran.

<li>blockIdx</li>
  - built in thread block index variable.

<li>fine-grained parallelism</li>
  - where sequential threads modify adjacent elements of an array. 

The "_d" variable suffix is not required but is a convention for differentiating device from
    host variables in host code. 

<li>Execution configuration</li>
  - e.g. <<<1,n>>> specifies that the kernel is executed by n GPU threads.

<li>Core</li>
      - a thread processor, the basic computational unit on the GPU. A floating point unit.
       Thread processors are grouped into multiprocessors that contain a limited amount 
       of resources used by resident threads(registers and shared memory). 

<li>Thread block</li> - groups of threads assigned to a multiprocessor and do not   
               migrate once assigned. When a kernel is invoked, a grid of 
               thread blocks is launched.                           

    cudaDeviceSynchronize()
                        
      this routine blocks the host execution until the kernel has finished.
                          Note: believe it to have been deprecated since version 11.6.                           
                          
    -gpu=managed  
    
        compiler option to specify all allocatable variables in a code implicitly
                have the managed attribute.
