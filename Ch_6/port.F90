! three preprocessors #ifdef _CUDA statements are used here: 
! one to include the cudafor module 
! one to add the managed attribute to the arrays
! and one to include the device synchronization also print a statement
! indicating that the code is executing on the GPU. 

! When the source code is in a file with a .F90 extension, the preprocessor 
! is invoked automatically, and the code is compiled as normal Fortran:

program main 
#ifdef _CUDA 
    use cudafor 
#endif 
    implicit none 
    integer, parameter :: n=8 
    real :: a(n), b(n)
#ifdef _CUDA 
    attributes(managed) :: a, b 
#endif 
    integer :: i 

    !$cuf kernel do <<<*,*>>>
    do i = 1, n 
        a(i) = i+1 
    enddo 

    !$cuf kernel do <<<*,*>>> 
    do i = 1, n 
        b(i) = a(i)+1 
    enddo 

#ifdef _CUDA 
    i = cudaDeviceSynchronize()
    print *, 'GPU version'
#endif 

    print *, a 
    print *, b 
end program main 
