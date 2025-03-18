! example of a single source code that be compiled to execute
! on either the host or the device by using the sentinel !@cuf in addition
! to the !$cuf directive

! comment out !@cuf if program to be run on any other fortran compiler
! when compiling use: 
! nvfortran -o sentinel2 sentinel2.f90 
! will not print out the "Running Cuda version.." message, but will compile 
! and run(Program Passed message)
! other than cudafor


program managedCUF
    !@cuf use cudafor
    implicit none 
    integer, parameter :: nx=1024, ny=512
    integer  :: a(nx,ny)
    !@cuf attributes(managed):: a 
    integer  :: b, i, j 
    
    a = 1
    b = 3 
    !$cuf kernel do (2) <<<*,*>>>
    do j=1, ny 
        do i=1,nx 
            a(i, j)=a(i,j)+b 
        end do 
    end do 
    ! @cuf  i = cudaDeviceSynchronize()

    !@cuf print *, "Running CUDA Version..."
    if(any(a /= 4)) then
        print*, " **** PROGRAM FAILED **** "
    else
        print*, " **** PROGRAM PASSED **** "
    endif 
end program managedCUF 
