! CUDA Fortran version of modparam code to demonstrate porting.
! compile with nvfortran 

! changes were made to module arrays: 
!   made use of the !@cuf sentinel to conditionally include code when
!   when compiled for CUDA Fortran. Arrays a_d() and aNew() are 
!   device counterparts to the host arrays a() and aNew(). The device
!   attribute is assigned to array absResidual().
!   When compiling for CUDA Fortran both host and device arrays 
!   are needed for a() and aNew(), whereas the absResidual() 
!   is needed only on the device (hence the addition of the device attribute).

! changes were made to initialize subroutine:
!   the !@cuf sentinel is used in the inti to transfer the results to the device.

! changes were made to the laplace and jacobi subroutines: 
!   the localName => moduleName option  is used for renaming.
!   Instead modifying the executable statements of the code by appending _d to the arrays, 
!   we rename the device arrays  declared in the module so they can be 
!   be locally accessed by the names without the _d suffix.
!   Note that using this renaming technique all changes made in porting this subroutine
!   occur before the implicit none statement, whereas the executable statements remain unaltered.
!   This reduces the amount of changes needed in porting and maintaining 
!   larger code bases than found in this simple example. 


! change made to the main program:
!   some conditional output indicating when the code is compiled for execution on the GPU
!   was added, which is strongly encouraged.
! Additional ported code examples from the authors can be found at
! https://github.com/PhysicsofFluids/AFiD_GPU_opensource
 

module parameters
    use, intrinsic :: iso_fortran_env 
    integer, parameter :: nx = 4096, ny =  4096 
    integer, parameter :: iterMax = 100 
    integer, parameter :: reportInterval = 10 
    integer, parameter :: fp_kind = real32 
    real(fp_kind), parameter :: tol = 1.0e-5_fp_kind 
end module parameters 



module arrays 
    use parameters 
    real(fp_kind) :: a(nx,ny), aNew(nx,ny), absResidual(2:nx-1,2:ny-1)
! changes made to module arrays for CUDA Fortran version

    !@cuf real(fp_kind), device :: a_d(nx,ny), aNew_d(nx,ny)
    !@cuf attributes(device)    :: absResidual
end module arrays 

module laplaceRoutines 
contains 
    subroutine initialize()
        use parameters 
        use arrays 
        implicit none 
        real(fp_kind), parameter :: & 
            pi = 2.0_fp_kind*asin(1.0_fp_kind)
        real(fp_kind)   :: y0(nx)
        integer :: i 

        do i = 1, nx 
            y0(i) = sin(pi*(i-1)/(nx-1))
        enddo
        a = 0.0_fp_kind 
        a(:,1) = y0
        a(:,ny) = y0*exp(-pi)
        aNew = a 
    ! changes made for CUDA Fortran version
        !@cuf aNew_d = aNew
        !@cuf a_d = a
    end subroutine initialize 


    subroutine laplaceSolution() 
        use parameters
    ! changes for CUDA Fortran
#ifdef _CUDA
        use arrays, only: a => a_d, aNew => aNew_d, absResidual
#else
        use arrays
#endif
        !@cuf use cudafor
        implicit none 
        real(fp_kind) :: maxResidual = 2*tol 
        integer :: iter 

        iter = 0 
        do while ( maxResidual > tol .and. iter <= iterMax )
            iter = iter + 1 
            call jacobiIteration() 
            maxResidual = maxval(absResidual)
            if(mod(iter, reportInterval) == 0) & 
                print '(i8,3x,f10.6)', iter, maxResidual 
            a = aNew 
        end do 
    end subroutine laplaceSolution 


subroutine jacobiIteration() 
    use parameters 
#ifdef _CUDA 
    use arrays, only: a => a_d, aNew => aNew_d, absResidual
#else    
    use arrays
#endif 
    implicit none 
    integer :: i, j 

    !$cuf kernel do(2) <<<*,*>>>
    do j=2,ny-1 
            do i=2, nx-1        
                aNew(i,j) = 0.2_fp_kind * & 
                    (a(i,j-1)+a(i-1,j)+a(i+1,j)+a(i,j+1)) + &
                    0.05_fp_kind * &
                    (a(i-1,j-1)+a(i+1,j-1)+a(i-1,j+1)+a(i+1,j+1))
                absResidual(i,j) = abs(aNew(i,j)-a(i,j))
            end do 
        end do    
    end subroutine jacobiIteration 

end module laplaceRoutines


program main 
    use parameters 
    use arrays 
    use laplaceRoutines 
    implicit none 
    real :: startTime, stopTime

    !@cuf print '(/,a,/)', 'GPU version'
    print '(/,a,i0,a,i0,a)', & 
        'Relaxation calculation on ', nx, ' x ', ny, ' mesh ' 
    print * , 'Iteration  Max Residual' 

    call initialize() 

    call cpu_time(startTime)
    call laplaceSolution() 

    call cpu_time(stopTime)
    print '(a,f10.3,a)',  ' Completed in ' , & 
        stopTime-startTime, ' seconds'
end program main 


