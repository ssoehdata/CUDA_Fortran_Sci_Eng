! #### Example code that can be compiled on either host or device
!      to generate .ppm image file (portable pixmap image)
! Note that for this simple case, no performance advantage is in using the GPU.
!      rather, this is meant as an example for maintaining a single source for that can be
!      compiled for either host or device execution.  

! compile both on cpu(host) and gpu(device): 
! ##############

! on host: 
! nvfortran -o ppmExampleHost ppmExample.f90 
! run and generate .ppm image file: 
! ./ppmExampleHost > ppmExample.ppm 

!################
! on device: 
! nvfortran -o ppmExampleCUDA -cuda ppmExamplef90 
! run and generate .ppm file: 
! ./ppmExampleCUDA > ppmExample.ppm




program main
    !@cuf use cudafor 
    implicit none 
    integer, parameter :: nx = 400, ny = 200 ! pixel size of image
    integer :: i, j 
    type rgb 
        real :: v(3) 
    end type rgb 
    type(rgb) :: fb(nx, ny)
    !@cuf type(rgb), device :: fb_d(nx, ny)  ! when compiled on device, generates an RGB version of the array that resides in GPU global memory

    !@cuf associate (fb => fb_d)
    !@cuf kernel do (2) <<<*,*>>>  ! when compiled on device, a kernel is generated from the following do loop(s)
    do j = 1, ny 
        do i = 1, nx 
            fb(i,j)%v(1) = real(i)/nx
            fb(i,j)%v(2) = real(j)/ny
            fb(i,j)%v(3) = 0.2 
        end do 
    end do 
    !@cuf end associate 

    !@cuf fb = fb_d 

    ! ppm output 

    print "(a2)", 'P3'  ! indicates RGB colors in ASCII, must be flush left
    print *, nx, ny     ! width and height of image
    print *, 255        ! maximum value for each color 

    do j = ny, 1, -1 
        do i = 1, nx 
            print "(3(1x,i3))", int(255*fb(i,j)%v)
        end do 
    end do 

end program main 
