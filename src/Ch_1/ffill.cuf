module ffill
  contains
    attributes(device) subroutine fill(a)
    integer, device :: a(*)
    i = (blockidx%x-1)*blockdim%x + threadidx%x
    a(i) = i
    end subroutine
end module
