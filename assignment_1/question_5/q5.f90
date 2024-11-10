program q5
    implicit none
    logical :: prime
    integer, dimension(0:5) :: arr
    integer :: i, f

    ! (i)
    write(*, "(/, A, /)") " (i) "
    
    do i = 0, 5
        f = (2**(2**i)) + 1
        arr(i) = f
    end do

    write(*, *) (arr(i), i=0, 5)

    write(*, *) "Fortran's 64-bit integer cannot process the value of the sixth fermat number "
    
    ! (ii)
    write(*, "(/, A, /)") " (ii) "
    
    do i = 0, 4
        if ( prime(arr(i)) .eqv. .True. ) then
        write(*, *) arr(i), " is a prime number" 
        else
        write(*, *) arr(i), " is not prime number"
        end if 
    end do

    ! (iii)
    write(*, "(/, A, /)") " (iii) "
    write(*, *) "From above we can deduce that atleast the first five fermat numbers are prime"
    
end program q5

logical function prime(a)
    implicit none
    integer, intent(in) :: a
    integer :: i
    prime = .True.
    do i = 2, a-1
        if (mod(a, i) == 0) then
            prime = .False.
            exit
        end if
    end do
end function prime