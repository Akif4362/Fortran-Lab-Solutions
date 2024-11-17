program q2

    implicit none
    integer :: n, i, j, temp, iptr
    integer, allocatable :: arr(:)
    open(unit= 42, file="output.txt", action="write")

    write(*, *) "input number n :"
    read(*, *) n
    allocate(arr(n))
    do i = 1, n
        write(*, *) "input ", i, " number :"
        read(*, *) temp
        arr(i) = temp        
    end do

    ! ascending order
    do i = 1, n-1
        iptr = i
        do j = i+1 , n
            if(arr(j) < arr(iptr)) then
                iptr = j
            end if
        end do
        if ( arr(i) /= arr(iptr)) then
            temp = arr(i)
            arr(i) = arr(iptr)
            arr(iptr) = temp
        end if
    end do

    write(42, *) "ascending order :"
    write(42, *) (arr(i), i=1, n)

    ! descending order
     do i = 1, n-1
        iptr = i
        do j = i+1 , n
            if(arr(j) > arr(iptr)) then
                iptr = j
            end if
        end do
        if ( arr(i) /= arr(iptr)) then
            temp = arr(i)
            arr(i) = arr(iptr)
            arr(iptr) = temp
        end if
    end do

    write(42, *) "descending order :"
    write(42, *) (arr(i), i=1, n)
end program q2
