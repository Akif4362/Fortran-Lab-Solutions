program q1

    implicit none
    integer, dimension(4, 4) :: mata
    integer :: i, j, a, d3, d2
    open(unit=43, file='input.dat', action="read")
    open(unit=62, file='output.dat', action='write')
    
    ! question 1
    write(62, 150) "(i)"
    do i = 1, 4
        read(43, *) (mata(i, j) ,j=1, 4)
    end do    
    
    do i= 1, 4
        write(62, 100) (mata(i,j), j=1, 4)
    end do

    ! question 2
    write(62, 150) "(ii)"
    write(62, '(16i3)') ((mata(j,i), i=1, 4), j=1, 4)

    ! question 3
    write(62, 150) "(iii)"
    do i = 1, 4
        do j = 1, 4
            write(62, *) mata(j, i)
        end do
    end do

    ! question 4
    write(62, 150) "(iv)"
    write(62, "(4i3)") (mata(1, j), j=1,4)
    do i = 2, 4
        write(62, "(i3)") mata(i, 1)
    end do

    ! question 5
    write(62, 150) "(v)"
    d3 = 0
    do i = 1, 4
            a = mata(i, i)**3
            d3 = d3 + a
    end do
    write(62, "(A5, i4)") "d3 is ", d3

    d2 = 0
    do i = 1, 4
            a = mata(i, 5-i)**2
            d2 = d2 + a
    end do
    write(62, "(A5, i4)") "d2 is ", d2
    write(62, "(A18)")"Therefore, d3 > d2 "

    ! question 6
    write(62, 150) "(vi)"
    do i= 1, 4
        write(62, 100) (mata(j,i), j=1, 4)
    end do

    150 format(/, A5, /)
    100 format(4(4i3))
end program q1
