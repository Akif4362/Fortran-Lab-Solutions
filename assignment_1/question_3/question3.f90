program q3

    implicit none
    real, dimension(36) :: list
    real, dimension(6, 6) :: sqmat
    real :: u, num, a = 10.0, b = 20.0
    integer :: i, j

    do i = 1, 36
        call random_number(u)
        num = a + (b - a)*u
        list(i) = num
    end do

    sqmat = reshape(list, [6, 6])

    open(unit=40, file='alq3_F.txt', action='write')
    open(unit=41, file='alq3_E.txt', action='write')
    open(unit=42, file='alq3_ES.txt', action='write')

    do i= 1, 6
        write(40, '(6F15.8)') (sqmat(i,j), j=1, 6)
    end do

    do i= 1, 6
        write(41, '(6E15.7)') (sqmat(i,j), j=1, 6)
    end do

    do i= 1, 6
        write(42, '(6ES15.7)') (sqmat(i,j), j=1, 6)
    end do
    
end program q3
