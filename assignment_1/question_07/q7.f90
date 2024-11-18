program q7
implicit none
integer :: i, a, b, count
integer :: divsum
count = 0
i = 1

do
    a = divsum(i)
    if (a > i) then
        b = divsum(a)
        if (i == b) then
            count = count + 1
            write(*, *) i, a
        end if
    end if
    i = i + 1
    if (count == 10) exit
end do

end program q7


integer function divsum(n)
implicit none
integer :: n, i, s
s = 0
do i = 1, n-1
    if ( mod(n, i) == 0 ) then
        s = s + i
    end if     
end do
divsum = s
end function
