program velacc
implicit none
real:: x(7), y(7), forward, backward
integer:: i, j
open(42, file="input.txt")

do i = 1, 7
  read(42, *) x(i), y(i)
end do
  
! velocity
forward = (y(2) - y(1)) / (x(2) - x(1))
backward = (y(7) - y(6)) / (x(7) - x(6))

write(*, *) "velocity at x=0.0 is ", forward, " cm/s"
write(*, *) "velocity at x=0.6 is ", backward, " cm/s"
write(*, *) "can't find acceleration at end points"

end program velacc


