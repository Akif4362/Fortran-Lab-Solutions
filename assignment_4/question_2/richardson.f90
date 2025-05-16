program richardson
implicit none
real:: f, ric(5, 5) = 0, h, sum 
real:: a = 0.0, b = 1.0
integer:: i, j
open(unit=42, file='output.txt')

h = 0.01

do i = 1, 5
  ric(i, 1) = ((1.0 / (2.0 * h)) * (f(2.0 + h) - f(2.0 - h)))
  h = h/2.0
end do

do i = 2, 5
  do j = 2, i
    ric(i, j) = (4**(j-1) * ric(i, j-1) - ric(i-1, j-1)) / (4**(j-1) - 1)
  end do
end do

do i = 1, 5
write(42, *) (ric(i, j), j = 1, 5)
end do

end program richardson

real function f(x)
implicit none
real:: x
f = (x * exp(x))
end function