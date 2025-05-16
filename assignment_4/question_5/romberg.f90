program romberg
implicit none
real:: f, rom(5, 5) = 0, h, sum 
real:: a = 0.0, b = 1.0
integer:: i, j
open(unit=42, file='output.txt')

h = b - a

do i = 1, 5
  sum = 0.0
  do j = 1, int((b-a)/h) - 1
    sum = sum + f(a + j*h)
  end do

  rom(i, 1) = (h / 2.0) * (f(a) + f(b) + 2 * sum)
  h = h/2.0
end do

do i = 2, 5
  do j = 2, i
    rom(i, j) = (4**(j-1) * rom(i, j-1) - rom(i-1, j-1)) / (4**(j-1) - 1)
  end do
end do

do i = 1, 5
write(42, *) (rom(i, j), j = 1, 5)
end do

write(42, *)
write(42, *) "actual answer is ", atan(1.0) - atan(0.0)

end program

real function f(x)
implicit none
real:: x
f = (1 / (1 + x**2))
end function