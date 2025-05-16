program integration
implicit none
real:: f, fexact, sum=0.0, trapezoidal, simpson, simpson3by8, weddle, h, a=-0.4, b=0.6, exact
integer:: n=30, i, j
open(42, file="output.txt")

exact = fexact(0.6) - fexact(-0.4)

h = (b-a) / n

! trapezoidal
do i = 1, n-1
  sum = sum + f(a + i*h)
end do
trapezoidal = (h / 2.0) * (f(a) + f(b) + 2*sum)

! simpson
sum = 0.0
do i = 1, n-1
  if (mod(i, 2) == 0.0) then
    sum = sum + 2*f(a + i*h)
  else 
    sum = sum + 4*f(a + i*h)
  end if
end do
simpson = (h / 3.0) * (f(a) + f(b) + sum)

! simpson3by8
sum = 0.0
do i = 1, n-1
  if (mod(i, 3) == 0.0) then
    sum = sum + 2*f(a + i*h)
  else 
    sum = sum + 3*f(a + i*h)
  end if
end do
simpson3by8 = ((3.0 * h) / 8.0) * (f(a) + f(b) + sum)

! weddle
sum = 0.0
do i = 1, n-1
  if (mod(i, 6) == 0.0) then
    sum = sum + 2*f(a + i*h)
  else if (mod(i, 6) == 1.0) then 
    sum = sum + 5*f(a + i*h)
  else if (mod(i, 6) == 2.0) then 
    sum = sum + 1*f(a + i*h)
  else if (mod(i, 6) == 3.0) then 
    sum = sum + 6*f(a + i*h)
  else if (mod(i, 6) == 4.0) then 
    sum = sum + 1*f(a + i*h)
  else if (mod(i, 6) == 5.0) then 
    sum = sum + 5*f(a + i*h)
  end if
end do
weddle = ((3.0 * h) / 10.0) * (f(a) + f(b) + sum)

write(42, "(t6, a, t21, a, t35, a, t48, a, t64, a)") "method", "exact", "approx", "abs error", "rel error"
write(42, "(t6, a, t21, a, t35, a, t48, a, t64, a)") "------", "-----", "------", "---------", "---------"
write(42, 150) "trapezoidal", exact, trapezoidal, abs(exact-trapezoidal), abs(exact-trapezoidal) / abs(exact)
write(42, 150) "simpson", exact, simpson, abs(exact-simpson), abs(exact-simpson) / abs(exact)
write(42, 150) "simpson3by8", exact, simpson3by8, abs(exact-simpson3by8), abs(exact-simpson3by8) / abs(exact)
write(42, 150) "weddle", exact, weddle, abs(exact-weddle), abs(exact-weddle) / abs(exact)

150 format(a13, 4f15.10)
end program

real function f(x)
implicit none
real:: x
f = (-1.0 / ((x - 1.0) * (2.0 + sqrt(2.0) - 2.0*x)))
end function

real function fexact(x)
implicit none
real:: x
fexact = ((-1.0 / sqrt(2.0)) * log(abs((x - 1.0) / (sqrt(2.0) - 2*x + 2))))
end function