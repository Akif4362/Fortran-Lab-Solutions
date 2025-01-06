program lagrange
implicit none
integer:: nval= 0, status, i, j
real :: temp, temp2, lagrangeval, xinput
real, allocatable, dimension (:) :: x, y

open(unit=42, file="data.txt", action="read")
open(unit=43, file="input.txt", action="read")
open(unit=44, file="output_lag.txt", action="write")

do
  read(42, *, iostat=status) temp
  if (status /= 0) exit
  nval = nval + 1  
end do

allocate(x(nval))
allocate(y(nval))

rewind(42)

do i = 1, nval
  read(42, *) temp, temp2
  x(i) = temp
  y(i) = temp2  
end do

read(43, *) xinput
write(44, *) "For x = ", xinput, " f(x) = ", lagrangeval(x, y, nval, xinput)

end program lagrange

real function lagrangeval(arr_x, arr_y, n, input)
integer :: n, i, j
real :: lagterm, result = 0, input
real, dimension(n):: arr_x, arr_y

do i = 1, n
  lagterm = arr_y(i)
  do j = 1, n
      if (i/=j) then
        lagterm = lagterm * (input - arr_x(j))/(arr_x(i) - arr_x(j)) 
      end if
  end do
  result = result + lagterm
end do
lagrangeval = result
end function
