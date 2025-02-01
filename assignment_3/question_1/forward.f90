program forward
implicit none
integer:: nval= 0, status, i, j, k
real :: temp, temp2, term, result = 0, input, p, h
real, allocatable, dimension (:) :: x, y
real, allocatable, dimension (:, :) :: dd

open(unit=42, file="data.txt", action="read")
open(unit=43, file="input.txt", action="read")
open(unit=44, file="output.txt", action="write")

do
  read(42, *, iostat=status) temp
  if (status /= 0) exit
  nval = nval + 1  
end do

allocate(x(nval))
allocate(y(nval))
allocate(dd(nval, nval))

rewind(42)

do i = 1, nval
  read(42, *) temp, temp2
  x(i) = temp
  y(i) = temp2  
end do

dd = 0 !set all entries to zero

do i = 1, nval
  dd(i, 1) = y(i) !set first column of table as value of function
end do

do j = 2, nval
  do i = 1, nval - j + 1
    dd(i, j) = dd(i+1, j-1) - dd(i, j-1)
  end do
end do


write(44, '(T9, A, T21, A, T38, A, T55, A, T72, A, T89, A)') "y", "first dd", "second dd", "third dd", "fourth dd", "fifth dd"
write(44, '(T5, A, T21, A, T38, A, T55, A, T72, A, T89, A)') "---------", "---------", "---------", "---------", "---------", "---------"

do i = 1, nval
  write(44, *) (dd(i, j), j = 1, nval - i + 1)
end do

do
  read(43, *, iostat=status) input
  if (status /= 0) exit
  h = x(2) - x(1)
  p = (input - x(1)) / h
  result = 0

  do j = 1, nval
    term = dd(1, j)
    do i = 1, j-1
      term = term * ((p - i + 1)/i)
    end do
    result = result + term
  end do

  write(44, *)
  write(44, *) "For x = ", input, " f(x) = ", result
end do

end program forward