program dividedDifference
implicit none
integer:: nval= 0, status, i, j, k
real :: temp, temp2, term, result = 0, input
real, allocatable, dimension (:) :: x, y
real, allocatable, dimension (:, :) :: dd

open(unit=42, file="data.txt", action="read")
open(unit=43, file="input.txt", action="read")
open(unit=44, file="output_dd.txt", action="write")

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
    dd(i, j) = (dd(i+1, j-1) - dd(i, j-1)) / (x(i+j-1) - x(i))
  end do
end do

read(43, *) input

do j = 1, nval
  term = dd(1, j)
  do i = 1, j-1
    term = term * (input - x(i))
  end do
  result = result + term
end do

write(44, '(T9, A, T21, A, T38, A, T55, A, T72, A)') "y", "first dd", "second dd", "third dd", "fourth dd"
write(44, '(T5, A, T21, A, T38, A, T55, A, T72, A)') "---------", "---------", "---------", "---------", "---------"

do i = 1, nval
  write(44, *) (dd(i, j), j = 1, nval - i + 1)
end do

write(44, *)
write(44, *) "For x = ", input, " f(x) = ", result

end program dividedDifference