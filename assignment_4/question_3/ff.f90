program forward
implicit none
integer:: nval= 0, status, i, j, k
real :: temp, temp2, term, result = 0, input, p, h
real, allocatable, dimension (:) :: x, y
real, allocatable, dimension (:, :) :: dd


open(unit=42, file="input.txt", action="read")
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
    dd(i, j) = (dd(i+1, j-1) - dd(i, j-1)) / (x(i+j-1) - x(i))
  end do
end do


write(44, '(T9, A, T21, A, T38, A, T55, A, T72, A, T89, A, T107, A)') "y", "first dd", "second dd", "third dd", "fourth dd", "fifth dd", "sixth dd"
write(44, '(T5, A, T21, A, T38, A, T55, A, T72, A, T89, A, T107, A)') "---------", "---------", "---------", "---------", "---------", "---------", "---------"

do i = 1, nval
  write(44, *) (dd(i, j), j = 1, nval - i + 1)
end do

write(44, *)
write(44, *) "velocity at t = 0.0s is", dd(1, 2), "cm/s"
write(44, *) "velocity at t = 0.6s is", dd(6, 2), "cm/s"

write(44, *)
write(44, *) "acceleration at t = 0.0s is", dd(1, 3), "cm/s^2"
write(44, *) "acceleration at t = 0.6s is", dd(5, 3), "cm/s^2"

end program forward