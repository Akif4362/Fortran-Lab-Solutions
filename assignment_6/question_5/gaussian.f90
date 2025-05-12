program gaussian
implicit none
integer, parameter:: n = 3
real:: scaling_factor
real:: matrix(n, n + 1), a(n, n), b(n), x(n)
integer:: i, j
open(unit=42, file="input.txt")
open(unit=43, file="output.txt")

do i = 1, n
  read(42, *) (matrix(i, j), j = 1, n + 1)
end do

do i = 1, n
  do j = 1, n
    a(i, j) = matrix(i, j)
  end do
end do

do i = 1, n
  b(i) = matrix(i, n + 1)
end do

write(43, *) "augmented Matrix="
do i = 1, n
  write(43, *) (matrix(i, j), j = 1, n + 1)
end do

do i = 1, n
  if (matrix(i,i) == 0.0) then
    write(43, *) "cant divide by zero"
  else
    matrix(i, :) = matrix(i, :) / matrix(i, i)
  end if

  do j = i+1, n
     scaling_factor = matrix(j, i)
     matrix(j, :) = matrix(j, :) - (scaling_factor * matrix(i, :))
  end do
end do

write(43, *)
write(43, *) "upper triangular matrix="
do i = 1, n
  write(43, *) (matrix(i, j), j = 1, n + 1)
end do

do i = n, 1, -1
  x(i) = matrix(i, n+1)

  do j = i+1, n
    x(i) = x(i) - matrix(i, j) * x(j)
  end do
end do

write(43, *)
write(43, *) "solution is "
write(43, *) x
end program 