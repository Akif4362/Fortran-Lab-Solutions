program lu
implicit none
integer, parameter:: n = 3
real:: matrix(n, n+1), matlower(n, n+1) = 0.0, a(n, n), b(n), x(n), pivot, scaling_factor, ltm(n), y(n), sum
integer:: i, j, ip
open(42, file="input.txt")
open(43, file="output_lu.txt")

do i = 1, n
 read(42, *) (matrix(i, j), j = 1, n+1)
end do

do i = 1, n
  b(i) = matrix(i, n+1)
end do

write(43, *) "Augmented matrix Ax=b :"
do i = 1, n
 write(43, *) (matrix(i, j), j = 1, n+1)
end do

! gaussian elimination
do i = 1, n
pivot = matrix(i, i)
  do ip = i+1, n
    scaling_factor = matrix(ip, i) / pivot
    matlower(ip, i) = scaling_factor ! creating lower triangular matrix
    matrix(ip, :) = matrix(ip, :) - matrix(i, :) * scaling_factor
  end do
end do

write(43, *)
write(43, *) "Reduced augmented matrix (Upper trianguler matrix) = "
do i = 1, n
 write(43, *) (matrix(i, j), j = 1, n+1)
end do


do i = 1, n
  matlower(i, i) = 1.0
  matlower(i, n+1) = b(i)
end do

write(43, *)
write(43, *) "(Lower trianguler matrix) Ly = b : "
do i = 1, n
 write(43, *) (matlower(i, j), j = 1, n+1)
end do

! forward substitution
do i = 1, n
sum = 0.0
  do j = 1, n
    if (j < i) then
      sum = sum + matlower(i, j) * y(j)
    end if
  end do
  y(i) = (1 / matlower(i, i)) * (b(i) - sum)
end do
write(43, *)
write(43, *) "y = "
write(43, *) y

do i = 1, n
  matrix(i, n+1) = y(i)
end do

write(43, *)
write(43, *) "Ux = y : "
do i = 1, n
 write(43, *) (matrix(i, j), j = 1, n+1)
end do

! backward substitution
do i = n, 1, -1
sum = 0.0
  do j = n, 1, -1
    if (j > i) then
      sum = sum + matrix(i, j) * x(j)
    end if
  end do
  x(i) = (1 / matrix(i, i)) * (y(i) - sum)
end do

write(43, *)
write(43, *) "x = "
write(43, *) x
end program