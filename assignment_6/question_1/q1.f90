program question1
implicit none
integer, parameter:: n = 6
real:: matrix(n, n + 1), a(n, n), b(n), x(n), x_old(n)=0.0
real::  sum=0.0, error, tol=10**(-3.0)
integer:: i, j, it=0
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

write(43, *) "Augmented Matrix="
do i = 1, n
  write(43, *) (matrix(i, j), j = 1, n + 1)
end do

write(43, *)
write(43, "(A3, 7A10)") "it.", "x1", "x2", "x3", "x4", "x5", "x6", "error"
write(43, "(A3, 7A10)") "---", "--", "--", "--", "--", "--", "--", "-----"
do 
  do i = 1, n
    sum = 0.0
    do j = 1, n
      if (i/=j) then
        sum = sum + a(i, j)*x_old(j)
      end if
    end do
    x(i) = (1/a(i, i)) * (b(i) - sum) 
  end do

  error = maxval(abs(x - x_old)) 

  write(43, '(i3, 6f10.5, f10.5)') it, (x_old(i), i=1, n), error

  x_old = x
  it = it + 1

  if (error < tol) exit
end do

write(43, *)
write(43, '(A, 6f10.5)') "Therefore solution is x = ", x

end program