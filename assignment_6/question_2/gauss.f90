program gauss
implicit none
integer, parameter:: n = 3
real:: matrix(n, n + 1), a(n, n), b(n), x(n), x_old(n)=0.0 
real:: sum1=0.0, sum2=0.0, error, tol=10**(-5.0)
integer:: i, j, it=0
open(unit=42, file="input.txt")
open(unit=43, file="output_gauss.txt")

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
write(43, "(A3, 4A13)") "it.", "x1", "x2", "x3", "error"
write(43, "(A3, 4A13)") "---", "--", "--", "--", "-----"

do 
  do i = 1, n

    sum1 = 0.0
    do j = 1, i - 1
      sum1 = sum1 + a(i, j)*x(j)
    end do

    sum2 = 0.0
    do j = i+1, n
      sum2 = sum2 + a(i, j)*x_old(j)
    end do

    x(i) = (1/a(i, i)) * (b(i) - sum1 - sum2) 
  end do

  error = maxval(abs(x - x_old)) 

  write(43, '(i3, 3f13.5, f13.5)') it, (x_old(i), i=1, n), error

  x_old = x
  it = it + 1

  if (error < tol .or. it==50) exit
end do

write(43, *)
write(43, '(A, 3f10.5)') "Therefore solution is x = ", x

end program