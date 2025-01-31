program q6
implicit none
integer:: i, j, nval = 0, status, r1, c1, r2, c2
real:: temp
logical:: matq
real, allocatable, dimension(:, :):: matA, matB, matC, matV

open(unit=40, file='matrix_A.txt', action='read')
open(unit=41, file='matrix_B.txt', action='read')

write(*, *) "input number of row of matrix 1: "
read(*, *) r1

write(*, *) "input number of col of matrix 1: "
read(*, *) c1

write(*, *) "input number of row of matrix 2: "
read(*, *) r2

write(*, *) "input number of col of matrix 2: "
read(*, *) c2



if (matq(r1, c1, r2, c2) .eqv. .TRUE.) then

  allocate(matA(r1, c1), matB(r2, c2), matC(r1, c2), matV(r1, c2))


  do i = 1, r1
    read(40, *) (matA(i, j), j = 1, c1)
  end do

  do i = 1, r2
    read(41, *) (matB(i, j), j = 1, c2)
  end do

  call matrixmultiplication(matA, matB, matC, r1, c1, r2, c2)

  write(*, *) "Matrix Multiplication by looping"
  do i = 1, r1
    write(*, *) (matC(i, j), j = 1, c2)
  end do

  matV = matmul(matA, matB)

  write(*, *)
  write(*, *) "Matrix Multiplication by matmul command"
  do i = 1, r1
    write(*, *) (matV(i, j), j = 1, c2)
  end do

else

  write(*, *) "The matrices are not compatible"
  
end if

end program q6

logical function matq(r1, c1, r2, c2)
implicit none
integer :: r1, c1, r2, c2

  if (c1 == r2) then
  matq = .TRUE.
  else
  matq = .FALSE.
  end if

end function

subroutine matrixmultiplication(A, B, C, row1, col1, row2, col2)
implicit none
integer:: i, j, k, row1, col1, row2, col2
real:: A(row1, col1), B(row2, col2), C(row1, col2)

C = 0.0

do i = 1, row1
  do j = 1, col2
    do k = 1, row2
      C(i, j) = C(i, j) + A(i, k) * B(k, j)
    end do
  end do
end do

end subroutine


