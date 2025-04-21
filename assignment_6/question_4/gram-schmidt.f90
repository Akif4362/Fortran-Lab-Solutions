program gram_schmidt
implicit none
integer :: i, j
real, dimension(3,3) :: matrix
real, dimension(3) :: u1, u2, u3
real, dimension(3) :: v1, v2, v3
real, dimension(3) :: e1, e2, e3
real :: proj_scaler
open(unit=42, file="input.txt")
open(unit=43, file="output.txt")

! take input data
do i = 1, 3
read(42, *) (matrix(i, j), j = 1, 3)
end do

! divide data into u1, u2, u3
do i = 1, 3
u1(i) = matrix(1, i)
u2(i) = matrix(2, i)
u3(i) = matrix(3, i)
end do

! step 1: v1 = u1
v1 = u1

! step 2: v2 = u2 - proj_v1(u2)
v2 = u2 - proj_scaler(u2, v1) * v1

! step 3: v3 = u3 - proj_v1(u3) - proj_v2(u3)
v3 = u3 - proj_scaler(u3, v1) * v1 - proj_scaler(u3, v2) * v2

! normalize the vectors to get the orthonormal basis
! norm2 is built-in fortran function
e1 = v1 / norm2(v1)
e2 = v2 / norm2(v2)
e3 = v3 / norm2(v3)

! output
write(43, *) "Orthonormal basis:"
write(43, *) "e1 = ", e1
write(43, *) "e2 = ", e2
write(43, *) "e3 = ", e3

end program gram_schmidt

! this function only calculates the scaler part. you have to multiply v again for projection
real function proj_scaler(u, v) 
implicit none
real, dimension(3):: u, v
real :: scalar
proj_scaler = dot_product(u, v) / dot_product(v, v) ! dot_product is built-in fortran function
end function proj_scaler



