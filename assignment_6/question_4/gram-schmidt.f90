program gram_schmidt
implicit none
integer :: i, j, n, m
real, dimension(:,:), allocatable :: u, v, e
real :: norm, proj_scaler
open(unit=42, file="input.txt")
open(unit=43, file="output.txt")

! n = number of vectors, m = dimension of each vector
read(42, *) n, m

allocate(u(n,m), v(n,m), e(n,m))

do i = 1, n
  read(42, *) (u(i,j), j=1,m)
end do

! step 1: v_1 = u_1
v(1,:) = u(1,:)

! step 2 to n: v_i = u_i - sum_{j=1}^{i-1} proj_{v_j}(u_i)
do i = 2, n
  v(i,:) = u(i,:)
  do j = 1, i-1
    v(i,:) = v(i,:) - proj_scaler(u(i,:), v(j,:)) * v(j,:)
  end do
end do

! step 3: normalize
do i = 1, n
  norm = norm2(v(i,:))
  e(i,:) = v(i,:) / norm
end do

write(43, *) "Orthonormal basis:"
do i = 1, n
  write(43, '(A,I0,A,*(F10.6))') "e", i, " = ", e(i,:)
end do

end program gram_schmidt

! this function only calculates the scaler part. you have to multiply v again for projection
real function proj_scaler(u, v) 
implicit none
real, dimension(3):: u, v
real :: scalar
proj_scaler = dot_product(u, v) / dot_product(v, v) ! dot_product is built-in fortran function
end function proj_scaler



