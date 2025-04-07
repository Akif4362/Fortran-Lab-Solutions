program mersenne
implicit none
integer:: i, j, tot=69, n
integer(kind=16):: m
logical:: prime

write(*, "(T3, A, T30, A, T45, A)") "n", "Mn", "prime?"
write(*, "(T3, A, T30, A, T45, A)") "-", "--", "------"

do n = 0, 69
  m = 2.0**n - 1.0
  write(*, "(i3, i28, T47, l)") n, m , prime(m)
end do

end program mersenne

logical function prime(x)
implicit none
integer:: i
integer(kind=16):: x
prime = .True.

do i = 2, x-1
  if ( mod(x,i)==0 ) then
    prime = .False.
    exit
  end if 
end do

end function