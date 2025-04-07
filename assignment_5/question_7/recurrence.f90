program recurr
implicit none
integer:: i, recr

write(*, "(T10, A, T15, A, T28, A)") "it.", "recursive", "iterative"
write(*, "(T10, A, T15, A, T28, A)") "---", "---------", "---------"

do i = 1, 50
  write(*, *) i, recr(i), (3.0/2) * i**2 + (5.0/2) * i + 1
end do

end program

recursive function recr(n) result(an)
implicit none
integer:: n, an

if (n==0) then
  an = 1
else if (n==1) then
  an = 5
else
  an = 2 * recr(n-1) - recr(n-2) + 3
end if
end function