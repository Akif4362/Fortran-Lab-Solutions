program question2
implicit none
integer:: n, i
integer(kind=16) :: hanoi, mersenne
integer(kind=16) :: days

write(*, "(A, T25, A, T50, A, T76, A)") "it.", "hanoi", "mersenne", "days"
write(*, "(A, T25, A, T50, A, T76, A)") "---", "-----", "--------", "----"
do i = 1, 64
  mersenne = 2.0**i - 1.0
  days = ((2.0**i - 1.0) * 5.0 / 60.0) + 1.0
  write(*, "(i3, i25, i25, i25)") i, hanoi(i), mersenne, days
end do

end program question2

recursive function hanoi(n) result(hn)
implicit none
integer:: n
integer(kind=16):: hn
if (n==1) then
  hn = 1
else
  hn = 2 * hanoi(n-1) + 1
end if
end function hanoi
