program logic
implicit none
logical:: p, q, r
integer:: i, j, k

write(*, "(A2, A2, A2, A16, A25)") "p", "q", "r", "(p and q) or r", "(p or r) and (q or r)"
write(*, "(A2, A2, A2, A16, A25)") "-", "-", "-", "--------------", "---------------------"

do i = 0, 1
  do j = 0, 1
    do k = 0, 1
      p = i; q = j; r = k
      write(*, "(l2, l2, l2, T15, l, T37, l)")p, q, r, (p .and. q) .or. r, (p .or. r) .and. (q .or. r)
    end do
  end do
end do

end program
