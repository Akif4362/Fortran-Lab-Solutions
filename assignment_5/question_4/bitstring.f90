program bitstring
implicit none
integer:: i, n = 0
character(len=8):: temp

write(*, *) "Ascending order"

do i = 0, 255, 1
 write(*, '(B8)') i
end do

write(*, *)
write(*, *) "Descending order:"

do i = 255, 0, -1
 write(*, '(B8)') i
end do

write(*, *)
write(*, *) "Without 101 and 100:"

do i = 0, 255, 1
 write(temp, '(B8)') i
 if (index(temp, '101')==0 .and. index(temp, '100')==0) then
  write(*,*) temp
  n = n + 1
 end if
end do

write(*, *)
write(*, *) "Total number of valid bitsrings: ", n

end program bitstring
