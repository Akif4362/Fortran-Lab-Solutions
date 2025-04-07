program question3
implicit none
integer:: i, fibrecur, fibfunc, temp
real:: t1, t2

write(*, "(T11, A, T20, A, T32, A)") "it.", "function", "recursive"
write(*, "(T11, A, T20, A, T32, A)") "---", "--------", "---------"

do i = 1, 20
write(*, *) i, fibfunc(i), fibrecur(i)
end do 

write(*, *)

call cpu_time(t1)
do i = 1, 20
  temp = fibfunc(i)
end do
call cpu_time(t2)

write(*, "(A, F8.6, A)") "Time for function: ", t2-t1, " seconds"

write(*, *)

call cpu_time(t1)
do i = 1, 20
  temp = fibrecur(i)
end do
call cpu_time(t2)

write(*, "(A, F8.6, A)") "Time for recursive function: ", t2-t1, " seconds"

end program

integer function fibfunc(n)
implicit none
integer:: n, n0 = 0, n1 = 1, nth, it=2
if (n==1) then 
  fibfunc = 0
else if (n==2) then 
  fibfunc = 1
else
  do
    if (it == n) exit
    nth = n0 + n1
    n0 = n1
    n1 = nth
    it = it + 1
  end do
  fibfunc = nth
end if
end function



recursive function fibrecur(n) result(f)
integer:: n, f
if (n==1) then 
  f = 0
else if (n==2) then 
  f = 1
else
  f = fibrecur(n-1) + fibrecur(n-2)
end if
end function