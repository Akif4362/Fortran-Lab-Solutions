program factorial
implicit none
integer:: fact, comb, perm

write(*, *) "10P6 = ", perm(10, 6)
write(*, *) "10C4 = ", comb(10, 4)
write(*, *) "10C6 = ", comb(10, 6)

end program factorial

recursive function fact(n) result(f)
implicit none
integer:: n, f
if (n==0) then
  f = 1
else if (n==1) then
  f = 1
else 
  f = n * fact(n-1)
end if
end function

integer function comb(x, y)
implicit none
integer:: x, y, fact
comb = fact(x)/ (fact(y) * fact(x - y))
end function

integer function perm(x, y)
implicit none
integer:: x, y, fact
perm = fact(x)/ fact(x - y)
end function