program q4

    implicit none
    integer :: gcd
    write(*, *) gcd(48, 18)

end program q4

recursive function gcd(a, b) result(g)
    integer, intent(in) :: a, b
    integer :: g

    if (b == 0) then
        g =  a
    else
        g = gcd(b, mod(a, b))
    end if
end function gcd