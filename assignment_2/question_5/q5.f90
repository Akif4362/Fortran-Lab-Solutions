program q5

    implicit none
    real :: a, b, p, e, f, p_prev, rerr
    integer :: i

    open(unit=42, file="output.dat", action="write")
    open(unit=40, file='input.dat', action='read')

    read(40, *) a, b

    p_prev = a
    p = (a*f(b) - b*f(a))/(f(b) - f(a))
    i = 1
    e = 10.0**(-5)

    if (f(a)* f(b) <= 0.0) then

    write(42, "(T5, A3, T15, A3, T30, A3, T45, A3, T63, A4, T78, A)") "It.", "a", "b", "x", "f(x)", "Relative Error"
    write(42, *) "---------------------------------------------------------------------------------------------"

    do
        rerr = abs(p_prev-p)/abs(p)
        write(42, "(A3, i3, A3, f12.6, A3, f12.6, A3, f12.6, A3,es17.8,A3,es17.8,A3)") "|",i,"|",a,"|",b,"|",p,"|",f(p),"|",rerr,"|"
        write(42, *) "---------------------------------------------------------------------------------------------"

        if (abs(f(p)) < e) exit

        if (f(p)*f(a) > 0.0) then
            a = p
        else
            b = p
        end if

        i = i + 1

        p_prev = p
        p = (a*f(b) - b*f(a))/(f(b) - f(a))
    end do

    else
        write(42, *) "There are no roots in this interval"

    end if

end program q5

real function f(x)
implicit none
real, intent(in) :: x
f = exp(x) + 2**(-x) + 2*cos(x) - 6
end function
