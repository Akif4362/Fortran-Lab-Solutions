program question1

    implicit none
    real :: a, b, p, e, f, p_prev, rerr
    integer :: i 

    open(unit=42, file="output.dat", action="write")

    a = 1.0
    b = 3.0
    p_prev = 1.0
    p = 0.5 * (a + b)
    i = 1 
    e = 10.0**(-8)
    
    write(42, "(T5, A3, T15, A3, T30, A3, T45, A3, T63, A4, T78, A)") "It.", "a", "b", "x", "f(x)", "Relative Error"
    write(42, *) "---------------------------------------------------------------------------------------------"

    do 
        rerr = abs(p_prev-p)/abs(p)
        write(42, "(A3, i3, A3, f12.8, A3, f12.8, A3, f12.8, A3,es17.8,A3,es17.8,A3)") "|",i,"|",a,"|",b,"|",p,"|",f(p),"|",rerr,"|"
        write(42, *) "---------------------------------------------------------------------------------------------"

        if (abs(f(p)) < e) exit

        if (f(p)*f(a) > 0.0) then
            a = p
        else
            b = p
        end if

        i = i + 1

        p_prev = p
        p = 0.5 * (a + b)
    end do

end program question1

real function f(x)
implicit none
real, intent(in) :: x
f = 3*x + sin(x) - exp(x)
end function 