program q5

    implicit none
    real :: p, e, f, p_prev, p_prev2, rerr
    integer ::  n

    open(unit=42, file="output.dat", action="write")
    open(unit=40, file='input.dat', action='read')
    
    read(40, *) p_prev2, p_prev
    n = 1
    e = 10.0**(-5)

   write(*, "(T3, A, T11, A, T22, A, T38, A, T50, A, T60, A)") "It.", "Xn-1", "Xn", "Xn+1", "f(Xn+1)", "Relative Error"
    write(*, *) "-------------------------------------------------------------------------"

    do 
       p = p_prev - ((f(p_prev) * (p_prev-p_prev2)) / ((f(p_prev) - f(p_prev2))))
       if ( abs(p - p_prev) < e .or. n == 10) exit
       write(*, "(i3, f13.6, f13.6, f13.6, f14.6, f13.6)") n, p_prev2, p_prev, p, f(p), Abs(p-p_prev)/Abs(p)
       p_prev2 = p_prev
       p_prev = p
       n = n + 1 
    end do
end program q5

real function f(x)
implicit none
real, intent(in) :: x
f = 4*x**3 -1 - exp((x**2)/2)
end function
