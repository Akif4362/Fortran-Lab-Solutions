program q3

    implicit none
    real :: p, f, g, e, p_prev
    integer :: n 

    open(unit=40, file="output.txt", action="write")

    ! P0 = -5
    e = 10.0**(-5)
    p = -5.0
    n = 1

    write(40, "(/, A, /)") "p is initialized as -5"
    write(40, "(T3, A, T11, A, T22, A, T38, A, T50, A, T60, A)") "It.", "Pn-1", "f(Pn-1)", "Pn", "f(Pn)", "Relative Error"
    write(40, *) "-------------------------------------------------------------------------"

    do 
       p_prev = p
       p = g(p_prev)
       if ( abs(p - p_prev) < e) exit
       write(40, "(i3, f13.6, f13.6, f13.6, f14.6, f13.6)") n, p_prev, f(p_prev), p, f(p), Abs(p-p_prev)/Abs(p)
       n = n + 1 
    end do

     ! P0 = -2
    e = 10.0**(-5)
    p = -2.0
    n = 1

    write(40, "(/, A, /)") "p is initialized as -2"
    write(40, "(T3, A, T11, A, T22, A, T38, A, T50, A, T60, A)") "It.", "Pn-1", "f(Pn-1)", "Pn", "f(Pn)", "Relative Error"
    write(40, *) "-------------------------------------------------------------------------"

    do 
       p_prev = p
       p = g(p_prev)
       if ( abs(p - p_prev) < e) exit
       write(40, "(i3, f13.6, f13.6, f13.6, f14.6, f13.6)") n, p_prev, f(p_prev), p, f(p), Abs(p-p_prev)/Abs(p)
       n = n + 1 
    end do


     ! P0 = 1
    e = 10.0**(-5)
    p = 1.0
    n = 1

    write(40, "(/, A, /)") "p is initialized as 1"
    write(40, "(T3, A, T11, A, T22, A, T38, A, T50, A, T60, A)") "It.", "Pn-1", "f(Pn-1)", "Pn", "f(Pn)", "Relative Error"
    write(40, *) "-------------------------------------------------------------------------"

    do 
       p_prev = p
       p = g(p_prev)
       if ( abs(p - p_prev) < e) exit
       write(40, "(i3, f13.6, f13.6, f13.6, f14.6, f13.6)") n, p_prev, f(p_prev), p, f(p), Abs(p-p_prev)/Abs(p)
       n = n + 1 
    end do


     ! P0 = 4
    e = 10.0**(-5)
    p = 4.0
    n = 1

    write(40, "(/, A, /)") "p is initialized as 4"
    write(40, "(T3, A, T11, A, T22, A, T38, A, T50, A, T60, A)") "It.", "Pn-1", "f(Pn-1)", "Pn", "f(Pn)", "Relative Error"
    write(40, *) "-------------------------------------------------------------------------"

    do 
       p_prev = p
       p = g(p_prev)
       if ( abs(p - p_prev) < e) exit
       write(40, "(i3, f13.6, f13.6, f13.6, f14.6, f13.6)") n, p_prev, f(p_prev), p, f(p), Abs(p-p_prev)/Abs(p)
       n = n + 1 
    end do
end program q3

real function f(x)
implicit none
real , intent(in) :: x
f = 5*x**2 + cos(3*x) - 2*exp(x) - exp(-x)
end function 

real function g(x)
implicit none
real , intent(in) :: x 
g = x - ((5*x**2 + cos(3*x) - 2*exp(x) - exp(-x))/(10*x - 3*sin(3*x) - 2*exp(x) + exp(-x)))
end function 