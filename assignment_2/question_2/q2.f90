program q2

    implicit none
    real :: p, f, g1, g2, g3, e, p_prev
    integer :: n 

    open(unit=40, file="g1_output.txt", action="write")
    open(unit=41, file="g2_output.txt", action="write")
    open(unit=42, file="g3_output.txt", action="write")

    ! for g1
    ! g1, P0 = 0.5
    e = 10.0**(-6)
    p = 0.5
    n = 1

    write(40, "(/, A, /)") "p is initialized as 0.5"
    write(40, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(40, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g1(p_prev)
       if ( abs(p - p_prev) < e .or. n == 200) exit
       write(40, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

    ! g1, P0 = 1.0
    e = 10.0**(-6)
    p = 1.0
    n = 1

    write(40, "(/, A, /)") "p is initialized as 1.0"
    write(40, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(40, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g1(p_prev)
       if ( abs(p - p_prev) < e .or. n == 200) exit
       write(40, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

    ! g1, P0 = 1.5
    e = 10.0**(-6)
    p = 1.5
    n = 1

    write(40, "(/, A, /)") "p is initialized as 1.5"
    write(40, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(40, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g1(p_prev)
       if ( abs(p - p_prev) < e .or. n == 200) exit
       write(40, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

    ! for g2
    ! g2, P0 = 0.5
    e = 10.0**(-6)
    p = 0.5
    n = 1

    write(41, "(/, A, /)") "p is initialized as 0.5"
    write(41, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(41, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g2(p_prev)
       if (abs(p - p_prev) < e .or. n == 200) exit
       write(41, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

    ! g2, P0 = 1.0
    e = 10.0**(-6)
    p = 1.0
    n = 1

    write(41, "(/, A, /)") "p is initialized as 1.0"
    write(41, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(41, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g2(p_prev)
       if ( abs(p - p_prev) < e .or. n==200) exit
       write(41, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

    ! g2, P0 = 1.5
    e = 10.0**(-6)
    p = 1.5
    n = 1

    write(41, "(/, A, /)") "p is initialized as 1.5"
    write(41, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(41, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g2(p_prev)
       if ( abs(p - p_prev) < e .or. n==200) exit
       write(41, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do


    ! for g3
    ! g3, P0 = 0.5
    e = 10.0**(-6)
    p = 0.5
    n = 1

    write(42, "(/, A, /)") "p is initialized as 0.5"
    write(42, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(42, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g3(p_prev)
       if ( abs(p - p_prev) < e .or. n==200) exit
       write(42, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

    ! g3, P0 = 1.0
    e = 10.0**(-6)
    p = 1.0
    n = 1

    write(42, "(/, A, /)") "p is initialized as 1.0"
    write(42, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(42, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g3(p_prev)
       if (abs(p - p_prev) < e .or. n == 200) exit
       write(42, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

    ! g3, P0 = 1.5
    e = 10.0**(-6)
    p = 1.5
    n = 1

    write(42, "(/, A, /)") "p is initialized as 1.5"
    write(42, "(A, T10, A, T25, A, T35, A, T45, A)") "It.", "Pn-1", "Pn", "f(Pn)", "Absolute Error"
    write(42, *) "-----------------------------------------------------------"

    do 
       p_prev = p
       p = g3(p_prev)
       if ( abs(p - p_prev) < e .or. n == 200 ) exit
       write(42, "(i3, f13.8, f13.8, f14.8, f13.8)") n, p_prev, p, f(p), Abs(p-p_prev)
       n = n + 1 
    end do

end program q2

real function f(x)
implicit none
real , intent(in) :: x
f = X**4 - x - 10
end function 

real function g1(x)
implicit none
real , intent(in) :: x
g1 = sqrt(x + 10.0) / x
end function 

real function g2(x)
implicit none
real , intent(in) :: x
g2 = (x + 10.0) ** (1.0/4.0)
end function 

real function g3(x)
implicit none
real , intent(in) :: x
g3 = 10.0 / ((x**3.0) - 1.0)
end function 
