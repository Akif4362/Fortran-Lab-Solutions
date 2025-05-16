program qc
implicit none
real:: q, q_prime, three_point_endpoint, three_point_midpoint, five_point_endpoint, five_point_midpoint
open(42, file="output.txt")

write(42, "(a12, a18, a12)") "method", "Approx.", "Abs. Error"
write(42, "(a12, a18, a12)") "======", "=======", "=========="
write(42, 150) "three point midpoint", three_point_midpoint(5.0, -0.25), abs(q_prime(5.0) - three_point_midpoint(5.0, -0.25))
write(42, 150) "three point endpoint", three_point_endpoint(5.0, 0.25), abs(q_prime(5.0) - three_point_endpoint(5.0, 0.25))
write(42, 150) "five point midpoint", five_point_midpoint(5.0, 0.5), abs(q_prime(5.0) - five_point_midpoint(5.0, 0.5))
write(42, 150) "five point endpoint", five_point_endpoint(5.0, 0.5), abs(q_prime(5.0) - five_point_endpoint(5.0, 0.5))

150 format(a, 2f10.5)

end program

real function q(t)
implicit none
real:: t
q = (60.0 - 60.0 * exp(-t/5.0))
end function

real function q_prime(t)
implicit none
real:: t
q_prime = (12.0 * exp(-t/5.0))
end function

real function three_point_endpoint(x, h)
implicit none
real:: x, h, q
three_point_endpoint = ((1.0 / (2.0 * h)) * (-3.0 * q(x) + 4 * q(x + h) - q(x + 2*h)))
end function

real function three_point_midpoint(x, h)
implicit none
real:: x, h, q
three_point_midpoint = ((1.0 / (2.0 * h)) * ( q(x + h) - q(x - h)))
end function

real function five_point_endpoint(x, h)
implicit none
real:: x, h, q
five_point_endpoint = ((1.0 / (12.0 * h)) * (-25.0 * q(x) + 48.0 * q(x + h) - 36.0 * q(x + 2*h) + 16.0 * q(x + 3*h) - 3.0* q(x + 4*h)))
end function

real function five_point_midpoint(x, h)
implicit none
real:: x, h, q
five_point_midpoint = ((1.0 / (12.0 * h)) * ( q(x - 2*h) - 8.0 * q(x - h) + 8.0 * q(x + h) - q(x + 2*h)))
end function