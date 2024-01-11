(* 
 Program accepts as parameters a list of real values a_0 through a_n, and a single
 real value x. Function returns the value of the polynomial specified by passed parameters.
 Program may be compiled and ran through command 'sml p0.sml'
*)

fun epoly([], x:real) = 0.0
|	epoly(L:real list as h::T, x:real) = h + (x * epoly(T,x));
