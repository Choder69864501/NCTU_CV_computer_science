function xint(x,n)result(y)
	integer::n
	real::x,y
	y=int(x+0.5*0.1**(n-1))*1.0
	return
end function xint
program main
	Implicit None
	Integer :: I,J, K
	Real :: X, Y,Z
	X = 1.234567
	Print *,"X ANINT(X) = ", X,ANINT(X)
	Print *,"X ANINT(X*10.)/10. = ",X, ANINT(X*10.)/10.
	Print *,"X,ANINT(X*100.)/100. = ",X,ANINT(X*100.)/100. 
	Print *,"X ANINT(X*1000.)/1000. = ",X, ANINT(X*1000.)/1000. 
end program main
