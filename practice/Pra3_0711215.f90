function g(x)result(res)
	real::res
	res=x**3/3+x**2
	return
end function g
function f(x)result(res)
	real::res
	res=x*x+2*x
	return
end function f
function trap(a,b,n)result(res)
	real::h,res
	res=0
	h=abs((b-a)/n)
	do i=1,n-1
		res=res+f(a+i*h)*2
	end do
	res=res+f(a)+f(b)
	res=res/2*h
	return
end function trap
function simpson(a,b,n)result(res)
	real::h,res
	res=0
	h=abs((b-a)/n)
	do i=1,n-1
		if(mod(i,2)==1)then
			res=res+f(a+i*h)*4
		else
			res=res+f(a+i*h)*2
		end if
	end do
	res=res+f(a)+f(b)
	res=res/3*h
	return
end function simpson
program main
	real::a,b,h
	read * ,a,b	
	write(*,FMT="(A)",advance="no")"Intergration range=["
	write(*,FMT="(F8.6)",advance="no")a
	write(*,FMT="(A)",advance="no")","
	write(*,FMT="(F8.6)",advance="no")b
	write(*,FMT="(A)",advance="no")"]"
	write(*,FMT="(A)",advance="no")"Exact value of intergration = "
	write(*,FMT="(F8.5)")g(b)-g(a)
	print *,"Trapezodial Rule"
	write(*,FMT="(A)",advance="no")" The approximate value of 4 division is "
	write(*,FMT="(F8.5)")trap(a,b,4)
	write(*,FMT="(A)",advance="no")" The approximate value of 6 division is "
	write(*,FMT="(F8.5)")trap(a,b,6)
	print * ,"Simpson's Rule"
	write(*,FMT="(A)",advance="no")" The approximate value of 4 division is "
	write(*,FMT="(F8.5)")simpson(a,b,4)
	write(*,FMT="(A)",advance="no")" The approximate value of 6 division is "
	write(*,FMT="(F8.5)")simpson(a,b,6)
end program main
