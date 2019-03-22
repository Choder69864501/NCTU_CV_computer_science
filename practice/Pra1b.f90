program main
	integer::sec,a,b,c
	read(*,*)sec
	a=sec/3600
	sec=mod(sec,3600)
	b=sec/60
	sec=mod(sec,60)
	c=sec
	write(*,*)a,b,c
end program main
