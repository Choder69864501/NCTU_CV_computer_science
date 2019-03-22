program main
	real::hh,mm,ss,ans
	read(*,*)hh,mm,ss
	ans=hh*3600+mm*60+ss
	print *,ans,ans/86400
end program main
