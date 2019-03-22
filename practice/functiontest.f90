function powwer(i,j)result(res)
	integer::i,j
	integer::res
	res=1
	do while(j/=0)
		if(mod(j,2)==1)then
			res=res*i
		end if 
		i=i*i
		j=j/2
	end do 
	return 
end function powwer
program main
	integer::x,y,powwer
	read *,x,y	
	x=powwer(x,y)
end program main
