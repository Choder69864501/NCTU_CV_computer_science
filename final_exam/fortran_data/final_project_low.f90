module share_data
    integer::a,b,k,m,res,i,stat
end module
subroutine count_Fis
    use share_data
    implicit none
	do
		read(10,*,iostat=stat)a,b,k,m
		if(stat/=0)exit
			do i=1,k-2
				res=mod(mod(a,m)+mod(b,m),m);a=b;b=res
			end do
		write(11,*)res
	end do
end subroutine
program HW_0611326
    use share_data
    implicit none
	open(10,file="low_input.txt",status="old")
	open(11,file="low_output.txt",status="replace")
    call count_Fis
end program

