module solve	
	implicit none
	character(len=105),dimension(105)::str
	integer,private::stat,i,j
	integer,dimension(210),private::arr,temp
contains
	subroutine cnt()	
		write(11,FMT="(A)",advance="no")"line"
		do i=97,97+5
			write(11,FMT="(A3)",advance="no")char(i)
		end do
		write(11,*)
		arr(1:210)=0
		do i=1,105
			temp(1:210)=0
			read(10,FMT="(A)",iostat=stat)str(i)
			write(11,FMT="(I4)",advance="no")i
			do j=1,len(str(i))
				temp(ichar(str(i)(j:j)))=temp(ichar(str(i)(j:j)))+1
			end do
			do j=1,210
				arr(j)=arr(j)+temp(j)
			end do
			do j=97,97+5
				write(11,FMT="(I3)",advance="no")temp(j)
			end do
			write(11,*)""
			if(stat/=0)then
				exit
			end if
		end do
		write(11,FMT="(A4)",advance="no")"sum"
		do j=97,97+5
			write(11,FMT="(I3)",advance="no")arr(j)
		end do
		write(11,*)
	end subroutine cnt
end module solve
program main
	use solve
	implicit none
	open(10,file="input.txt",status="old")
	open(11,file="output.txt",status="replace")
	call cnt()
	close(10)
	close(11)
end program main
