module share_data
    type matrix
        integer*8,dimension(2,2)::v
    end type
    integer::a,b,m
contains
	function multiple(c,d)
	    implicit none
	    type(matrix)::c,d,multiple
	    integer::i,j,k
	    do i=1,2
				do j=1,2
					multiple%v(i,j)=0
					do k=1,2
						multiple%v(i,j)=mod(mod(c%v(i,k)*d%v(k,j),m)+mod(multiple%v(i,j),m),m)
	                end do
	            end do
	    end do
	    return
	end function
end module
subroutine pow(trans,j,res)
    use share_data
    implicit none
    integer::j
    type(matrix)::trans,res
    res%v(1,1)=1;res%v(1,2)=0;res%v(2,1)=0;res%v(2,2)=1
    do while(j/=0)
        if(mod(j,2)==1)res=multiple(res,trans)
        trans=multiple(trans,trans)
        j=j/2
    end do
end subroutine
subroutine count_Fis
    use share_data
    implicit none
    integer::k,stat
    type(matrix)::trans,res
        do
			trans%v(1,1)=1;trans%v(1,2)=1;trans%v(2,1)=1;trans%v(2,2)=0
            read(10,*,iostat=stat)a,b,k,m
            if(stat/=0)exit
            if(k==1)then
                write(11,*)a
                cycle
            end if
            if(k==2)then
                write(11,*)b
                cycle
            end if
            call pow(trans,k-2,res)
			write(11,*)mod(mod(b*res%v(1,1),m)+mod(a*res%v(1,2),m),m)
        end do
end subroutine
program final_project
	use share_data
	implicit none
	open(10,file="high_input.txt",status="old")
	open(11,file="high_output.txt",status="replace")
    call count_Fis
end program
