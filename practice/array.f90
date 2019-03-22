program main
	integer,dimension(5)::arr
	real,dimension(5,5)::matrix
	arr(1)=2
	do i=1,5
		arr(i)=i
	end do
	do i=1,5
		do j=1,5
			matrix(i,j)=i*j
		end do
	end do
end program main
