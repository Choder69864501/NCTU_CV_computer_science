module rb
	implicit none
	type student  !此為學生的type內容物分別為，學號、出席次數、出席率
		character(len=6)::st_num
		real::att_rate,att_cnt
	end type student
	type week !此為周次的type內容物分別為，周次、出席人數、出席率
		integer::num
		real::att_rate,att_cnt
	end type week
	save
	integer,dimension(:,:),allocatable::atten   !將一整張從一和零組成的出席表格，用二維陣列讀取進來
	type(student),dimension(:),allocatable::st !學生的陣列
	type(week),dimension(18)::wk !周次的陣列
	integer::kase
end module rb
subroutine count_atten  !將那張一和零組成的二維陣列分別以列跟行為主整理給學生的陣列和周次的陣列
	use rb
	implicit none
	integer::i,j
	do i=1,kase
		do j=1,18
			if(atten(i,j)==1)st(i)%att_cnt=st(i)%att_cnt+1
		end do
		st(i)%att_rate=st(i)%att_cnt/18*100
	end do
	do j=1,18
		wk(j)%num=j
		do i=1,kase
			if(atten(i,j)==1)wk(j)%att_cnt=wk(j)%att_cnt+1
		end do
		wk(j)%att_rate=wk(j)%att_cnt/kase*100
	end do
end subroutine count_atten
subroutine read_data !讀取資料
	use rb
	implicit none
	integer::stat,i
	integer,dimension(18)::ta
	type(student)::ts
	kase=0
	do
		read(10,*,iostat=stat)ts%st_num,ta
		if(stat/=0)exit
		kase=kase+1
	end do
	rewind(10)
	allocate(atten(kase,18))
	allocate(st(kase))
	do i=1,kase
		read(10,*)st(i)%st_num,atten(i,:)
	end do
end subroutine read_data
subroutine get_student !計算誰出席最多和最少，並且順便輸出
	use rb
	implicit none
	integer::i,mx,mn
	mx=1;mn=1;
	write(11,FMT="(A6,A14,A14)")"學號","出席次數","出席率(%)"
	do i=1,kase
		write(11,FMT="(A6,I5,F10.2)")st(i)%st_num,int(st(i)%att_cnt),st(i)%att_rate
	end do
	write(11,FMT="(A)")"出席超過16週(含)之學號:"
	do i=1,kase
		if(st(i)%att_cnt>=16)write(11,*)st(i)%st_num
		if(st(i)%att_cnt>st(mx)%att_cnt)mx=i
	end do
	write(11,FMT="(A)")"出席低於12週(含)之學號:"
	do i=1,kase
		if(st(i)%att_cnt<=12)write(11,*)st(i)%st_num
		if(st(i)%att_cnt<st(mn)%att_cnt)mn=i
	end do
	write(11,FMT="(2A)")"出席率最高之學號:",st(mx)%st_num
	write(11,FMT="(2A)")"出席率最低之學號:",st(mn)%st_num
	write(11,FMT="(A13,I3)")"學生人數:",kase
end subroutine get_student
subroutine get_week  !計算哪周人最多和最少，並且順便輸出
	use rb
	implicit none
	integer::i,mx,mn
	mx=1;mn=1;
	write(11,FMT="(A6,A14,A14)")"周次","出席人數","出席率(%)"
	do i=1,18
		write(11,FMT="(I2,I9,F10.2)")wk(i)%num,int(wk(i)%att_cnt),wk(i)%att_rate
	end do
	write(11,FMT="(A)")"超過九成(含)出席率之周次:"
	do i=1,18
		if(wk(i)%att_rate>=90)write(11,*)wk(i)%num
		if(wk(i)%att_cnt>wk(mx)%att_cnt)mx=i
	end do
	write(11,FMT="(A)")"低於六成(含)出席率之周次:"
	do i=1,18
		if(wk(i)%att_rate<=60)write(11,*)wk(i)%num
		if(wk(i)%att_cnt<wk(mn)%att_cnt)mn=i
	end do
	write(11,FMT="(A,I3)")"出席率最高之周次:",wk(mx)%num
	write(11,FMT="(A,I3)")"出席率最低之周次:",wk(mn)%num
end subroutine get_week
program HW4_0711215
	use rb
	implicit none
	open(10,file="attendance_input.txt",status="old")
	open(11,file="attendance_output.txt",status="replace")
	call read_data
	call count_atten
	call get_student
	call get_week
	close(10)
	close(11)
end program HW4_0711215
