module sc
	implicit none
	type student
		character(len=6)::st_num      !學生學號
		real,dimension(5)::score,grade_score    !學生的原始成績和等級制成績
		real,dimension(2)::mean !平均 第一維為原始百分平均，第二維為等級制平均
	end type student
	save
	type(student),allocatable::st(:)	!存儲學生資料的陣列
	integer::kase,per_pass,gra_pass     !kase為流水號同時也是學生人數，per_pass為百分制通過的人數，另外一個為等級制通過的人數
	real::mean,gra_mean,pass_gra_mean,pass_mean
	!mean也是平均的意思，分別為全班百分平均，全班等級平均，全班通過的人的等級平均，全班通過的人的百分平均
	character(len=6),dimension(2,5)::rank !rank為兩種計分方式下的前五名
	integer,dimension(2,6)::segment  !統計位於各區間的人數
end module sc
recursive subroutine quicksort(a,first,last,m)  !此為quicksort m為1會照著百分平均排為2會照著等級平均排序
	use sc
	implicit none
	type(student)::a(*),t
	real*8::x
	integer::first,last,i,j,m
	x=a((first+last)/2)%mean(m)
	i=first;j=last
	do
		do while(a(i)%mean(m)<x)
			i=i+1
		end do
		do while(x<a(j)%mean(m))
			j=j-1
		end do
		if(i>=j)exit
		t=a(i);a(i)=a(j);a(j)=t
		i=i+1
		j=j-1
	end do
	if(first<i-1)call quicksort(a,first,i-1,m)
	if(j+1<last)call quicksort(a,j+1,last,m)
end subroutine quicksort
subroutine read_data !輸入資料
	use sc
	implicit none
	integer::stat,i
	type(student)::temp
	kase=0
	do
		read(10,*,iostat=stat)temp%st_num,temp%score
		if(stat/=0)then
			exit
		end if
		kase=kase+1
	end do
	rewind(10);
	allocate(st(kase))
	do i=1,kase
		read(10,*)st(i)%st_num,st(i)%score
	end do
end subroutine read_data
subroutine trans !將原始分數轉換為等級分數
	use sc
	implicit none
	integer::i,j
	do i=1,kase
		do j=1,5
			if(st(i)%score(j)>=93)then
				st(i)%grade_score(j)=4.3
			else if(st(i)%score(j)>=85)then
				st(i)%grade_score(j)=4.0
			else if(st(i)%score(j)>=80)then
				st(i)%grade_score(j)=3.7
			else if(st(i)%score(j)>=77)then
				st(i)%grade_score(j)=3.3
			else if(st(i)%score(j)>=73)then
				st(i)%grade_score(j)=3.0
			else if(st(i)%score(j)>=70)then
				st(i)%grade_score(j)=2.7
			else if(st(i)%score(j)>=67)then
				st(i)%grade_score(j)=2.3
			else if(st(i)%score(j)>=63)then
				st(i)%grade_score(j)=2.0
			else if(st(i)%score(j)>=60)then
				st(i)%grade_score(j)=1.7
			else if(st(i)%score(j)>=50)then
				st(i)%grade_score(j)=1.0
			else
				st(i)%grade_score(j)=0.0
			end if
		end do
	end do
	return
end subroutine trans
subroutine count_grade !計算每個學生的平均
	use sc
	implicit none
	integer::i,j
	real::summ
	call trans
	do i=1,kase
		summ=0
		do j=1,5
			if(j>=1.and.j<=2)then
				summ=summ+st(i)%score(j)*4
			else if(j>=3.and.j<=4)then
				summ=summ+st(i)%score(j)*2
			else
				summ=summ+st(i)%score(j)
			end if	
		end do
		st(i)%mean(1)=summ/13
		summ=0
		do j=1,5
			if(j>=1.and.j<=2)then
				summ=summ+st(i)%grade_score(j)*4
			else if(j>=3.and.j<=4)then
				summ=summ+st(i)%grade_score(j)*2
			else
				summ=summ+st(i)%grade_score(j)
			end if
		end do
		st(i)%mean(2)=summ/13
	end do
end subroutine count_grade
subroutine get_mean !計算全班的平均
	use sc
	implicit none
	integer::i,j
	real::summ,summ_pass,gra_summ,gra_summ_pass
	summ=0;summ_pass=0;gra_summ=0;gra_summ_pass=0
	per_pass=0;gra_pass=0
	do i=1,kase
		do j=1,5
			if(j>=1.and.j<=2)then
				summ=summ+st(i)%score(j)*4
			else if(j>=3.and.j<=4)then
				summ=summ+st(i)%score(j)*2
			else
				summ=summ+st(i)%score(j)
			end if
		end do
		if(st(i)%mean(1)>=60)then
			per_pass=per_pass+1
			do j=1,5
				if(j>=1.and.j<=2)then
					summ_pass=summ_pass+st(i)%score(j)*4
				else if(j>=3.and.j<=4)then
					summ_pass=summ_pass+st(i)%score(j)*2
				else
					summ_pass=summ_pass+st(i)%score(j)
				end if
			end do
		end if
		do j=1,5
			if(j>=1.and.j<=2)then
				gra_summ=gra_summ+st(i)%grade_score(j)*4
			else if(j>=3.and.j<=4)then
				gra_summ=gra_summ+st(i)%grade_score(j)*2
			else
				gra_summ=gra_summ+st(i)%grade_score(j)
			end if
		end do
		if(st(i)%mean(2)>=1.7)then
			gra_pass=gra_pass+1
			do j=1,5
				if(j>=1.and.j<=2)then
					gra_summ_pass=gra_summ_pass+st(i)%grade_score(j)*4
				else if(j>=3.and.j<=4)then
					gra_summ_pass=gra_summ_pass+st(i)%grade_score(j)*2
				else
					gra_summ_pass=gra_summ_pass+st(i)%grade_score(j)
				end if
			end do
		end if
	end do
	mean=summ/13/kase
	gra_mean=gra_summ/13/kase
	pass_mean=summ_pass/13/per_pass
	pass_gra_mean=gra_summ_pass/13/gra_pass
end subroutine get_mean
subroutine get_rank !先對原始平均排序再對等級平均排序以獲得前五名
	use sc
	implicit none
	integer::i
	type(student),dimension(100)::temp
	temp=st
	call quicksort(temp,1,kase,1)
	do i=kase-4,kase
		rank(1,kase-i+1)=temp(i)%st_num
	end do
	call quicksort(temp,1,kase,2)
	do i=kase-4,kase
		rank(2,kase-i+1)=temp(i)%st_num
	end do
end subroutine get_rank
subroutine get_segment !計算各區間人數
	use sc
	implicit none
	integer::i
	real::x
	do i=1,kase	
		x=st(i)%mean(1)
		if(x>=90)then
			segment(1,1)=segment(1,1)+1
		else if(x>=80)then
			segment(1,2)=segment(1,2)+1
		else if(x>=70)then
			segment(1,3)=segment(1,3)+1
		else if(x>=60)then
			segment(1,4)=segment(1,4)+1
		else if(x>=50)then
			segment(1,5)=segment(1,5)+1
		else
			segment(1,6)=segment(1,6)+1
		end if
		x=st(i)%mean(2)
		if(x>=4.1)then
			segment(2,1)=segment(2,1)+1
		else if(x>=3.7)then
			segment(2,2)=segment(2,2)+1
		else if(x>=2.7)then
			segment(2,3)=segment(2,3)+1
		else if(x>=1.7)then
			segment(2,4)=segment(2,4)+1
		else if(x>=1.0)then
			segment(2,5)=segment(2,5)+1
		else
			segment(2,6)=segment(2,6)+1
		end if
	end do
end subroutine get_segment
subroutine output  !整理完各種資料後一次輸出
	use sc
	implicit none
	integer::i
	write(11,FMT="(A,A9,A6,A9,A9,A11,A17,A17)")&
		"number","Chinese","Math","English","History","Geography","百分制平均","等級制平均"
	do i=1,kase
		write(11,FMT="(A,2I7,I8,2I9,F14.2,F11.2)")st(i)%st_num,int(st(i)%score),st(i)%mean
	end do
	write(11,FMT="(A,I3)")"全班人數:",kase
	write(11,FMT="(A,I3)")"百分制及格人數:",per_pass
	write(11,FMT="(A,I3)")"等級制及格人數:",gra_pass
	write(11,FMT="(A,F5.2)")"百分制全班平均:",mean
	write(11,FMT="(A,F5.2)")"百分制及格平均:",pass_mean
	write(11,FMT="(A,F5.2)")"等級制全班平均:",gra_mean
	write(11,FMT="(A,F5.2)")"等級制及格平均:",pass_gra_mean
	write(11,FMT="(A18,5A8)")"百分制前五名:",rank(1,:)
	write(11,FMT="(A18,5A8)")"等級制前五名:",rank(2,:)
	write(11,*)"百分制平均 各區間人數"
	write(11,*)"100-90   90-80    80-70    70-60    60-50    <50"
	write(11,FMT="(I6,I8,3I9,I7)")segment(1,:)
	write(11,*)"4.3-4.1  4.1-3.7  3.7-2.7  2.7-1.7  1.7-1.0  <1.0"
	write(11,FMT="(I6,I8,3I9,I7)")segment(2,:)
end subroutine output
program HW3_0711215  !有別於之前這次對資料的處理採用離線的作法
	use sc
	implicit none
	open(10,file="ranking_input.txt",status="old")	
	open(11,file="ranking_output.txt",status="replace")
	call read_data
	call count_grade
	call get_mean
	call get_segment
	call get_rank
	call output
	close(10)
	close(11)
end program HW3_0711215
