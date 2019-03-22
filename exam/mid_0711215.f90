module rabbittank
	type ta
		character::cate
		integer::site
		real::total
		integer,dimension(9)::score
		integer::ok
	end type
	type(ta),dimension(100)::arr
	integer::err_site,err_cate,kase
	real,dimension(9)::weiP=(/17.8,8.1,5.9,16.2,7.5,22.7,10.1,6.5,5.2/)
	real,dimension(9)::weiR=(/14.5,6.9,10.3,23.8,12.9,6.7,9.2,8.5,7.2/)
	integer,dimension(6,4)::sta
	real,dimension(6)::sum_scr,cor_q
end module
subroutine get_grade(cate,te,grade,gg)
	use rabbittank
	implicit none
	integer::grade
	real::te
	character::cate
	character(len=2)::gg
	if(cate=='P')then
		if(0<=te.and.te<300)then
			grade=1
			gg="C"
		else if(te<550)then
			grade=2
			gg="B"
		else if(te<750)then
			grade=3
			gg="A"
		else
			grade=4
			gg="A+"
		end if
	else
		if(0<=te.and.te<400)then
			grade=1
			gg="C"
		else if(te<600)then
			grade=2
			gg="B"
		else if(te<800)then
			grade=3
			gg="A"
		else
			grade=4
			gg="A+"
		end if
	end if
end subroutine
subroutine count_sta
	use rabbittank
	implicit none
	integer::i,grade
	type(ta)::te
	character(len=2)::gg
	do i=1,kase
		te=arr(i)
		if(te%ok==0)cycle
		call get_grade(te%cate,te%total,grade,gg)
		if(te%cate=='P')then
			if(te%site==1)then
				sta(1,grade)=sta(1,grade)+1
				cor_q(1)=cor_q(1)+1
				sum_scr(1)=sum_scr(1)+te%total
			else if(te%site==2)then
				sta(2,grade)=sta(2,grade)+1
				cor_q(2)=cor_q(2)+1
				sum_scr(2)=sum_scr(2)+te%total
			else
				sta(3,grade)=sta(3,grade)+1
				cor_q(3)=cor_q(3)+1
				sum_scr(3)=sum_scr(3)+te%total
			end if
		else
			if(te%site==1)then
				sta(4,grade)=sta(4,grade)+1
				cor_q(4)=cor_q(4)+1
				sum_scr(4)=sum_scr(4)+te%total
			else if(te%site==2)then
				sta(5,grade)=sta(5,grade)+1
				cor_q(5)=cor_q(5)+1
				sum_scr(5)=sum_scr(5)+te%total
			else
				sta(6,grade)=sta(6,grade)+1
				cor_q(6)=cor_q(6)+1
				sum_scr(6)=sum_scr(6)+te%total
			end if
		end if
	end do
	write(11,FMT="(A9,A27,A16,A15,A15)")&
		"cate-site","number-of-grade(C,B,A,A+)","number-of-ques","ave-of-score","grade-of-site"
	do i=1,6
		if(i>=1.and.i<=3)then
			write(11,FMT="(A,I1)",advance="no")"P",i
			call get_grade('P',sum_scr(i)/cor_q(i),grade,gg)
		end if
		if(i>=4.and.i<=6)then
			write(11,FMT="(A,I1)",advance="no")"R",i-3
			call get_grade('R',sum_scr(i)/cor_q(i),grade,gg)
		end if
		write(11,FMT="(10x,4I5,I15,F15.2,A10)")sta(i,:),int(cor_q(i)),sum_scr(i)/cor_q(i),gg
	end do
end subroutine
subroutine solve
	use rabbittank
	implicit none
	integer::stat,i
	kase=0
	write(11,FMT="(A2,A6,A6,A30,A13)")&
		"no","cate","site","--scores of questionnaire--","total-score"
	do
		read(10,*,iostat=stat)arr(kase+1)%cate,arr(kase+1)%site,arr(kase+1)%score
		if(stat/=0)exit
		kase=kase+1
		write(11,FMT="(I2,A5,I6,3x,9I3)",advance="no")&
			kase,arr(kase)%cate,arr(kase)%site,arr(kase)%score
		if(arr(kase)%cate/='P'.and.arr(kase)%cate/='R')then
			err_cate=err_cate+1
			write(11,FMT="(A13)")"error_cate"
			arr(kase)%ok=0
			cycle
		end if
		if(arr(kase)%site<1.or.arr(kase)%site>3)then
			err_site=err_site+1
			write(11,FMT="(A13)")"error_site"
			arr(kase)%ok=0
			cycle
		end if
		arr(kase)%ok=1
		arr(kase)%total=0
		if(arr(kase)%cate=='P')then
			do i=1,9
				arr(kase)%total=arr(kase)%total+arr(kase)%score(i)*weiP(i)
			end do
		else
			do i=1,9
				arr(kase)%total=arr(kase)%total+arr(kase)%score(i)*weiR(i)
			end do
		end if
		write(11,FMT="(I6)")int(arr(kase)%total+0.5)
	end do
	write(11,FMT="(A,I3)")"total_quest=",kase
	write(11,FMT="(2(A,I2,1x))")"err_cate=",err_cate,"err_site=",err_site
	call count_sta
end subroutine
program main
	use rabbittank
	implicit none
	open(10,file="Tourist_Attractions_input.txt",status="old")
	open(11,file="Tourist_Attractions_output.txt",status="replace")
	call solve
	close(10)
	close(11)
end program main
