module bmr
	implicit none
	real,dimension(6)::ai(1:6)=(/1.0,1.2,1.375,1.55,1.725,2.0/) !ai array is the value of ai
	integer,dimension(6)::mdexc,fdexc !mdexc means male index count fdexc too 
	real::h,w,res !res is the result after calculating ,h is height ,w is weight
	character::gender
	integer::stat,i,dex,age,kase,Mcnt,Fcnt,errgender,errindex !kase is serial number 
contains
	subroutine count_bmr()
		kase=0;Mcnt=0;Fcnt=0;errgender=0;errindex=0   !initialize the varieties
		fdexc(1:6)=0;mdexc(1:6)=0
		write(11,FMT="(A,A6,A4,A4,A5,A6,A9)")"no","index","sex","age","wei","hei","bigcal"
		do                                                                                      !infinity loop to read input file
			read(10,FMT="(I2,A3,I5,E7.1,E9.1)",iostat=stat)dex,gender,age,w,h
			if(stat/=0)exit
			kase=kase+1
			write(11,FMT="(I2,I6,A3,I5,F5.1,F6.1)",advance="no")kase,dex,gender,age,w,h
			if(gender/='M'.and.gender/='F')then           !if it read the wrong gender ,errgender add one
				errgender=errgender+1
				write(11,FMT="(A15)")"wrong gender"	
				cycle
			end if
			if(dex>6.or.dex<1)then                        !the same if it read the worng index ,errindex will add one 
				write(11,FMT="(A14)")"wrong index"
				errindex=errindex+1
				cycle
			end if
			if(gender=='M')then                          !tell the gender male or female
				Mcnt=Mcnt+1
				mdexc(dex)=mdexc(dex)+1
				if(age<=30)then
					res=66+13.7*w+5*h-6.8*age           !calculate the bmr
				else if(age<=55)then
					res=63.5+12.7*w+4.2*h-6.9*age
				else
					res=60.5+11.4*w+3.6*h-7.1*age
				end if
			else
				Fcnt=Fcnt+1
				fdexc(dex)=fdexc(dex)+1
				if(age<=30)then
					res=655+9.6*w+1.7*h-4.7*age
				else if(age<=55)then
					res=635+8.6*w+1.5*h-4.8*age
				else
					res=615+7.2*w+1.1*h-4.9*age
				end if
			end if
			res=res*ai(dex)
			write(11,FMT="(I7)")int(res)
		end do
		!outside the loop ,input file had finished
		write(11,FMT="(A,I2)")"iTotal=",kase           !output
		write(11,FMT="(4(A,I2))")"iMale=",Mcnt," iFemale=",Fcnt," Err_gender=",errgender," err_index=",errindex
		write(11,FMT="(A,I2)",advance="no")"Male(index1-6):"
		do i=1,6
			write(11,FMT="(I2)",advance="no")mdexc(i)
		end do
		write(11,*)
		write(11,FMT="(A,I2)",advance="no")"Female(index1-6):"
		do i=1,6
			write(11,FMT="(I2)",advance="no")mdexc(i)
		end do
		write(11,*)
	end subroutine count_bmr
end module bmr
program HW1_0711215
	use bmr
	implicit none
	open(10,file="BMR_input.txt",status="old")
	open(11,file="BMR_output.txt",status="replace")
	call count_bmr()
	close(10)
	close(11)
end program HW1_0711215
