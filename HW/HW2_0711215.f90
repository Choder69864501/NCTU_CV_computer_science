module EUI
	implicit none
	integer::stat
	real::area
	integer,dimension(6)::ele
	integer,dimension(3,4)::static=reshape((/0,0,0,0,0,0,0,0,0,0,0,0/),(/3,4/))
	integer,dimension(3)::ele_type
contains
	function cnt_bill(typ,e,summer)result(res)
		character(len=2)::typ
		integer::e,summer
		real::res
		res=0
		if(typ=="R")then
			if(e<=120)then
				if(summer==1)then
					res=e*1.63
				else
					res=e*1.63
				end if
			else if(e<=330)then
				if(summer==1)then
					res=(e-120)*2.38+120*1.63
				else
					res=(e-120)*2.10+120*1.63
				end if
			else if(e<=500)then
				if(summer==1)then
					res=(e-330)*3.52+(330-120)*2.38+120*1.63
				else
					res=(e-330)*2.89+(330-120)*2.10+120*1.63
				end if
			else if(e<=700)then
				if(summer==1)then
					res=(e-500)*4.61+(500-330)*3.52+(330-120)*2.38+120*1.63
				else
					res=(e-500)*3.79+(500-330)*2.89+(330-120)*2.10+120*1.63
				end if
			else if(e<=1000)then
				if(summer==1)then
					res=(e-700)*5.42+(700-500)*4.61+(500-330)*3.52+(330-120)*2.38+120*1.63
				else
					res=(e-700)*4.42+(700-500)*3.79+(500-330)*2.89+(330-120)*2.10+120*1.63
				end if
			else
				if(summer==1)then
					res=(e-1000)*6.13+(1000-700)*5.42+(700-500)*4.61+(500-330)*3.52+(330-120)*2.38+120*1.63
				else
					res=(e-1000)*4.83+(1000-700)*4.42+(700-500)*3.79+(500-330)*2.89+(330-120)*2.10+120*1.63
				end if
			end if
		else if(typ=="B1")then
			if(e<=330)then
				if(summer==1)then
					res=2.53*e	
				else
					res=2.12*e
				end if
			else if(e<=700)then
				if(summer==1)then
					res=(e-330)*3.55+330*2.53
				else
					res=(e-330)*2.91+330*2.12
				end if
			else if(e<=1500)then
				if(summer==1)then
					res=(e-700)*4.25+(700-330)*3.55+330*2.53
				else
					res=(e-700)*3.44+(700-330)*2.91+330*2.12
				end if
			else
				if(summer==1)then
					res=(e-1500)*6.15+(1500-700)*4.25+(700-330)*3.55+330*2.53
				else
					res=(e-1500)*4.85+(1500-700)*3.44+(700-330)*2.91+330*2.12
				end if
			end if
		else
			if(e<=330)then
				if(summer==1)then
					res=2.53*e	
				else
					res=2.12*e
				end if
			else if(e<=700)then
				if(summer==1)then
					res=(e-330)*3.55+330*2.53
				else
					res=(e-330)*2.91+330*2.12
				end if
			else
				if(summer==1)then
					res=(e-700)*4.25+(700-330)*3.55+330*2.53
				else
					res=(e-700)*3.44+(700-330)*2.91+330*2.12
				end if
			end if
		end if
		return
	end function cnt_bill
	function get_index(typ,e)result(dex)
		character(len=2)::typ
		real::e
		integer::dex
		if(typ=="R")then
			if(e<15)then
				dex=1
				static(1,1)=static(1,1)+1
			else if(e<20)then
				dex=2
				static(1,2)=static(1,2)+1
			else if(e<25)then
				dex=3
				static(1,3)=static(1,3)+1
			else
				dex=4
				static(1,4)=static(1,4)+1
			end if
		else if(typ=="B1")then
			if(e<40)then
				dex=1
				static(2,1)=static(2,1)+1
			else if(e<50)then
				dex=2
				static(2,2)=static(2,2)+1
			else if(e<60)then
				dex=3
				static(2,3)=static(2,3)+1
			else
				dex=4
				static(2,4)=static(2,4)+1
			end if
		else if(typ=="B2")then
			if(e<40)then
				dex=1
				static(3,1)=static(3,1)+1
			else if(e<50)then
				dex=2
				static(3,2)=static(3,2)+1
			else if(e<60)then
				dex=3
				static(3,3)=static(3,3)+1
			else
				dex=4
				static(3,4)=static(3,4)+1
			end if
		end if
		return
	end function get_index
	subroutine read_data()
		integer::i,kase
		character(len=2)::house
		real::res
		kase=0
		write(11,FMT="(A2,A6,A30,2A11,A6,A7,A10)")&
			"no","cate","----- electricity used -----","year_bill","year_elec","area","BUI","sub-cate"
		do
			res=0
			read(10,*,iostat=stat)house,ele,area
			if(stat/=0)then
				exit
			end if
			if(house=="R")then
				ele_type(1)=ele_type(1)+1
			else if(house=="B1")then
				ele_type(2)=ele_type(2)+1
			else
				ele_type(3)=ele_type(3)+1
			end if
			kase=kase+1
			do i=1,6
				if(i>=4.and.i<=5)then
					res=res+cnt_bill(house,ele(i),1)
				else
					res=res+cnt_bill(house,ele(i),0)
				end if 
			end do
			write(11,FMT="(I2,A6,6I5,I9,I10,F10.2,F7.2,A4,A,I0)")&
				kase,house,ele,int(res+0.5),sum(ele),area,sum(ele)/area,house,"_",get_index(house,sum(ele)/area)
		end do
		write(11,FMT="(A,I3)")"total number=",kase
		write(11,FMT="(3(A,I3,1X))")&
			"number of Resident=",ele_type(1),"number of Business I=",ele_type(2),"number of Business II=",ele_type(3)
		write(11,FMT="(A,4I2)")"Sub of Resident    (category 1~4):",static(1,1:4)
		write(11,FMT="(A,4I2)")"Sub of Business  I (category 1~4):",static(2,1:4)
		write(11,FMT="(A,4I2)")"Sub of Business II (category 1~4):",static(3,1:4)
	end subroutine read_data
end module EUI
program HW2_0711215
	use EUI
	implicit none
	open(10,file="electricity_bill_input.txt",status="old")	
	open(11,file="electricity_bill_output.txt",status="replace")
	call read_data()
	close(10)
	close(11)
end program HW2_0711215
