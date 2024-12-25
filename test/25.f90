program main
   use aoc_2024
   use iso_fortran_env
   implicit none
   character::a(7,5)
   type psw
      character::a(7,5)
   end type psw
   type(psw),allocatable::key(:)
   type(psw),allocatable::lock(:)
   character::s
   integer::ios,i,j,res
   ! allocate(key(0)))
   ! allocate(lock(0)))
   key=[psw::]
   lock=[psw::]
   open(10,file="data/25.txt")
   do
      do i=1,7
         read(10,"(5A1)")a(i,:)
      end do
      if(all(a(1,:)=="#"))then
         key=[psw::key,psw(a)]
      else
         lock=[psw::lock,psw(a)]
      end if
      read(10,"(A)",iostat=ios)s
      if(is_iostat_end(ios))exit
   end do
   close(10)
   res=0
   do i=1,size(key)
      associate(sk=>count(key(i)%a=="#",dim=1))
         do j=1,size(lock)
            associate(sl=>count(lock(j)%a=="#",dim=1))
               if(all(sk+sl<=7))res=res+1
            end associate
         end do
      end associate
   end do
   print*,res
end program main
