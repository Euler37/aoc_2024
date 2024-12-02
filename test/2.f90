program main
   use aoc_2024
   implicit none
   integer::row,i,res,col
   character(len=*),parameter::filename="data/2.txt"
   integer,allocatable::a(:)
   character(len=1000)::line
   row=getrow(filename)
   open(10,file=filename)
   ! part 1
   res=0
   do i=1,row
      line=""
      read(10,"(A)")line
      col=getcolnum(line)
      allocate(a(col))
      read(line,*)a
      if(safe(a))res=res+1
      deallocate(a)
   end do
   print*,res
   ! part 2
   rewind(10)
   res=0
   do i=1,row
      line=""
      read(10,"(A)")line
      col=getcolnum(line)
      allocate(a(col))
      read(line,*)a
      if(safe(a))then
         res=res+1
      else
         if(dampener_safe(a))res=res+1
      end if
      deallocate(a)
   end do
   print*,res
   close(10)
contains
   logical function safe(a)result(res)
      integer,intent(in)::a(:)
      associate(p=>a(:size(a)-1)-a(2:))
         res=all(p<=3.and.p>=1).or.all(p>=-3.and.p<=-1)
      end associate
   end function safe

   logical function Dampener_safe(a)result(res)
      integer,intent(in)::a(:)
      integer::num,i
      res= .true.
      do i=1,size(a)
         if(safe([a(1:i-1),a(i+1:)])) return
      end do
      res=.false.
   end function dampener_safe
end program main
