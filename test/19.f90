program main
   use aoc_2024
   use string_mod
   use iso_fortran_env
   implicit none
   character(len=3000)::str
   type(string),allocatable::pattern(:)
   logical::flag
   integer::res,ios,start,end,i
   integer(8)::res2,num
   integer::maxlen
   open(10,file="data/19.txt")
   read(10,"(A)")str
   pattern=split(str,", ",.true.)
   maxlen=maxval(len(pattern))
   read(10,*) ! blank line
   res=0
   res2=0
   do 
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      end=len_trim(str)
      flag=.false.
      call check(1,flag)
      num=dp()
      if(flag)res=res+1
      res2=res2+num
   end do
   close(10)
   print*,res
   print*,res2
contains
   recursive subroutine check(start,flag)
      integer,value::start
      logical,intent(inout)::flag
      integer::i
      if(flag)return
      do i=start,end
         if(str(start:i) .in. pattern)then
            if(i==end)then
               flag=.true.
               return
            else
               call check(i+1,flag)
            end if
         end if
      end do
   end subroutine check

   integer(8) function dp()result(res)
      integer(8)::a(0:1000)
      integer::i,j
      a=0
      do i=1,end
         if(str(1:i) .in. pattern) a(i)=1
         do j=0,maxlen-1
            if(i-j<=1)cycle
            if(str(i-j:i) .in. pattern) a(i)=a(i)+a(i-j-1)
         end do
      end do
      res=a(end)
   end function dp
end program main
