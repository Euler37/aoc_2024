program main
   use aoc_2024
   implicit none
   character(len=*),parameter::filename="data/7.txt"
   integer(8),allocatable::a(:)
   character(len=100)::str
   integer::n,i,m
   integer(8)::num,res,res2
   logical::flag
   n=getrow(filename)
   open(10,file=filename)
   res=0
   res2=0
   do i=1,n
      str=""
      read(10,"(A)")str
      m=getcolnum(str)
      allocate(a(m-1))
      call replace(str,":"," ")
      read(str,*)num,a
      ! part 1
      flag=.false.
      call eval(a(2:),a(1),flag)
      if(flag)res=res+num
      ! part 2
      if(.not. flag)then
         call eval2(a(2:),a(1),flag)
         if(flag)res2=res2+num
      end if
      deallocate(a)
   end do
   print*,res
   print*,res+res2
contains
   recursive subroutine eval(a,res,flag)
      integer(8),intent(inout)::a(:)
      integer(8),value::res
      logical::flag
      if(flag) return
      if(size(a)==0)then
         flag= res==num
         return
      end if
      call eval(a(2:),res+a(1),flag)
      if(flag) return
      call eval(a(2:),res*a(1),flag)
   end subroutine eval

   recursive subroutine eval2(a,res,flag)
      integer(8),intent(inout)::a(:)
      integer(8),value::res
      logical::flag
      if(flag) return
      if(size(a)==0)then
         flag= res==num
         return
      end if
      call eval2(a(2:),res+a(1),flag)
      if(flag) return
      call eval2(a(2:),res*a(1),flag)
      if(flag) return
      call eval2(a(2:),combine(res,a(1)),flag)
   end subroutine eval2

   integer(8) function combine(a,b)result(res)
      integer(8),value::a,b
      res=a*10**int(log10(b*1.d0)+1)+b 
   end function combine
end program main
