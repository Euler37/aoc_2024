program main
   use aoc_2024
   implicit none
   character(len=100)::str
   integer::n,i
   integer(8),allocatable::a(:)
   integer(8)::res
   integer,parameter::MM=100000
   integer(8)::path(0:MM,75)
   open(10,file="data/11.txt")
   read(10,"(A)")str
   close(10)
   n=getcolnum(str)
   allocate(a(n))
   read(str,*)a
   path=0
   ! part 1
   res=0
   do i=1,n
      res=res+num(a(i),25)
   end do
   print*,res
   ! part 2
   res=0
   do i=1,n
      res=res+num(a(i),75)
   end do
   print*,res
contains
   recursive integer(8) function num(a,i)result(res)
      ! save path search
      integer(8),value::a
      integer,value::i
      integer(8)::tmp,tmp2
      integer::l
      character(len=:),allocatable::nums
      if(i==0)then
         res=1
         return
      end if
      if(a==0)then
         res=num(1_8,i-1)
      else
         nums=tostring(a)
         l=len(nums)
         if(mod(l,2)==0)then
            tmp=tonum(nums(1:l/2))
            if(tmp <= MM .and. i>1)then
               if(path(tmp,i-1)==0) path(tmp,i-1)=num(tmp,i-1)
               res=path(tmp,i-1)
            else
               res=num(tmp,i-1)
            end if
            tmp=tonum(nums(l/2+1:))
            if(tmp <= MM .and. i>1)then
               if(path(tmp,i-1)==0 ) path(tmp,i-1)=num(tmp,i-1)
               tmp2=path(tmp,i-1)
            else
               tmp2=num(tmp,i-1)
            end if
            res=res+tmp2
         else
            if(huge(1_8)/2024<a)then
               print*,a
               error stop "OVERFLOW"
            end if
            res=num(a*2024,i-1)
         end if
      end if
   end function num
end program main
