program main
   use aoc_2024
   use string_mod
   implicit none
   character(len=:),allocatable,target::str
   character(len=1),pointer::ps(:)
   integer::start,end,idx,lens,right,s,ido,idon
   integer(8)::res
   call getfile("data/3.txt",str)
   lens=len(str)
   ps=>string_view(str,lens)
   !part 1
   res=0
   start=1
   do
      res=res+mul()
      if(start>lens)exit
   end do
   print*,res
   !part 2
   str="do()"//str
   lens=len(str)
   ps=>string_view(str,lens)
   start=1
   res=0
   do
      s=mul()
      right=min(start,lens)
      ido =index(str(1:right),"do()",back=.true.)
      idon=index(str(1:right),"don't()",back=.true.)
      if(ido > idon)res=res+s
      if(start > lens) exit
   end do
   print*,res
contains
   integer function mul()result(num)
      integer::idx,num1,num2
      logical::flag
      do
         num=0
         idx=index(str(start:),"mul(")+start-1
         if(idx==start-1)then
            ! not find
            start=lens+1
            return
         end if
         start=idx+4
         if(start > lens)return
         ! first num end with ','
         call getnum(',',num1,flag)
         if(.not.flag)cycle
         ! second num end with ')'
         call getnum(')',num2,flag)
         if(.not.flag)cycle
         num=num1*num2
         return
      end do
   end function mul

   subroutine getnum(c,num,flag)
      character(len=*),intent(in)::c
      integer,intent(inout)::num
      logical,intent(inout)::flag
      character(len=:),allocatable::nums
      flag=.false.
      nums=""
      do
         if(start > lens)return
         if(isdigit(ps(start)))then
            nums=nums//ps(start)
            start=start+1
         else if (ps(start)==c)then
            num=tonum(nums)
            start=start+1
            flag=.true.
            return
         else
            ! may be 'm', no ++
            return
         end if
      end do
   end subroutine getnum
end program main
