program main
   use aoc_2024
   use string_mod
   implicit none
   character(len=*),parameter::filename="data/10.txt"
   integer,allocatable::a(:,:)
   integer::n,i,idx(2),res,res2
   integer,allocatable::flag(:,:)
   logical,allocatable::mask(:,:)
   n=getrow(filename)
   allocate(a(n,n))
   allocate(flag(n,n))
   allocate(mask(n,n))
   open(10,file=filename)
   do i=1,n
      read(10,form("(<>i1)",[n]))a(i,:)
   end do
   close(10)
   mask=.true.
   res=0
   res2=0
   do
      flag=0
      idx=findloc(a,0,mask=mask)
      if(all(idx==0))exit
      mask(idx(1),idx(2))=.false.
      call dfs(idx)
      res=res+count(flag/=0)
      res2=res2+sum(flag)
   end do
   print*,res
   print*,res2
contains
   recursive subroutine dfs(idx)
      integer,intent(in)::idx(2)
      integer::idy(2),i
      integer,parameter::move(2,4)=reshape([-1,0,0,1,1,0,0,-1],[2,4])
      if(a(idx(1),idx(2))==9)then
         flag(idx(1),idx(2))= flag(idx(1),idx(2))+1
         return
      end if
      do i=1,4
         idy=idx+move(:,i)
         if(.not.all(idy<=n.and.idy>=1))cycle
         if(a(idy(1),idy(2))==a(idx(1),idx(2))+1) call dfs(idy)
      end do
   end subroutine dfs
end program main
