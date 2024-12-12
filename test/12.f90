program main
   use aoc_2024
   use string_mod
   implicit none
   character(len=*),parameter::filename="data/12.txt"
   character,allocatable::a(:,:)
   integer::n,i,idx(2),length,area,j,res,tag,k
   integer,allocatable::flag(:,:)
   integer,allocatable::cnt(:,:,:)
   n=getrow(filename)
   allocate(a(n,n))
   allocate(flag(n,n))
   allocate(cnt(4,n,n))
   open(10,file=filename)
   do i=1,n
      read(10,form("(<>A1)",[n]))a(i,:)
   end do
   close(10)
   flag=0
   res=0
   do i=1,n
      do j=1,n
         if(flag(i,j)==0)then
            length=0
            area=1
            tag=tag+1
            flag(i,j)=tag
            call dfs([i,j],tag,area,length)
            res=res+area*length
            print*,area,length
         end if
      end do
   end do
   print*,res
   res=0
   do k=1,tag
      length=0
      do j=1,n
         do i=1,n
            if(flag(i,j)==k)then
               call side([i,j],tag,length)
            end if
         end do
      end do
      print*,length
   end do
   print*,res
contains
   recursive subroutine dfs(idx,tag,area,length)
      integer,intent(in)::idx(2),tag
      integer,intent(inout)::length,area
      integer::idy(2),i
      integer,parameter::move(2,4)=reshape([-1,0,1,0,0,1,0,-1],[2,4])
      do i=1,4
         idy=idx+move(:,i)
         if(.not.all(idy<=n.and.idy>=1))then ! map boundary
            length=length+1
            cycle
         else
            if(a(idy(1),idy(2))/=a(idx(1),idx(2)))then ! another area boundary
               length=length+1
               cycle
            else
               ! in area
               if(flag(idy(1),idy(2))==tag)cycle
               flag(idy(1),idy(2))=tag
               area=area+1
               call dfs(idy,tag,area,length)
            end if
         end if
      end do
   end subroutine dfs

   recursive subroutine side(idx,tag,length)
      integer,intent(in)::idx(2),tag
      integer,intent(inout)::length
      integer::idy(2),i
      integer,parameter::move(2,4)=reshape([-1,0,1,0,0,1,0,-1],[2,4])
      do i=1,4
         idy=idx+move(:,i)
         if(.not.all(idy<=n.and.idy>=1))then ! map boundary
            print"(*(g0))","[idy']",idx,idy,check_same_side(idx,i,tag,1)
            if(.not.check_same_side(idx,i,tag,1)) length=length+1
            cnt(i,idx(1),idx(2))=1
            cycle
         else
            if(a(idy(1),idy(2))/=a(idx(1),idx(2)))then ! another area boundary
               ! if(tag==2)print*,"[idy']",idy
               print"(*(g0))","[idy']",idx,idy,check_same_side(idx,i,tag,2)
               if(.not.check_same_side(idx,i,tag,2)) length=length+1
               cnt(i,idx(1),idx(2))=1
               cycle
            end if
         end if
      end do
   end subroutine side

   logical function check_same_side(idx,mv,tag,type)result(res)
      integer,intent(in)::idx(2),mv,tag,type
      integer::idy(2),i,j
      integer,parameter::move(2,4)=reshape([-1,0,1,0,0,1,0,-1],[2,4])
      integer,parameter::map(2,4)=reshape([3,4,3,4,1,2,1,2],[2,4])
      res=.false.
      do i=1,2
         idy=idx+move(:,map(i,mv))
         ! find point in same tag
         ! outboundary
         if(.not.all(idy<=n.and.idy>=1))cycle
         if(a(idy(1),idy(2))/=a(idx(1),idx(2)))cycle
         if(cnt(mv,idy(1),idy(2))==0)cycle
         idy=idy+move(:,mv)
         if(type==1)then
            ! overflow
            res=.not.all(idy<=n.and.idy>=1)
            ! print*,res
            if(res)return
         else
            if(.not.all(idy<=n.and.idy>=1))cycle
            res=a(idy(1),idy(2))/=a(idx(1),idx(2))
            ! print*,res
            if(res)return
         end if
      end do
   end function check_same_side
end program main
