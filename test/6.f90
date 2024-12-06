program main
   use aoc_2024
   use string_mod
   character(len=*),parameter::filename="data/6.txt"
   character(len=:),allocatable::s
   character(len=1),pointer::ps(:,:)
   integer::n,i,idx(2),idy(2),id0(2),j,k,res
   integer,parameter::move(2,0:3)=reshape([-1,0,0,1,1,0,0,-1],[2,4])
   !^(-1,0) > (0,1) V (1,0) < (0,-1)
   integer,allocatable::flag(:,:)
   integer,allocatable::visit(:,:,:)
   n=getrow(filename)
   allocate(character(len=n*n)::s)
   ps=>string_view2d(s,n,n)
   allocate(flag(n,n),source=0)
   open(10,file=filename)
   do i=1,n
      read(10,form("(<>A1)",[n]))ps(i,:)
   end do
   close(10)
   id0=findloc(ps,"^")
   ! part 1
   j=0
   idx=id0
   flag(idx(1),idx(2))=1
   do
      idy=idx+move(:,j)
      if(.not.all(idy<=n.and.idy>=1))exit
      if(ps(idy(1),idy(2))=="#")then
         j=modulo(j+1,4)
      else
         idx=idy
         flag(idx(1),idx(2))=1
      end if
   end do
   print*,count(flag==1)
   ! part 2
   allocate(visit(n,n,0:3))
   res=0
   do j=1,n
      do i=1,n
         if(ps(i,j)/="#")then
            if(check_loop(i,j))then
               res=res+1
            end if
         end if
      end do
   end do
   print*,res
contains

   logical function check_loop(ix,iy)result(res)
      integer,intent(in)::ix,iy
      integer::k,l
      ps(ix,iy)="#"
      k=0
      visit=0
      idx=id0
      do
         idy=idx+move(:,k)
         if(.not.all(idy<=n.and.idy>=1))exit
         if(ps(idy(1),idy(2))=="#")then
            ! if move to site i more than once with same direction, then in loop 
            if(visit(idx(1),idx(2),k)==1)then
               res=.true.
               ps(ix,iy)="."
               return
            end if
            visit(idx(1),idx(2),k)=1
            k=modulo(k+1,4)
         else
            idx=idy
         end if
      end do
      ps(ix,iy)="."
      res=.false.
   end function check_loop
end program main
