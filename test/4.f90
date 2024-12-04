program main
   use aoc_2024
   implicit none
   integer::n,i,j,res
   character(1),allocatable::s(:,:)
   n=getrow("data/4.txt")
   allocate(s(n,n))
   open(10,file="data/4.txt")
   do i=1,n
      read(10,form("(<>A1)",[n]))s(:,i)
   end do
   ! part 1
   res=0
   do j=1,n
      do i=1,n
         if(s(i,j)=="X")then
            res=res+xmas([i,j])
         end if
      end do
   end do
   print*,res
   ! part 2
   res=0
   do j=1,n
      do i=1,n
         if(s(i,j)=="A")then
            res=res+x_mas([i,j])
         end if
      end do
   end do
   print*,res
contains
   integer function xmas(x)result(res)
      integer,parameter::move(2,8)=reshape([1,0,-1,0,0,1,0,-1, 1,1,-1,-1,-1,1,1,-1],shape=[2,8])
      integer,intent(in)::x(2)
      integer::y(2)
      character(1),parameter::mas(3)=["M","A","S"]
      logical::flag
      integer::i,j
      res=0
      do i=1,8
         y=x
         flag=.true.
         associate(mv=>move(:,i))
            do j=1,3
               y=y+mv
               if(all(y<=n) .and. all(y>0))then
                  if(s(y(1),y(2))/=mas(j))flag=.false.
               else
                  flag=.false.
                  exit
               end if
            end do
            if(flag)res=res+1
         end associate
      end do
   end function xmas

   integer function x_mas(x)result(res)
      integer,parameter::move(2,4)=reshape([1,1,-1,-1,-1,1,1,-1],shape=[2,4])
      integer,intent(in)::x(2)
      integer::y(2,4)
      character(1),parameter::ms(2)=["M","S"]
      logical::flag
      integer::i,j
      character(len=1)::c(4)
      res=0
      do i=1,4
         y(:,i)=x+move(:,i)
      end do
      if(all(y<=n).and.all(y>0))then
         c=[(s(y(1,i),y(2,i)),i=1,4)]
         flag=(all(c(1:2)==ms).or.all(c(2:1:-1)==ms))
         flag=flag.and.(all(c(3:4)==ms).or.all(c(4:3:-1)==ms))
         if(flag)res=1
      end if
   end function x_mas
end program main
