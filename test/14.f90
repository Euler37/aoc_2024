program main
   use aoc_2024
   implicit none
   integer(8)::res(4)
   integer::ios,ptr,i,j,k,n
   logical::flag
   character(len=100)::str
   character(len=*),parameter::filename="data/14.txt"
   integer,allocatable::p(:,:),v(:,:)
   n=getrow(filename)
   allocate(p(2,n),v(2,n))
   open(10,file=filename)
   do i=1,n
      read(10,"(A)")str
      ptr=1
      p(1,i)=strtol(str,ptr)
      p(2,i)=strtol(str,ptr)
      v(1,i)=strtol(str,ptr)
      v(2,i)=strtol(str,ptr)
   end do
   res=0
   do i=1,n
      j=quadrant(p(:,i),v(:,i),100)
      if(j/=0)res(j)=res(j)+1
   end do
   print*,product(res)
   close(10)

   k=0
   do
      call plot(p,v,k,flag)
      if(flag)then
         print*,k
         read(*,*)
      end if
      k=k+1
   end do
contains

   integer function quadrant(p,v,k)result(res)
      integer,parameter::px=101,py=103
      integer,intent(in)::p(2),v(2),k
      integer::pn(2)
      pn=modulo(p+k*v,[px,py])
      res=0
      if(pn(1)<px/2.and.pn(2)<py/2) res=1
      if(pn(1)>px/2.and.pn(2)<py/2) res=2
      if(pn(1)<px/2.and.pn(2)>py/2) res=3
      if(pn(1)>px/2.and.pn(2)>py/2) res=4
   end function quadrant

   subroutine plot(p,v,k,flag)
      use iso_c_binding
      integer,parameter::px=101,py=103
      integer,intent(in)::p(2,n),v(2,n),k
      integer::pn(2)
      logical::flag
      character,target::image(0:px-1,0:py-1)
      character(len=px),pointer::ptr
      image=""
      do i=1,n
         pn=modulo(p(:,i)+k*v(:,i),[px,py])
         image(pn(1),pn(2))="*"
      end do
      flag=.false.
      do i=0,py-1
         call c_f_pointer(c_loc(image(0,i)),ptr)
         if(index(ptr,repeat("*",10))/=0)then
            flag=.true.
            exit
         end if
      end do
      if(flag)then
         do i=0,px-1
            print"(*(a1))",image(i,:)
         end do
      end if
   end subroutine  plot
end program main
