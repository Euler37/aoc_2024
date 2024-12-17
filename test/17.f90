program main
   use aoc_2024
   implicit none
   integer::a,b,c
   character(len=100)::cmd,output
   abstract interface
      subroutine op(n)
         integer,intent(in)::n
      end subroutine op
   end interface
   type fptr
      procedure(op),nopass,pointer::f
   end type fptr
   type(fptr)::p(0:7)
   integer,allocatable::pgm(:,:)
   integer::res,ptr
   integer::n,ps
   p(0)%f=>adv
   p(1)%f=>bxl
   p(2)%f=>bst
   p(3)%f=>jnz
   p(4)%f=>bxc
   p(5)%f=>out
   p(6)%f=>bdv
   p(7)%f=>cdv
   open(10,file="data/17.txt")
   read(10,"(A)")cmd; ps=1; a=strtol(cmd,ps)
   read(10,"(A)")cmd; ps=1; b=strtol(cmd,ps)
   read(10,"(A)")cmd; ps=1; c=strtol(cmd,ps)
   read(10,*)
   read(10,"(A)")cmd
   cmd=cmd(9:)
   ! cmd="0,3,5,4,3,0"
   ! A=117440
   ! B=0
   ! C=0
   n=getcolnum(cmd)
   allocate(pgm(2,0:n/2-1))
   read(cmd,*)pgm
   call part1()
   print*,output
contains

   subroutine part1()
      integer::x,y
      ptr=0
      output=""
      do
         x=pgm(1,ptr)
         y=pgm(2,ptr)
         call p(x)%f(y)
         if(ptr>=n/2)exit
      end do
      output=output(1:len_trim(output)-1)
   end subroutine part1

   subroutine adv(n)
      integer,intent(in)::n
      a=a/2**oper(n)
      ptr=ptr+1
   end subroutine adv

   subroutine bxl(n)
      integer,intent(in)::n
      b=xor(b,oper(n))
      ptr=ptr+1
   end subroutine bxl

   subroutine bst(n)
      integer,intent(in)::n
      b=modulo(oper(n),8)
      ptr=ptr+1
   end subroutine bst

   subroutine jnz(n)
      integer,intent(in)::n
      if(a==0)then
         ptr=ptr+1
         return
      end if
      ptr=oper(n)
   end subroutine jnz
   subroutine bxc(n)
      integer,intent(in)::n
      b=xor(b,c)
      ptr=ptr+1
   end subroutine bxc

   subroutine out(n)
      integer,intent(in)::n
      ! write(*,"(g0,',')",advance="no")modulo(oper(n),8)
      output=trim(output)//tostring(modulo(oper(n),8))//","
      ptr=ptr+1
   end subroutine out

   subroutine bdv(n)
      integer,intent(in)::n
      b=a/2**oper(n)
      ptr=ptr+1
   end subroutine bdv

   subroutine cdv(n)
      integer,intent(in)::n
      c=a/2**oper(n)
      ptr=ptr+1
   end subroutine cdv
   
   integer function oper(n)result(res)
      integer,intent(in)::n
      select case(n)
      case(0:3);res=n
      case(4);res=a
      case(5);res=b
      case(6);res=c
      case default
         res=n
      end select      
   end function oper
end program main
