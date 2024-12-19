program main
   use aoc_2024
   use string_mod
   use set_mod
   use iso_fortran_env
   implicit none
   character(len=3000)::str
   type(string),allocatable::pattern(:)
   logical::flag
   integer::res,ios,start,end,i
   integer(8)::res2,num
   integer::maxlen
   type(set)::st
   open(10,file="data/19.txt")
   read(10,"(A)")str
   pattern=split(str,", ",.true.)
   maxlen=maxval(len(pattern))
   call st%init(eq,bit)
   do i=1,size(pattern)
      call st%append(pattern(i))
   end do
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
         ! if(str(start:i) .in. pattern)then
          if(string(str(start:i)) .in. st)then
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
         ! if(str(1:i) .in. pattern) a(i)=1
         if(string(str(1:i)) .in. st)a(i)=1
         do j=0,maxlen-1
            if(i-j<=1)cycle
            ! if(str(i-j:i) .in. pattern) a(i)=a(i)+a(i-j-1)
            if(string(str(i-j:i)) .in. st) a(i)=a(i)+a(i-j-1)
         end do
      end do
      res=a(end)
   end function dp

   subroutine eq(a,b)
      class(*),intent(inout)::a
      class(*),intent(in)::b
      select type(a); type is (integer)
      select type(b); type is (integer)
      a=b
      end select; end select 

      select type(a); type is (real)
      select type(b); type is (real)
      a=b
      end select; end select 

      select type(a); type is (character(len=*))
      select type(b); type is (character(len=*))
      a=b
      end select; end select 
      

      select type(a); class is (string)
      select type(b); class is (string)
      a%str=b%str
      end select; end select 
   end subroutine eq

   subroutine bit(a,b)
      use iso_c_binding
      class(*),intent(in),target::a
      integer(1),intent(inout),pointer::b(:)
      select type(a)
      type is (integer)          ; call c_f_pointer(c_loc(a),b,[4])
      type is (real)             ; call c_f_pointer(c_loc(a),b,[4])
      type is (character(len=*)) ; call c_f_pointer(c_loc(a),b,[len(a)])
      class is (string)          ; call c_f_pointer(c_loc(a%str),b,[len(a%str)])
      end select
   end subroutine bit

end program main
