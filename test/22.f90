program main
   use aoc_2024
   use set_mod
   use hash_mod
   use iso_fortran_env
   implicit none
   integer(8)::res,num
   integer::ios,i,j
   integer::n
   integer,allocatable::diff(:,:)
   integer,allocatable::price(:,:)
   type(hashmap)::h
   type(set)::s
   type tuple
      sequence
      integer::idx(4)
   end type tuple
   type(tuple)::key
   type(hash_iter)::iter
   integer::val
   class(*),pointer::ptr

   n=getrow("data/22.txt")
   allocate(diff(2000,n))
   allocate(price(2000,n))
   open(10,file="data/22.txt")
   res=0
   do j=1,n
      read(10,*)num
      do i=1,2000
         diff(i,j)=modulo(num,10_8)
         num=next_num(num)
         diff(i,j)=modulo(num,10_8)-diff(i,j)
         price(i,j)=modulo(num,10_8)
      end do
      res=res+num
   end do
   print*,res
   close(10)
   call s%init(eq,bit)
   call h%init(eq,bit)
   do j=1,n
      do i=1,1996
         key=tuple(diff(i:i+3,j))
         if(key .in. s)cycle
         if(key .in. h)then
            ptr=>h%view(key)
            select type(ptr); type is (integer)
            ptr=ptr+price(i+3,j)
            end select 
         else
            call h%append(key,price(i+3,j))
         end if
         call s%append(key)
      end do
      call s%clean()
   end do
   call iter%init(h)
   res=0
   do while(iter%next(key,val))
      res=max(res,val)
   end do
   print*,res
contains
   integer(8) function next_num(a)result(res)
      integer(8),value::a
      a=modulo(ieor(a,a*64_8),16777216_8)
      a=modulo(ieor(a,a/32_8),16777216_8)
      res=modulo(ieor(a,a*2048_8),16777216_8)
   end function next_num


   subroutine eq(a,b)
      class(*),intent(inout)::a
      class(*),intent(in)::b
      select type(a); type is (integer)
      select type(b); type is (integer)
      a=b
      end select; end select 
      select type(a); type is (integer(8))
      select type(b); type is (integer(8))
      a=b
      end select; end select 

      select type(a); type is (character(len=*))
      select type(b); type is (character(len=*))
      a=b
      end select; end select 
      
      select type(a); class is (tuple)
      select type(b); class is (tuple)
      a%idx=b%idx
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
      class is (tuple)           ; call c_f_pointer(c_loc(a%idx),b,[16])
      end select
   end subroutine bit
end program main
