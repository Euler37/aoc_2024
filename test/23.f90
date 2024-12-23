program main
   use aoc_2024
   use set_mod
   use iso_fortran_env
   implicit none
   character(len=5)::str
   integer::ios
   integer,parameter::n=26*26
   integer::map(0:n-1,0:n-1)
   integer::i,j,k,a(3)
   type tuple
      sequence
      integer::a(3)
   end type tuple
   type(set)::st
   type(set_iter)::iter
   type(tuple)::key
   integer::ta,tz,res
   integer,allocatable::connect(:),tmp(:),best(:)
   open(10,file="data/23.txt")
   do
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      i=str2num(str(1:2))
      j=str2num(str(4:5))
      map(i,j)=1
      map(j,i)=1
   end do
   call st%init(eq,bit)
   do i=0,n-1
      do j=0,n-1
         if(map(i,j)==0)cycle
         do k=0,n-1
            if(map(i,j)+map(j,k)+map(k,i)==3)then
               a=[i,j,k]
               call quicksort(a,0,2)
               call st%append(tuple(a))
            end if
         end do
      end do
   end do
   call iter%init(st)
   ta=str2num("ta")
   tz=str2num("tz")
   res=0
   do while(iter%next(key))
      if(any(key%a>=ta.and.key%a<=tz))res=res+1
   end do
   print*,res
   best=[0]
   do i=0,n-1
      connect=[i,pack([(j,j=0,n-1)],map(:,i)==1)]
      do j=2,size(connect)
         tmp=connect
         tmp=[tmp(j),pack(tmp,map(tmp,tmp(j))==1)]
         k=2
         do
            if(size(tmp)<=k)exit
            tmp=[tmp(k),pack(tmp,map(tmp,tmp(k))==1)]
            k=k+1
         end do
         call quicksort(tmp,0,size(tmp)-1)
         if(size(tmp)>size(best)) best=tmp
      end do
   end do
   call print_char(best)
contains
   logical function check(a)result(res)
      integer::i,j
      integer,intent(in)::a(:)
      res=.true.
      do i=1,size(a)
         do j=i+1,size(a)
            if(map(a(i),a(j))/=1)then
               res=.false.
               return
            end if
         end do
      end do
   end function check

   subroutine print_char(a)
      integer::a(:)
      character(2)::str(0:n-1)
      integer::i,j
      do j=0,25
         do i=0,25
            str(j*26+i)=char(j+97)//char(i+97)
         end do
      end do
      print"(*(g0,:,','))",str(a)
   end subroutine print_char

   integer function str2num(str)result(res)
      character(len=*),intent(in)::str
      associate(idx=>[ichar(str(1:1)),ichar(str(2:2))]-ichar("a"))
         res=hash_bit(idx,[26,26])
      end associate
   end function str2num

   integer function hash_bit(a,b)result(res)
      integer,intent(in)::a(:)
      integer,intent(in)::b(:)
      integer::n,i,idx
      n=size(a)
      idx=1
      res=a(n)
      do i=n,2,-1
         idx=idx*b(i)
         res=res+a(i-1)*idx
      end do
   end function hash_bit

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
      a%a=b%a
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
      class is (tuple)           ; call c_f_pointer(c_loc(a%a),b,[12])
      end select
   end subroutine bit

end program main
