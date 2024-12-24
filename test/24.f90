program main
   use aoc_2024
   use string_mod
   use list_mod
   use hash_mod
   use iso_fortran_env
   implicit none
   character(len=30),target::str
   integer::ios
   type(hashmap)::map
   type node
      sequence
      character(len=3)::x
      character(len=3)::op
      character(len=3)::y
      character(len=3)::z
   end type node
   type(string),allocatable::rule(:)
   type(list),target::queue
   type(list),pointer::ptr
   type(hash_iter),target::hiter
   type(node),pointer::pt
   type(node)::val
   character(3)::k
   integer::v
   integer(8)::res
   integer::ix,iy

   call map%init(eq,bit)
   call queue%init()
   open(10,file="data/24.txt")
   do
      read(10,"(A)")str
      if(str=="")exit
      call map%append(str(1:3),tonum(str(6:6)))
   end do

   do
      read(10,"(A)",iostat=ios)str
      if(is_iostat_end(ios))exit
      call replace(str,"-"," ")
      call replace(str,">"," ")
      call split(str," ",rule,skip=.true.)
      call queue%append(node(rule(1)%str,rule(2)%str,rule(3)%str,rule(4)%str))
   end do
   do while(.not. queue%empty())
      !---- popleft------!
      ptr=>queue%next
      pt=>ptr%val
      val=pt
      call queue%remove(ptr)
      !---- popleft------!
      if((val%x .in. map) .and.(val%y .in. map))then
         call map%get(val%x,ix)
         call map%get(val%y,iy)
         select case(val%op)
         case("XOR");call map%append(val%z,ieor(ix,iy))
         case("AND");call map%append(val%z,iand(ix,iy))
         case("OR ");call map%append(val%z,ior(ix,iy))
         end select
      else
         call queue%append(val)
      end if
   end do
   call hiter%init(map)
   res=0
   do while(hiter%next(k,v))
      if(k(1:1)=="z")then
         if(v==1)res=ibset(res,tonum(k(2:3)))
      end if
   end do
   print*,res
contains
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
      
      select type(a); class is (node)
      select type(b); class is (node)
      a%x=b%x
      a%y=b%y
      a%z=b%z
      a%op=b%op
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
      class is (node)           ; call c_f_pointer(c_loc(a%x),b,[12])
      end select
   end subroutine bit
end program main
